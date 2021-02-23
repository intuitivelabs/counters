// Copyright 2019-2020 Intuitive Labs GmbH. All rights reserved.
//
// Use of this source code is governed by a BSD-style license
// that can be found in the LICENSE.txt file in the root of the source
// tree.

// Package counters provides easy to use atomic counters.
// The counters are defined in groups and each groups can have subgroups
// resulting in a tree like structure.
//
// Each counter has an associated handle (used in all the operations),
// name and description.
//
// Supported operations are increment, decrement, min and max.
//
// Entire groups (complete with their subgroups subtree) can be printed.
package counters

import (
	"expvar"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
)

// Handle is the type for the counter handle.
type Handle int

const (
	// Invalid marks an invalid handle (e.g. set on error).
	Invalid Handle = Handle(-1)
)

// Counter flags.
const (
	// Counter is storing Min values.
	CntMinF = 1 << iota
	// Counter is storing Max values.
	CntMaxF
	// The counter value should be hidden (not printed).
	CntHideVal
)

// Val is the type for the counter value.
type Val uint64

// CbkF is the type for the callback function called to
// transform a counter value, when reading it.
//
// The parameters are the counter handle, the value to be transformed
// and an opaque parameter, set when the callback is registered.
//
// It returns the transformed value, which will be returned by the counter
// read or get function.
type CbkF func(h Handle, v Val, p interface{}) Val

// Def is the structure for registering a counter.
type Def struct {
	// Counter handle poiner, filled with the allocated
	// handle value if not nil.
	H *Handle
	// Counter flags, e.g. CntHideVal or CntMaxF.
	Flags int
	// Counter callback function, called on read to transform the
	// value (see type CbkF).
	Cbk CbkF
	// Counter callback function opaque parameter.
	// This parameter will be passed to the callback.
	CbP interface{} // callback param
	// Counter name.
	Name string
	// Counter description (short string describing what the counter will do)
	Desc string
}

type cntvar struct {
	v uint64
}

// String implements the expvar Var interface.
func (cv *cntvar) String() string {
	v := atomic.LoadUint64(&cv.v)
	return strconv.FormatUint(v, 10)
}

type cnt struct {
	v     cntvar
	min   cntvar
	max   cntvar
	flags int
	// Counter callback function, called on read to transform the
	// value (see type CbkF).
	cbk CbkF
	// Counter callback function opaque parameter.
	// This parameter will be passed to the callback.
	cbp  interface{}
	name string
	desc string
}

// Group is the type for the internal counter group representation.
type Group struct {
	// Group name.
	Name     string
	prefix   string
	counters []cnt
	no       int32
	parent   *Group
	cnames   map[string]Handle
	subg     map[string]*Group
	lock     sync.Mutex
}

// Print flags used by the Group.Print function.
const (
	// Full counter name, e.g.: group1.group2.counter.
	PrFullName = 1 << iota
	// Print value (cnt: val).
	PrVal
	// Print counter description  (cnt: [val] [desc]).
	PrDesc
	// Print recursively all subgroups.
	PrRec
)

// RootGrp contains all the groups defined without a parent group (nil).
var RootGrp Group

func init() {
}

// NewGroup creates a new counter group, with a specified name and parent.
//
// The maximum counters number will be n (it cannot be increased afterwards).
// If the parent group is nil, the parent will be set to RootGrp.
//
// If the group already exists in parent, the existing group will be returned.
//
// It returns a pointer to the newly created (and initialised) group.
func NewGroup(Name string, parent *Group, n int) *Group {
	if parent == nil {
		parent = &RootGrp
	}
	g := parent.GetSubGroup(Name)
	if g != nil {
		if len(g.counters) >= n {
			return g
		}
		// error, existing incompatible group
		return nil
	}
	g = &Group{}
	return g.Init(Name, parent, n)
}

// Init initialises a new group.
// It takes 3 parameters: the name of the group, the parent and the maximum
// supported number of counters in this group (cannot be increased afterwards).
func (g *Group) Init(Name string, parent *Group, n int) *Group {
	g.no = 0
	g.Name = Name
	g.counters = make([]cnt, n)
	g.subg = make(map[string]*Group, 8)
	g.cnames = make(map[string]Handle, n)
	if parent == nil && g != &RootGrp {
		parent = &RootGrp
	}
	prefix := ""
	if parent != nil {
		prefix = parent.prefix
	}
	g.prefix = prefix + Name + "."
	g.parent = parent
	if parent != nil {
		parent.AddSubGroup(g)
	}
	return g
}

// AddSubGroup adds a subgroup to the current group.
func (g *Group) AddSubGroup(sg *Group) {
	g.lock.Lock()
	if g.subg == nil {
		g.subg = make(map[string]*Group, 8)
	}
	g.subg[sg.Name] = sg
	g.lock.Unlock()
}

// RegisterDef adds a new counter to the group, based on a counter Def.
// It returns the new counter handle and true on success,
// or garbage and false on error.
// If the counter was previously registered and it has the same def, the
// old counter handle will be returned. If the definitions are different
// it will return error.
func (g *Group) RegisterDef(d *Def) (Handle, bool) {
	h, ok := g.GetCounter(d.Name)
	if ok {
		// already registered
		if !g.EqDef(h, d) {
			// different defs => error
			return h, false
		}
		if d.H != nil {
			*d.H = h
		}
		return h, true
	}

	name := g.prefix + d.Name
	i := int(atomic.AddInt32(&g.no, 1) - 1)
	if i >= len(g.counters) {
		return Handle(-1), false
	}
	g.counters[i].cbk = d.Cbk
	g.counters[i].cbp = d.CbP
	g.counters[i].flags = d.Flags
	g.counters[i].desc = d.Desc
	g.counters[i].name = d.Name
	if d.Flags&CntMinF != 0 {
		// min needs to be set to some high value
		g.Reset(Handle(i), 0)
	}
	expvar.Publish(name, &g.counters[i].v)
	if d.H != nil {
		*d.H = Handle(i)
	}
	g.lock.Lock()
	g.cnames[d.Name] = Handle(i)
	g.lock.Unlock()
	return Handle(i), true
}

// EqDef checks if counter handle was defined with the def d.
func (g *Group) EqDef(h Handle, d *Def) bool {
	return g.counters[h].cbp == d.CbP &&
		g.counters[h].flags == d.Flags &&
		g.counters[h].name == d.Name &&
		g.counters[h].desc == d.Desc
}

// CheckDefs check if the counters in the group were defined with the defs
// definitions.
func (g *Group) CheckDefs(defs []Def) bool {
	if g.CntNo() != len(defs) {
		return false
	}
	for _, d := range defs {
		h, ok := g.GetCounter(d.Name)
		if !ok {
			return false
		}
		if !g.EqDef(h, &d) {
			return false
		}
	}
	return true
}

// FillHandles fills the handles in a defs slice, if a counters with
// the correponding name ( defs[].Name) exits in this group.
// It returns the number of handles filled.
// For counters that not exits in the group, the corresponding H value
// in defs will be set to counters.Invalid.
func (g *Group) FillHandles(defs []Def) int {
	n := 0
	for _, d := range defs {
		h, ok := g.GetCounter(d.Name)
		if !ok {
			if d.H != nil {
				*d.H = Invalid
			}
			continue
		}
		if !g.EqDef(h, &d) {
			continue
		}
		if d.H != nil {
			*d.H = h
		}
		n++
	}
	return n
}

// RegisterDefs adds several counters to the group, based on an array of
// counter Def.
//
// It returns true on success (all counter have been added successfully),
// or false on error (at least one counter failed to be added).
func (g *Group) RegisterDefs(defs []Def) bool {
	for _, d := range defs {
		if _, ok := g.RegisterDef(&d); !ok {
			return false
		}
	}
	return true
}

// Register adds a new standard counter based on name and description.
//
// It returns the new counter handle and true on success, or false on error.
func (g *Group) Register(Name string, Desc string) (Handle, bool) {
	d := Def{Name: Name, Desc: Desc}
	return g.RegisterDef(&d)
}

// CntNo returns the number of counters registered.
func (g *Group) CntNo() int {
	return int(g.no)
}

// Add adds a value to a counter. The counter is specified by its handle.
//
// It returns the new counter value (after the addition).
func (g *Group) Add(h Handle, v Val) Val {
	n := atomic.AddUint64(&g.counters[h].v.v, uint64(v))
	if g.counters[h].flags&(CntMaxF|CntMinF) != 0 {
		if g.counters[h].flags&CntMaxF != 0 {
			atomicUint64Max(&g.counters[h].max.v, n)
		}
		if g.counters[h].flags&CntMinF != 0 {
			atomicUint64Min(&g.counters[h].min.v, n)
		}
	}
	return Val(n)
}

// Sub subtracts a value from a counter.
// The counter is specified by its handle.
//
// It returns the new counter value (after the subtraction).
func (g *Group) Sub(h Handle, v Val) Val {
	return g.Add(h, Val(^uint64(v)+1))
	//	n := atomic.AddUint64(&g.counters[h].v.v, ^uint64(v)+1)
	//	return Val(n)
}

// Set sets a counter value. The counter is specified by its handle.
//
// It returns the new counter value.
func (g *Group) Set(h Handle, v Val) Val {
	atomic.StoreUint64(&g.counters[h].v.v, uint64(v))
	if g.counters[h].flags&(CntMaxF|CntMinF) != 0 {
		if g.counters[h].flags&CntMaxF != 0 {
			atomicUint64Max(&g.counters[h].max.v, uint64(v))
		}
		if g.counters[h].flags&CntMinF != 0 {
			atomicUint64Min(&g.counters[h].min.v, uint64(v))
		}
	}
	return v
}

// Reset sets a counter value. The counter is specified by its handle.
// The difference from Set() is that it re-initializes the counter min and
// max values.
//
// It returns the new counter value.
func (g *Group) Reset(h Handle, v Val) Val {
	atomic.StoreUint64(&g.counters[h].v.v, uint64(v))
	if g.counters[h].flags&(CntMaxF|CntMinF) != 0 {
		if g.counters[h].flags&CntMaxF != 0 {
			g.SetMax(h, v)
		}
		if g.counters[h].flags&CntMinF != 0 {
			g.SetMin(h, Val(^uint64(0)))
		}
	}
	return v
}

// SetMax force-sets directly the counter internal maximum value.
// The counter is specified by its handle.
func (g *Group) SetMax(h Handle, v Val) {
	atomic.StoreUint64(&g.counters[h].max.v, uint64(v))
}

// SetMin force-sets directly the counter internal minimum value.
// The counter is specified by its handle.
func (g *Group) SetMin(h Handle, v Val) {
	atomic.StoreUint64(&g.counters[h].min.v, uint64(v))
}

func atomicUint64Max(dst *uint64, val uint64) uint64 {
	var crt uint64
	for {
		crt = atomic.LoadUint64(dst)
		if val <= crt {
			return crt
		}
		if atomic.CompareAndSwapUint64(dst, crt, val) {
			return val
		}
	}
}

func atomicUint64Min(dst *uint64, val uint64) uint64 {
	var crt uint64
	for {
		crt = atomic.LoadUint64(dst)
		if val >= crt {
			return crt
		}
		if atomic.CompareAndSwapUint64(dst, crt, val) {
			return val
		}
	}
}

// Max sets the counter maximum value if v > current value.
// The counter is specified by its handle.
//
// It returns the new value.
func (g *Group) Max(h Handle, v Val) Val {
	return Val(atomicUint64Max(&g.counters[h].v.v, uint64(v)))
}

// Min sets the counter minimum value if v < current value.
// The counter is specified by its handle.
//
// It returns the new value.
func (g *Group) Min(h Handle, v Val) Val {
	return Val(atomicUint64Min(&g.counters[h].v.v, uint64(v)))
}

// Inc increments the counter current value by 1.
// The counter is specified by its handle.
//
// It returns the new value.
func (g *Group) Inc(h Handle) Val {
	return g.Add(h, 1)
}

// Dec decrements the counter current value by 1.
// The counter is specified by its handle.
//
// It returns the new value.
func (g *Group) Dec(h Handle) Val {
	return g.Sub(h, 1)
}

// Get returns the counter current value.
// If the counter has a defined callback, the callback will be
// called to transform the value.
//
// The counter is specified by its handle.
func (g *Group) Get(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].v.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

// GetMax returns the counter maximum value.
// If the counter has a defined callback, the callback will be
// called to transform the value.
//
// The counter is specified by its handle.
func (g *Group) GetMax(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].max.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

// GetMin returns the counter minimum value.
// If the counter has a defined callback, the callback will be
// called to transform the value.
//
// The counter is specified by its handle.
func (g *Group) GetMin(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].min.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

// GetFlags returns the flags with which a counter was registered.
func (g *Group) GetFlags(h Handle) int {
	return g.counters[h].flags
}

// GetName returns a counter name, given the counter handle.
func (g *Group) GetName(h Handle) string {
	return g.counters[h].name
}

// GetFullName returns a counter full name, composed of the group prefix and
// the counter name.
func (g *Group) GetFullName(h Handle) string {
	return g.prefix + g.counters[h].name
}

// GetDesc returns a counter description.
func (g *Group) GetDesc(h Handle) string {
	return g.counters[h].desc
}

// GetCounter returns a counter handle, given the counter name.
// If the counter is not found, it will return false.
func (g *Group) GetCounter(name string) (Handle, bool) {
	g.lock.Lock()
	h, ok := g.cnames[name]
	g.lock.Unlock()
	if !ok {
		return Invalid, ok
	}
	return h, ok
}

// GetCounterDot returns a counter group and handle, given a dot separated
// subgroup and counter list.
// E.g.: "foo.bar.counter1"
// It returns the counter group, and counter handle on success.
// If not found it will return
// nil, invalid handle, position of the first not found part if no group
// could be found and
// grp, invalid handle , position of not found part if the subgroups could be
// found but the counter does not exists.
// Error check:  g, c, pos := counters.RootGrpGetCounterDot(name)
//               if g == nil || c == counters.Invalid  {
//                      ERROR("in %s after %s\n", name, name[:pos])
//               }
func (g *Group) GetCounterDot(name string) (*Group, Handle, int) {
	s := name
	last := strings.LastIndexByte(s, '.')
	if last < 0 {
		// no dot => the whole name is the counter
		ret, _ := g.GetCounter(name)
		return g, ret, 0
	}
	subg, errpos := g.GetSubGroupDot(s[:last])
	if subg == nil {
		return nil, Invalid, errpos
	}
	ret, _ := subg.GetCounter(name[last+1:])
	return subg, ret, last + 1
}

// GetSubGroup returns a subgroup, based on a subgroup name.
// If no subgroup with the corresponding name exists it will
// return nil.
//
// GetSubGroup does not work recursively, it will search only at
// the current level.
func (g *Group) GetSubGroup(gname string) *Group {
	g.lock.Lock()
	subgr := g.subg[gname]
	g.lock.Unlock()
	return subgr
}

// GetSubGroupDot returns a subgroup, based on a dot separated subgroup names
// string.
// If no subgroup with the corresponding name exists it will
// return nil and the start index in string of the first non-existing subgroup.
//
// E.g.: GetSubGroupDot("foo.bar") will look for the subgroup "foo" and
// if found it will return the subgroup "bar" in  "foo".
func (g *Group) GetSubGroupDot(gnames string) (*Group, int) {
	s := gnames
	start := 0
	pos := strings.IndexByte(s, '.')
	subg := g
	for pos >= 0 {
		subg = subg.GetSubGroup(s[:pos])
		if subg == nil {
			return nil, start
		}
		if (pos + 1) >= len(s) {
			// handle ending in dot, e.g. "foo.bar." => equiv to "foo.bar"
			return subg, start
		}
		s = s[pos+1:]
		start += pos + 1
		pos = strings.IndexByte(s, '.')
	}
	// no more dots
	return subg.GetSubGroup(s), start
}

// GetGroup is obsolete and will be removed in future versions,
// use GetSubGroup instead.
func (g *Group) GetGroup(gname string) *Group {
	return g.GetSubGroup(gname)
}

// GetParent it will return the group parent or nil.
func (g *Group) GetParent() *Group {
	return g.parent
}

// GetSubGroupsNo will return the number of direct subgroups.
func (g *Group) GetSubGroupsNo() int {
	g.lock.Lock()
	n := len(g.subg)
	g.lock.Unlock()
	return n
}

// GetSubGroups will fill a passed groups array with all the
// direct subgroups.
func (g *Group) GetSubGroups(groups *[]*Group) {
	g.lock.Lock()
	for _, v := range g.subg {
		*groups = append(*groups, v)
	}
	g.lock.Unlock()
}

// CopyCnts will copy all the counter from a src group into the current group.
// Note that all the current group subgroups will be removed.
func (g *Group) CopyCnts(src *Group) {
	g.no = int32(copy(g.counters, src.counters[:src.no]))
	g.cnames = src.cnames
	g.subg = nil
}

/*
// AvgGrp fills all the counters in the group with the average values of the
// corresponding counters in src: (g-src) / units.
//
// Note: the groups must have the same counters.
func (g *Group) AvgGrp(src *Group, units int64) {
	if src.no < g.no {
		g.no = src.no
	}
	if units == 0 {
		return
	}
	for i := 0; i < int(g.no); i++ {
		// FIXME: src/units
		g.Sub(Handle(i), Val(int64(src.Get(Handle(i)))/units))
		if g.GetFlags(Handle(i))&CntMaxF != 0 {
			atomicUint64Max(&g.counters[i].max.v, uint64(src.GetMax(Handle(i))))
		}
		if g.GetFlags(Handle(i))&CntMinF != 0 {
			atomicUint64Min(&g.counters[i].max.v, uint64(src.GetMin(Handle(i))))
		}
	}
}
*/

// PrintCounter prints one counter according to the passed flags.
// It will print the contents of ident at the start and the
// counter name will be prefixed by prefix. If prefix is empty and
// PrFullName is set, the full counter name will be printed.
func (g *Group) PrintCounter(w io.Writer, h Handle,
	ident, prefix string, flags int) {
	n := g.GetName(h)
	if len(n) > 0 {
		if flags&PrFullName != 0 && prefix == "" {
			prefix = g.prefix
		}
		fmt.Fprintf(w, "%s%-30s", ident, prefix+n)
		if flags&PrVal != 0 {
			fmt.Fprintf(w, ": ")
			if g.GetFlags(h)&CntHideVal == 0 {
				fmt.Fprintf(w, "%6d", g.Get(h))
			} else {
				fmt.Fprintf(w, "      ")
			}
			if g.GetFlags(h)&CntMinF != 0 {
				m := g.GetMin(h)
				if m == Val(^uint64(0)) {
					m = 0
				}
				fmt.Fprintf(w, " m: %6d", m)
			} else {
				fmt.Fprintf(w, "       ")
			}
			if g.GetFlags(h)&CntMaxF != 0 {
				fmt.Fprintf(w, " M: %6d", g.GetMax(h))
			} else {
				fmt.Fprintf(w, "       ")
			}
		}
		if flags&PrDesc != 0 {
			fmt.Fprintf(w, "		%s", g.GetDesc(h))
		}
		fmt.Fprintln(w)
	}
}

// Print prints all the group counters according to the passed flags
// (PrVal, PrDesc, PrFullName).
func (g *Group) Print(w io.Writer, pre string, flags int) {
	prefix := ""
	if flags&PrFullName != 0 {
		prefix = g.prefix
	}
	for i := 0; i < int(g.no); i++ {
		g.PrintCounter(w, Handle(i), pre, prefix, flags)
	}
}

// PrintSubGroups prints all the subgroups according to the passed flags
// (PrVal, PrDesc, PrFullName, PrRec).
//
// If the PrRec is set in the passed flags it will recursively print all
// the subgroup tree.
func (g *Group) PrintSubGroups(w io.Writer, flags int) {
	n := g.GetSubGroupsNo()
	pre := ""
	if n > 0 {
		subgr := make([]*Group, 0, n)
		g.GetSubGroups(&subgr)
		sort.Stable(subgrps(subgr))
		for _, sg := range subgr {
			if flags&PrFullName == 0 && len(sg.Name) > 0 {
				fmt.Fprintf(w, "%s:\n", sg.Name)
				pre = "    "
			}
			sg.Print(w, pre, flags)
			if flags&PrRec != 0 && sg.GetSubGroupsNo() > 0 {
				fmt.Fprintln(w)
				sg.PrintSubGroups(w, flags)
			}
			fmt.Fprintln(w)
		}
	}
}

// sort Interface for []*Group
type subgrps []*Group

func (ga subgrps) Len() int {
	return len(ga)
}

func (ga subgrps) Less(i, j int) bool {
	return strings.Compare(ga[i].Name, ga[j].Name) < 0
}

func (ga subgrps) Swap(i, j int) {
	ga[i], ga[j] = ga[j], ga[i]
}
