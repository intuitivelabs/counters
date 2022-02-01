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
	// The whole counter should be hidden
	CntHideAllF
	// Hide value if 0
	CntHideZeroF
	// The counter is not monotonic, no rate should be computed
	CntNonMonoF
)

// Val is the type for the counter value.
type Val uint64

// CbkF is the type for the callback function called to
// transform a counter value, when reading it.
//
// The parameters are the group handle, the counter handle, the value to be
// transformed and an opaque parameter, set when the callback is registered.
//
// It returns the transformed value, which will be returned by the counter
// read or get function.
type CbkF func(g *Group, h Handle, v Val, p interface{}) Val

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
	// Print only non zero values
	PrHideZero
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

// MaxCntNo returns the maximum numbers of counters that can be used
// in the group (can be greater then CntNo).
func (g *Group) MaxCntNo() int {
	return int(len(g.counters))
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
		return g.counters[h].cbk(g, h, Val(v), g.counters[h].cbp)
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
		return g.counters[h].cbk(g, h, Val(v), g.counters[h].cbp)
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
		return g.counters[h].cbk(g, h, Val(v), g.counters[h].cbp)
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

// copyCnts will copy all the counter from a src group into the current group.
// Note that all the current group subgroups will be removed.
func (g *Group) copyCnts(src *Group) {
	g.lock.Lock()
	g.no = int32(copy(g.counters, src.counters[:src.no]))
	if len(g.cnames) == 0 {
		g.cnames = make(map[string]Handle, g.MaxCntNo())
	}
	if &g.lock != &src.lock {
		src.lock.Lock()
	}
	for k, v := range src.cnames {
		g.cnames[k] = v
	}
	if &g.lock != &src.lock {
		src.lock.Unlock()
	}
	g.lock.Unlock()
}

// CopyGrp will copy all the counters from the src Group to dst.
// If rec is true it will recursively copy the subgroups too (reusing existing
// subgroups with the same name in dst or creating  new destination subgroups
// if no corresponding one is found or there is not
// enough space for all the counters).
// If rec is false the subgroups will be cleared.
//
// Returns 0 on success, -1 if the master group could not be copied or
// the number of subgroups for each the copy failed.
func CopyGrp(dst, src *Group, rec bool) int {
	if dst.MaxCntNo() < src.CntNo() {
		// failed, not enough pre-alloc counters in dst
		return -1
	}
	dst.copyCnts(src)
	err := 0
	if rec {
		// recursively copy the subgroups
		subgroups := make([]*Group, src.GetSubGroupsNo())
		src.GetSubGroups(&subgroups)
		for _, sg := range subgroups {
			if sg == nil {
				continue
			}
			var c *Group
			if dsg := dst.GetSubGroup(sg.Name); (dsg != nil) &&
				dsg.MaxCntNo() >= sg.CntNo() {
				// existing subgroup, copy inside it
				c = dsg
			} else {
				// subgroup does not exist in dst, or it does not have
				// enough space for all the counters => create new one
				c = &Group{}
				c.Init(sg.Name, dst, sg.MaxCntNo())
				dst.AddSubGroup(c)
			}
			if CopyGrp(c, sg, rec) != 0 {
				err++
			}
		}
	} else {
		dst.lock.Lock()
		dst.subg = nil
		dst.lock.Unlock()
	}
	return err
}

// ResetGrp will set all the counters to v. If rec is true it will
// reset also all the subgroups.
func ResetGrp(dst *Group, v Val, rec bool) {
	for i := Handle(0); i < Handle(dst.no); i++ {
		dst.Reset(i, v)
	}
	if rec {
		// recursively reset the subgroups
		subgroups := make([]*Group, dst.GetSubGroupsNo())
		dst.GetSubGroups(&subgroups)
		for _, sg := range subgroups {
			if sg == nil {
				continue
			}
			ResetGrp(sg, v, rec)
		}
	}
}

// FillRate will fill/overwrite the counters in the dst group with the
// difference of the counters from a & b divided by the provided number of
// units (dst = (a-b)/units).
// If rec is true, the subgroups will be filled too with the corresponding
// rate (non existing or too small subgroups in dst will be created).
// All the groups must have the same number of counters or the function will
// fail.
//
// Returns 0 on success, -1 if the master group could not be copied or
// the number of subgroups for each the operation failed
// (non-matching counters).
func FillRate(dst, a, b *Group, units float64, rec bool) int {
	if (a.CntNo() != b.CntNo()) || (dst.MaxCntNo() < a.CntNo()) {
		return -1
	}
	dst.no = int32(a.CntNo())
	for i := Handle(0); i < Handle(dst.no); i++ {
		if (a.GetFlags(i)|b.GetFlags(i))&(CntHideAllF|CntNonMonoF) != 0 {
			// skip over hidden counters
			continue
		}
		v := uint64(a.Get(i) - b.Get(i))
		if units != 0 {
			v = uint64(float64(v) / units)
		}
		dst.counters[i] = a.counters[i]
		// clear call read callbacks
		// (since we compute the rate using them, we don't need them again
		//  to transform the result)
		dst.counters[i].cbk = nil
		dst.counters[i].cbp = nil
		dst.counters[i].v.v = v
		dst.counters[i].flags |= b.GetFlags(i)
		// max & min : use max(a,b) and min(a,b)
		if dst.GetFlags(i)&CntMaxF != 0 {
			atomicUint64Max(&dst.counters[i].max.v, uint64(b.GetMax(i)))
		}
		if dst.GetFlags(Handle(i))&CntMinF != 0 {
			atomicUint64Min(&dst.counters[i].max.v, uint64(b.GetMin(i)))
		}
	}
	dst.lock.Lock()
	if len(dst.cnames) == 0 {
		dst.cnames = make(map[string]Handle, dst.MaxCntNo())
	}
	if &dst.lock != &a.lock {
		a.lock.Lock()
	}
	//  copy dst.cnames = a.cnames
	for k, v := range a.cnames {
		dst.cnames[k] = v
	}
	if &dst.lock != &a.lock {
		a.lock.Unlock()
	}
	dst.lock.Unlock()
	err := 0
	if rec {
		// recursively copy the subgroups
		subgroups := make([]*Group, a.GetSubGroupsNo())
		a.GetSubGroups(&subgroups)
		for _, s1 := range subgroups {
			if s1 == nil {
				continue
			}
			s2 := b.GetSubGroup(s1.Name)
			if s2 == nil {
				err++
				continue
			}
			if s1.CntNo() != s2.CntNo() {
				// mismatched counters numbers, skip
				err++
				continue
			}
			var c *Group
			if dsg := dst.GetSubGroup(s1.Name); (dsg != nil) &&
				dsg.MaxCntNo() >= s1.CntNo() {
				// existing large enough subgroup, re-use it
				c = dsg
			} else {
				// subgroup does not exist in dst, or it does not have
				// enough space for all the counters => create new one
				c = &Group{}
				c.Init(s1.Name, dst, s1.MaxCntNo())
				dst.AddSubGroup(c)
			}
			if FillRate(c, s1, s2, units, rec) != 0 {
				err++
			}
		}
	} else {
		dst.lock.Lock()
		dst.subg = nil
		dst.lock.Unlock()
	}
	return err
}

// PrintCounter prints one counter according to the passed flags.
// It will print the contents of ident at the start and the
// counter name will be prefixed by prefix. If prefix is empty and
// PrFullName is set, the full counter name will be printed.
func (g *Group) PrintCounter(w io.Writer, h Handle,
	ident, prefix string, flags int) {
	var v Val
	cntF := g.GetFlags(h)
	if cntF&CntHideAllF != 0 {
		return
	}
	// skip if hidden if zero flag set
	if (flags&PrHideZero != 0) || (cntF&CntHideZeroF != 0) {
		v = g.Get(h)
		if v == 0 {
			return
		}
	} else if cntF&CntHideVal == 0 {
		// get counter value only if needed (not hidden)
		v = g.Get(h)
	}

	n := g.GetName(h)
	if len(n) > 0 {
		if flags&PrFullName != 0 && prefix == "" {
			prefix = g.prefix
		}
		fmt.Fprintf(w, "%s%-30s", ident, prefix+n)
		if flags&PrVal != 0 {
			fmt.Fprintf(w, ": ")
			if cntF&CntHideVal == 0 {
				fmt.Fprintf(w, "%6d", v)
			} else {
				fmt.Fprintf(w, "      ")
			}
			if cntF&CntMinF != 0 {
				m := g.GetMin(h)
				if m == Val(^uint64(0)) {
					m = 0
				}
				fmt.Fprintf(w, " m: %6d", m)
			} else {
				fmt.Fprintf(w, "       ")
			}
			if cntF&CntMaxF != 0 {
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
