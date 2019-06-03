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

type Handle int

const (
	Invalid Handle = Handle(-1)
)

// counter flags
const (
	CntMinF = 1 << iota
	CntMaxF
	CntHideVal
)

type Val uint64

// callback function for transforming a value, when reading
type CbkF func(h Handle, v Val, p interface{}) Val

// Def is for registering a counter
type Def struct {
	H     *Handle // filled if not nil
	Flags int
	Cbk   CbkF
	CbP   interface{} // callback param
	Name  string
	Desc  string
}

type cntvar struct {
	v uint64
}

// String() implements the expvar Var interface
func (cv *cntvar) String() string {
	v := atomic.LoadUint64(&cv.v)
	return strconv.FormatUint(v, 10)
}

type cnt struct {
	v     cntvar
	min   cntvar
	max   cntvar
	flags int
	cbk   CbkF
	cbp   interface{}
	name  string
	desc  string
}

type Group struct {
	Name     string
	prefix   string
	counters []cnt
	no       int32
	parent   *Group
	cnames   map[string]Handle
	subg     map[string]*Group
	lock     sync.Mutex
}

// Print flags
const (
	PrFullName = 1 << iota // full counter name, e.g.: group1.group2.counter
	PrVal                  // print value (cnt: val)
	PrDesc                 // print desc (cnt: [val] [desc])
	PrRec                  // recursive, all subgroups
)

var RootGrp Group

func init() {
}

// NewGroup creates a new counter group, with a specified name and parent.
// The maximum counters number will be n (it cannot be increased afterwards)
func NewGroup(Name string, parent *Group, n int) *Group {
	g := &Group{}
	return g.Init(Name, parent, n)
}

func (g *Group) Init(Name string, parent *Group, n int) *Group {
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

func (g *Group) AddSubGroup(sg *Group) {
	g.lock.Lock()
	if g.subg == nil {
		g.subg = make(map[string]*Group, 8)
	}
	g.subg[sg.Name] = sg
	g.lock.Unlock()
}

// RegisterDef returns new handle and true on success, or garbage and false
// on error
func (g *Group) RegisterDef(d *Def) (Handle, bool) {
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

func (g *Group) RegisterDefs(defs []Def) bool {
	for _, d := range defs {
		if _, ok := g.RegisterDef(&d); !ok {
			return false
		}
	}
	return true
}

func (g *Group) Register(Name string, Desc string) (Handle, bool) {
	d := Def{Name: Name, Desc: Desc}
	return g.RegisterDef(&d)
}

func (g *Group) CntNo() int {
	return int(g.no)
}

//Add() returns the new counter value.
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

func (g *Group) Sub(h Handle, v Val) Val {
	return g.Add(h, Val(^uint64(v)+1))
	//	n := atomic.AddUint64(&g.counters[h].v.v, ^uint64(v)+1)
	//	return Val(n)
}

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

// Reset() is similar to Set, but it re-initializes Min & Max
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

func (g *Group) SetMax(h Handle, v Val) {
	atomic.StoreUint64(&g.counters[h].max.v, uint64(v))
}

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

// Max sets handle to v is v > crt val and returns the new maximum
func (g *Group) Max(h Handle, v Val) Val {
	return Val(atomicUint64Max(&g.counters[h].v.v, uint64(v)))
}

// Min sets handle to v is v < crt val and returns the new minimum
func (g *Group) Min(h Handle, v Val) Val {
	return Val(atomicUint64Min(&g.counters[h].v.v, uint64(v)))
}

func (g *Group) Inc(h Handle) Val {
	return g.Add(h, 1)
}

func (g *Group) Dec(h Handle) Val {
	return g.Sub(h, 1)
}

func (g *Group) Get(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].v.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

func (g *Group) GetMax(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].max.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

func (g *Group) GetMin(h Handle) Val {
	v := atomic.LoadUint64(&g.counters[h].max.v)
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, Val(v), g.counters[h].cbp)
	}
	return Val(v)
}

func (g *Group) GetFlags(h Handle) int {
	return g.counters[h].flags
}

func (g *Group) GetName(h Handle) string {
	return g.counters[h].name
}

func (g *Group) GetFullName(h Handle) string {
	return g.prefix + g.counters[h].name
}

func (g *Group) GetDesc(h Handle) string {
	return g.counters[h].desc
}

func (g *Group) GetCounter(name string) (Handle, bool) {
	g.lock.Lock()
	h, ok := g.cnames[name]
	g.lock.Unlock()
	return h, ok
}

func (g *Group) GetGroup(gname string) *Group {
	g.lock.Lock()
	subgr := g.subg[gname]
	g.lock.Unlock()
	return subgr
}

func (g *Group) GetParent() *Group {
	return g.parent
}

func (g *Group) GetSubGroupsNo() int {
	g.lock.Lock()
	n := len(g.subg)
	g.lock.Unlock()
	return n
}

func (g *Group) GetSubGroups(groups *[]*Group) {
	g.lock.Lock()
	for _, v := range g.subg {
		*groups = append(*groups, v)
	}
	g.lock.Unlock()
}

func (g *Group) CopyCnts(src *Group) {
	g.no = int32(copy(g.counters, src.counters[:src.no]))
	g.cnames = src.cnames
	g.subg = nil
}

// AvgGrp fills group with the average value : (g-src) / units
func (g *Group) AvgGrp(src *Group, units int64) {
	if src.no < g.no {
		g.no = src.no
	}
	if units == 0 {
		return
	}
	for i := 0; i < int(g.no); i++ {
		g.Sub(Handle(i), Val(int64(src.Get(Handle(i)))/units))
		if g.GetFlags(Handle(i))&CntMaxF != 0 {
			atomicUint64Max(&g.counters[i].max.v, uint64(src.GetMax(Handle(i))))
		}
		if g.GetFlags(Handle(i))&CntMinF != 0 {
			atomicUint64Min(&g.counters[i].max.v, uint64(src.GetMin(Handle(i))))
		}
	}
}

func (g *Group) Print(w io.Writer, pre string, flags int) {
	prefix := ""
	if flags&PrFullName != 0 {
		prefix = g.prefix
	}
	for i := 0; i < int(g.no); i++ {
		n := g.GetName(Handle(i))
		if len(n) > 0 {
			fmt.Fprintf(w, "%s%-30s", pre, prefix+n)
			if flags&PrVal != 0 {
				fmt.Fprintf(w, ": ")
				if g.GetFlags(Handle(i))&CntHideVal == 0 {
					fmt.Fprintf(w, "%6d", g.Get(Handle(i)))
				} else {
					fmt.Fprintf(w, "      ")
				}
				if g.GetFlags(Handle(i))&CntMinF != 0 {
					m := g.GetMin(Handle(i))
					if m == Val(^uint64(0)) {
						m = 0
					}
					fmt.Fprintf(w, " m: %6d", m)
				} else {
					fmt.Fprintf(w, "       ")
				}
				if g.GetFlags(Handle(i))&CntMaxF != 0 {
					fmt.Fprintf(w, " M: %6d", g.GetMax(Handle(i)))
				} else {
					fmt.Fprintf(w, "       ")
				}
			}
			if flags&PrDesc != 0 {
				fmt.Fprintf(w, "		%s", g.GetDesc(Handle(i)))
			}
			fmt.Fprintln(w)
		}
	}
}

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
