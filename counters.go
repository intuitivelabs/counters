package counters

import (
	"expvar"
	"fmt"
	"io"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
)

type Handle int

const (
	Invalid Handle = Handle(-1)
)

type Val int64

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

type cnt struct {
	v     expvar.Int
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

func (g *Group) Add(h Handle, v Val) {
	g.counters[h].v.Add(int64(v))
}

func (g *Group) Sub(h Handle, v Val) {
	g.counters[h].v.Add(-int64(v))
}

func (g *Group) Set(h Handle, v Val) {
	g.counters[h].v.Set(int64(v))
}

func (g *Group) Inc(h Handle) {
	g.counters[h].v.Add(1)
}

func (g *Group) Dec(h Handle) {
	g.counters[h].v.Add(-1)
}

func (g *Group) Get(h Handle) Val {
	v := Val(g.counters[h].v.Value())
	if g.counters[h].cbk != nil {
		return g.counters[h].cbk(h, v, g.counters[h].cbp)
	}
	return v
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

func (g *Group) RateGrpCnts(src *Group, units int64) {
	if src.no < g.no {
		g.no = src.no
	}
	if units == 0 {
		return
	}
	for i := 0; i < int(g.no); i++ {
		g.Sub(Handle(i), Val(int64(src.Get(Handle(i)))/units))
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
			fmt.Fprintf(w, "%s%-40s", pre, prefix+n)
			if flags&PrVal != 0 {
				fmt.Fprintf(w, ": %6d", g.Get(Handle(i)))
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
