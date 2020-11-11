package counters_test

import (
	"fmt"
	"os"

	"github.com/intuitivelabs/counters"
)

var stats counters.Group

var cntFoo counters.Handle
var cntMax counters.Handle

func init() {
	cntDefs := [...]counters.Def{
		{&cntFoo, 0, nil, nil, "FooCounter",
			"example counter"},
		{&cntMax, counters.CntMaxF, nil, nil, "MaxCounter",
			"maximum value seen so far"},
	}
	stats.Init("example_group", nil, len(cntDefs))
	if !stats.RegisterDefs(cntDefs[:]) {
		panic("failed to register counters")
	}
}

func Example() {
	stats.Inc(cntFoo)
	stats.Set(cntMax, 1)
	stats.Set(cntMax, 10)
	stats.Sub(cntMax, 1)
	fmt.Println("cntFoo=", stats.Get(cntFoo))
	fmt.Println("cntMax max=", stats.GetMax(cntMax))
	fmt.Println("cntMax crt=", stats.Get(cntMax))
	stats.Print(os.Stdout, "",
		counters.PrFullName|counters.PrVal|counters.PrDesc|counters.PrRec)

	// Output: cntFoo= 1
	// cntMax max= 10
	// cntMax crt= 9
	// example_group.FooCounter      :      1              		example counter
	// example_group.MaxCounter      :      9        M:     10		maximum value seen so far

}
