# counters

[![Go Reference](https://pkg.go.dev/badge/github.com/intuitivelabs/counters.svg)](https://pkg.go.dev/github.com/intuitivelabs/counters)

Package counters provides easy to use atomic counters.
The counters are defined in groups and each groups can have subgroups
 resulting in a tree like structure.

Each counter has an associated handle (used in all the operations),
 name and description.

Supported operations are increment, decrement, min and max.

Entire groups (complete with their subgroups subtree) can be printed.
