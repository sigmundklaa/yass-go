package analyser

import (
	"fmt"
	"os"
)

func TestFollow() interface{} {
	g := newGen(metaprod, "$start")

	for k, v := range g.follow {
		fmt.Printf("%s => %v\n", k, v)
	}

	return g.first(symbol{"$opt_ext", false, 0}, nil)
}

func TestClosure() interface{} {
	g := newGen(metaprod, "$start")

	for k := range g.productions {
		fmt.Printf("Closure for %s:\n", k)
		for _, item := range g.closures[k] {
			fmt.Printf("\t%s -> %v\n", item.prodname, item.symbols)
		}
	}

	return 0
}

func (g *gen) findState(st *state) int {
	for idx, x := range g.states {
		if x == st {
			return idx
		}
	}

	return -1
}

func TestStates() interface{} {
	g := newGen(metaprod, "$start")
	g.constructStates()

	f, err := os.OpenFile("dump.githide.txt", os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0755)
	if err != nil {
		panic(err)
	}

	defer f.Close()

	for idx, s := range g.states {
		fmt.Fprintln(f, idx)

		for _, prox := range s.trans {
			fmt.Fprintf(f, "\t%s (%v) => State %d\n", prox.key, prodString(prox.state().kernel), g.findState(prox.state()))
		}

		for s := range s.complete {
			fmt.Fprintf(f, "\t%s => END\n", s)
		}
	}
	fmt.Printf("%d states printed\n", len(g.states))

	return 0
}
