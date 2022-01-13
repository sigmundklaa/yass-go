package analyser

import (
	"fmt"
	"strings"
)

type gen struct {
	startpoint  string
	follow      map[string]stringset
	firstCache  map[string]stringset
	productions map[string][]lr0Item
	closures    map[string][]lr0Item
	kernelMap   map[string]*state
	baseStates  map[string]*state
	states      []*state
}

func newGen(productions map[string][]string, startpoint string) *gen {
	g := &gen{
		startpoint,
		make(map[string]stringset),
		make(map[string]stringset),
		make(map[string][]lr0Item),
		make(map[string][]lr0Item),
		make(map[string]*state),
		make(map[string]*state),
		nil,
	}

	for symname, union := range productions {
		prods := []lr0Item{}

		for _, u := range union {
			prods = append(prods, lr0Item{symname, 0, createMetaRules(u)})
		}

		g.productions[symname] = prods
	}

	for symname, cls := range g.productions {
		g.closures[symname] = cls
		stack := stringset{}.add(symname)
		work := cls

		for len(work) > 0 {
			itm := work[0]
			work = work[1:]

			cur, err := itm.cur()

			if err != nil {
				continue
			}

			content := cur.content

			if cur.terminal || stack.contains(content) {
				continue
			}

			g.closures[symname] = append(g.closures[symname], g.productions[content]...)
			work = append(work, g.productions[content]...)
			stack.add(content)
		}
	}

	g.constructAllFollows()

	return g
}

func (g *gen) constructFollows(itm lr0Item) {
	for i, length := 0, len(itm.symbols); i < length; i++ {
		currentitm := itm.symbols[i]
		current := currentitm.content

		if currentitm.terminal {
			continue
		}

		if currentitm.flag.canRepeat() {
			g.follow[current].merge(g.first(currentitm, nil))
		}

		for offset, cont := 1, true; cont; offset++ {
			if i+offset >= length {
				// TODO: This does not work in multilpe cases
				// May need a custom structure that holds a reference to all the maps
				g.follow[current] = g.follow[itm.prodname]
				break
			}

			cont = false

			nxtitm := itm.symbols[i+offset]
			nxtContent := nxtitm.content

			// TODO: Fix +* operations
			if nxtitm.terminal {
				g.follow[current].add(nxtContent)
				break
			} else if nxtitm.flag.hasEpsilon() {
				g.follow[current] = g.follow[itm.prodname]
				break
			} else {
				deriv := g.first(nxtitm, nil)

				if deriv.contains(EPSILON) {
					deriv.delete(EPSILON)
					cont = true
				}

				g.follow[current].merge(deriv)
			}
		}
	}
}

func (g *gen) constructAllFollows() {
	for symname := range g.productions {
		g.follow[symname] = make(stringset)
	}

	g.follow[g.startpoint].add(END)

	for _, union := range g.productions {
		for _, production := range union {
			g.constructFollows(production)
		}
	}
}

// Return possible terminals
// Stack prevents infinite recursion
func (g *gen) first(sym symbol, stack stringset) stringset {
	symname := sym.content
	set := make(stringset)

	if stack == nil {
		stack = make(stringset)
	}

	addToCache := len(stack) == 0
	cached, ok := g.firstCache[symname]

	if ok {
		return cached
	}

	if !sym.terminal {
		for _, production := range g.productions[symname] {
			// Union: One of all possible productions for "rule"
			for i, cont := 0, true; i < len(production.symbols) && cont; i++ {
				// Normally only one iteration, but if L -> RA, and FIRST(L) -> FIRST(R) where
				// R is a derivative of epsilon we can substitue R and continue to A,
				// resulting in FIRST(L) -> FIRST(A)
				prodAt := production.symbols[i]

				if content := prodAt.content; content != symname && !stack.contains(content) {
					stack.add(symname)
					deriv := g.first(prodAt, stack)

					// If an epsilon is encountered, we substitute current and keep iterating
					if deriv.contains(EPSILON) {
						deriv.delete(EPSILON)
						cont = true
					} else {
						cont = false
					}

					set.merge(deriv)

					// Because epsilon was found on this non-terminal, we add it
					if prodAt.flag.hasEpsilon() {
						set.add(EPSILON)
					}
				} else {
					break
				}
			}
		}
	} else {
		set.add(symname)
	}

	if addToCache {
		g.firstCache[symname] = set
	}
	return set
}

func (g *gen) processTransitions(st *state, set []lr0Item, extend bool) {
	for _, itm := range set {
		cur, err := itm.cur()

		if err != nil {
			continue
		}

		trstate := st.transition(cur.content).state()
		trstate.kernel = uniqueItemSet(append(trstate.kernel, itm.advance()))

		if extend && !cur.terminal {
			// Process transitions for the closures
			g.processTransitions(st, g.closures[cur.content], false)
		}
	}
}

func (g *gen) constructState(proxy *proxyState) {
	st := proxy.state()
	key := kernelMapKey(st.kernel)
	existing := g.kernelMap[key]

	if existing != nil {
		proxy.replace(existing)
		return
	}

	g.processTransitions(st, st.kernel, true)

	g.kernelMap[key] = st
	g.states = append(g.states, st)

	for _, ps := range st.trans {
		g.constructState(ps)
	}
}

func (g *gen) constructStates() {
	for symname, kernel := range g.productions {
		proxy := newProxy(symname)
		pxstate := proxy.state()
		pxstate.kernel = kernel
		g.baseStates[symname] = pxstate
		g.constructState(proxy)
	}
}

func kernelMapKey(items []lr0Item) string {
	b := strings.Builder{}

	for _, i := range items {
		fmt.Fprintf(&b, "%s|", i.String())
	}

	return b.String()
}
