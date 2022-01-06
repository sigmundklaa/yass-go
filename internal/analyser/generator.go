package analyser

import (
	"fmt"
	"strings"
)

// Pattern for the grammar format
// This pattern are all regex patterns, so when adding an entry make sure it is properly escaped
var Metapattern = map[string]string{
	"name":          `[a-zA-Z_]\w*`,
	"assign":        ":",
	"regex":         `/(?:[^\\/]|[\\](?:[\\]{2})*/|[\\][^/])*/`,
	"string":        `"(?:[^"\\]|[\\](?:[\\]{2})*[^\"])*"`,
	"sqbrac_open":   "\\[",
	"sqbrac_close":  "\\]",
	"paran_open":    "\\(",
	"paran_close":   "\\)",
	"question_mark": "\\?",
	"union":         "\\|",
	"line_cont":     `\\`,
	"newline":       "\n+",
	"ignore":        `(?:[^\S\r\n]+)|(?:/\*(?:[^\*]|\*[^/])*(?:\*/|$))|(?://[^\n]*\n)`,
}

var metaprod = map[string][]string{
	"$start": {
		"$prod*",
	},
	"$prod": {
		"name assign $expr newline",
	},
	"$expr": {
		"paran_open $expr paran_close",
		"$expr union $expr",
		"$expr line_cont newline $expr",
		"$opt_ext",
		"regex",
		"string",
		//"$expr+",
	},
	"$opt_ext": {
		"sqbrac_open $expr sqbrac_close",
		"$expr question_mark",
	},
}

type prodFlag uint

const (
	PLUS prodFlag = 1 << iota // One or more
	STAR                      // Zero or more
	OPT                       // Optional (? suffix)
)

func (pf prodFlag) hasEpsilon() bool {
	return (pf & (STAR | OPT)) > 0
}

func (pf prodFlag) canRepeat() bool {
	return (pf & (PLUS | STAR)) > 0
}

const (
	EPSILON = `\epsilon`
	END     = `\end`
)

var flagChars = []byte{'+', '*', '?'}

type symbol struct {
	content string
	flag    prodFlag
}

// name: symbol1 symbol2? symbol3+
type item struct {
	prodname string
	dotindex int
	symbols  []symbol
}

type state struct {
	kernel []item
	trans  map[string]**state // Double pointer, so we can change the pointer after assignment to another state with an equal kernel
}

type gen struct {
	follow      map[string]stringset
	firstCache  map[string]stringset
	productions map[string][]item
	startpoint  string
	kernelMap   map[string]*state
}

func isTerminal(symname string) bool {
	return symname[0] != '$'
}

func createRule(symname string) symbol {
	var flag prodFlag

	for i := len(symname) - 1; i >= 0; i-- {
		cont := func() bool {
			for idx, r := range flagChars {
				if symname[i] == r {
					flag |= 1 << idx
					return true
				}
			}

			return false
		}()

		if !cont {
			return symbol{symname[:i+1], flag}
		}
	}

	panic(fmt.Errorf("bad rule %s", symname))
}

func createRules(raw string) (rules []symbol) {
	for _, sym := range strings.Split(raw, " ") {
		rules = append(rules, createRule(sym))
	}
	return
}

func newGen(productions map[string][]string, startpoint string) *gen {
	g := &gen{
		make(map[string]stringset),
		make(map[string]stringset),
		make(map[string][]item),
		startpoint,
		make(map[string]*state),
	}

	for symname, union := range productions {
		prods := []item{}

		for _, u := range union {
			prods = append(prods, item{symname, 0, createRules(u)})
		}

		g.productions[symname] = prods
	}

	g.constructAllFollows()

	return g
}

func inStack(content string, stack []string) bool {
	for _, s := range stack {
		if s == content {
			return true
		}
	}

	return false
}

func (g *gen) constructFollows(itm item) {
	for i, length := 0, len(itm.symbols); i < length; i++ {
		currentitm := itm.at(i)
		current := currentitm.content

		if isTerminal(current) {
			continue
		}

		if currentitm.flag.canRepeat() {
			g.follow[current].merge(g.first(current, nil))
		}

		for offset, cont := 1, true; cont; offset++ {
			if i+offset >= length {
				// TODO: This does not work in multilpe cases
				// May need a custom structure that holds a reference to all the maps
				g.follow[current] = g.follow[itm.prodname]
				break
			}

			cont = false

			nxtitm := itm.at(i + offset)
			nxtContent := nxtitm.content

			// TODO: Fix +* operations
			if isTerminal(nxtContent) {
				g.follow[current].add(nxtContent)
				break
			} else if nxtitm.flag.hasEpsilon() {
				g.follow[current] = g.follow[itm.prodname]
				break
			} else {
				deriv := g.first(nxtContent, nil)

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
func (g *gen) first(symname string, stack []string) stringset {
	set := make(stringset)
	addToCache := len(stack) == 0
	cached, ok := g.firstCache[symname]

	if ok {
		return cached
	}

	if !isTerminal(symname) {
		for _, production := range g.productions[symname] {
			// Union: One of all possible productions for "rule"
			for i, cont := 0, true; i < len(production.symbols) && cont; i++ {
				// Normally only one iteration, but if L -> RA, and FIRST(L) -> FIRST(R) where
				// R is a derivative of epsilon we can substitue R and continue to A,
				// resulting in FIRST(L) -> FIRST(A)
				prodAt := production.at(i)

				if content := prodAt.content; content != symname && !inStack(content, stack) {
					stack := append(stack, symname)
					deriv := g.first(content, stack)

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

func (g *gen) closureFromItem(itm item) []item {
	cur := itm.cur()
	content := cur.content

	if isTerminal(content) || content == itm.prodname {
		return nil
	}

	return g.productions[content]
}

func (g *gen) closure(symname string) []item {
	cls := g.productions[symname]

	for _, itm := range cls {
		cls = append(cls, g.closureFromItem(itm)...)
	}

	return cls
}

func (g *gen) constructState(spointer **state) {
	s := *spointer
	key := kernelMapKey(s.kernel)
	fmt.Println(key, "\n")
	existing := g.kernelMap[key]

	if existing != nil {
		*spointer = existing
		return
	}

	for _, itm := range s.kernel {
		cur := itm.cur()

		for fterm := range g.first(cur.content, nil) {
			trs := s.trans[fterm]
			advanced, err := itm.advance(fterm)

			if trs == nil {
				tmp := &state{nil, make(map[string]**state)}
				s.trans[fterm] = &tmp
				trs = &tmp
			}

			if err == nil {
				(*trs).kernel = append((*trs).kernel, advanced)
			}
		}
	}

	g.kernelMap[key] = s

	for _, st := range s.trans {
		g.constructState(st)
	}
}

func (s *state) print(x int) {
	b := strings.Builder{}

	for i := 0; i < x; i++ {
		b.WriteRune('\t')
	}

	for k, v := range s.trans {
		fmt.Printf("%s%s:\n", b.String(), k)

		if kernelMapKey((*v).kernel) == kernelMapKey(s.kernel) {
			fmt.Printf("%s\trepeat\n", b.String())
		} else {
			(*v).print(x + 1)
		}
	}
}

func (g *gen) constructStateSymname(symname string) *state {
	s := &state{g.closure(symname), make(map[string]**state)}
	g.constructState(&s)

	return s
}

func kernelMapKey(items []item) string {
	b := strings.Builder{}

	for _, i := range items {
		b.WriteString(fmt.Sprintf("%s|", i.String()))
	}

	return b.String()
}

func (i *item) String() string {
	b := strings.Builder{}
	b.WriteString(fmt.Sprintf("%s:", i.prodname))

	for idx, sym := range i.symbols {
		if idx == i.dotindex {
			b.WriteRune('.')
		}

		b.WriteString(fmt.Sprintf("%s:%d", sym.content, sym.flag))
	}

	return b.String()
}

func (i *item) cur() symbol {
	return i.at(i.dotindex)
}

func (i *item) at(index int) symbol {
	return i.symbols[index]
}

func (i *item) advance(terminal string) (item, error) {
	// Returns a copy with position advanced by one
	n := *i
	n.symbols[n.dotindex].content = terminal
	n.dotindex++

	if i.dotindex > len(i.symbols) {
		return n, fmt.Errorf("advance reached eof")
	}

	return n, nil
}

func TestFollow() interface{} {
	g := newGen(metaprod, "$start")

	for k, v := range g.follow {
		fmt.Printf("%s => %v\n", k, v)
	}

	return g.first("$opt_ext", nil)
}

func TestClosure() interface{} {
	g := newGen(metaprod, "$start")

	for k, _ := range g.productions {
		fmt.Printf("Closure for %s:\n", k)
		for _, item := range g.closure(k) {
			fmt.Printf("\t%s -> %v\n", item.prodname, item.symbols)
		}
	}

	return 0
}

func TestStates() interface{} {
	g := newGen(metaprod, "$start")
	s := g.constructStateSymname("$expr")

	s.print(1)
	return 0
}
