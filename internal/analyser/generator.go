package analyser

import (
	"fmt"
	"os"
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

var (
	ENDSYMBOL = symbol{END, 0}
)

// name: symbol1 symbol2? symbol3+
type item struct {
	prodname string
	dotindex int
	symbols  []symbol
}

type state struct {
	kernel []item
	trans  map[string]*proxyState // Double pointer, so we can change the pointer after assignment to another state with an equal kernel
}

type proxyState struct {
	key string
	st  *state
}

func newProxy(key string) *proxyState {
	return &proxyState{key, &state{nil, make(map[string]*proxyState)}}
}

func (ps *proxyState) replace(s *state) {
	ps.st = s
}

func (ps *proxyState) state() *state {
	return ps.st
}

func (ps *proxyState) String() string {
	return fmt.Sprintf("<proxyState: %s>", ps.key)
}

type gen struct {
	startpoint  string
	follow      map[string]stringset
	firstCache  map[string]stringset
	productions map[string][]item
	kernelMap   map[string]*state
	closures    map[string][]item
	states      []*state
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
		startpoint,
		make(map[string]stringset),
		make(map[string]stringset),
		make(map[string][]item),
		make(map[string]*state),
		make(map[string][]item),
		nil,
	}

	for symname, union := range productions {
		prods := []item{}

		for _, u := range union {
			prods = append(prods, item{symname, 0, createRules(u)})
		}

		g.productions[symname] = prods
	}

	for symname, cls := range g.productions {
		g.closures[symname] = cls
		stack := []string{symname}
		work := cls

		for len(work) > 0 {
			itm := work[0]
			work = work[1:]

			cur, err := itm.cur()

			if err != nil {
				continue
			}

			content := cur.content

			if isTerminal(content) || inStack(content, stack) {
				continue
			}

			g.closures[symname] = append(g.closures[symname], g.productions[content]...)
			work = append(work, g.productions[content]...)
			stack = append(stack, content)
		}
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
		currentitm := itm.symbols[i]
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

			nxtitm := itm.symbols[i+offset]
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
				prodAt := production.symbols[i]

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

func (st *state) transition(symname string) *proxyState {
	trprox := st.trans[symname]

	if trprox == nil {
		trprox = newProxy(symname)
		st.trans[symname] = trprox
	}

	return trprox
}

func (g *gen) constructState(proxy *proxyState) {
	st := proxy.state()
	key := kernelMapKey(st.kernel)
	existing := g.kernelMap[key]

	if existing != nil {
		proxy.replace(existing)
		return
	}

	closure := st.kernel
	closure = append(closure, g.closures[proxy.key]...)

	for _, itm := range closure {
		cur, err := itm.cur()

		if err != nil {
			continue
		}

		advanced := itm.advance() // this needs to advance tho??!

		trstate := st.transition(cur.content).state()
		trstate.kernel = append(trstate.kernel, advanced)
	}

	g.kernelMap[key] = st
	g.states = append(g.states, st)

	for _, ps := range st.trans {
		g.constructState(ps)
	}
}

func (s *state) print(x int, builder *strings.Builder, stack []string) {
	b := strings.Builder{}

	for i := 0; i < x; i++ {
		builder.WriteRune(' ')
	}

	stack = append(stack, kernelMapKey(s.kernel))
	for k, v := range s.trans {
		builder.WriteString(fmt.Sprintf("%s%s:\n", b.String(), k))

		if inStack(kernelMapKey(v.state().kernel), stack) {
			builder.WriteString(fmt.Sprintf("%s\trepeat\n", b.String()))
		} else {
			v.state().print(x+1, builder, stack)
		}
	}
}

func (g *gen) constructStates() {
	for symname, kernel := range g.productions {
		proxy := newProxy(symname)
		proxy.state().kernel = kernel
		g.constructState(proxy)
	}
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

func (i *item) cur() (symbol, error) {
	return i.at(i.dotindex)
}

func (i *item) at(index int) (symbol, error) {
	if index >= len(i.symbols) {
		return symbol{}, fmt.Errorf("index error: attempted to access symbol %d, only %d available", index, len(i.symbols))
	}
	return i.symbols[index], nil
}

func (i *item) advance() item {
	// Returns a copy with position advanced by one
	n := *i
	//n.symbols[n.dotindex].content = terminal
	n.dotindex++

	return n
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

	for k := range g.productions {
		fmt.Printf("Closure for %s:\n", k)
		for _, item := range g.closures[k] {
			fmt.Printf("\t%s -> %v\n", item.prodname, item.symbols)
		}
	}

	return 0
}

func TestStates() interface{} {
	g := newGen(metaprod, "$start")
	g.constructStates()

	f, err := os.OpenFile("dump.githide.txt", os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0755)
	if err != nil {
		panic(err)
	}

	defer f.Close()

	builder := &strings.Builder{}

	for _, s := range g.states {
		s.print(1, builder, nil)
	}
	fmt.Printf("%d states printed\n", len(g.states))

	f.WriteString(builder.String())

	return 0
}
