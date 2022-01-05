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
		"$expr",
	},
	"$opt_ext": {
		"sqbrac_open $expr sqbrac_open",
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
	return pf&STAR > 0 || pf&OPT != 0
}

const (
	EPSILON = `\epsilon`
	END     = `\end`
)

var flagChars = []byte{'+', '*', '?'}

type prodtoken struct {
	content string
	flag    prodFlag
}

// name: prodtoken1 prodtoken2? prodtoken3+
type prod struct {
	name     string
	dotindex int
	r        []prodtoken
	g        *gen
}

type gen struct {
	follow      map[string]stringset
	firstCache  map[string]stringset
	productions map[string][]prod
	startpoint  string
}

func isTerminal(sym string) bool {
	return sym[0] != '$'
}

func createRule(sym string) prodtoken {
	var flag prodFlag

	for i := len(sym) - 1; i >= 0; i-- {
		cont := func() bool {
			for idx, r := range flagChars {
				if sym[i] == r {
					flag |= 1 << idx
					return true
				}
			}

			return false
		}()

		if !cont {
			return prodtoken{sym[:i+1], flag}
		}
	}

	panic(fmt.Errorf("bad rule %s", sym))
}

func createRules(raw string) (rules []prodtoken) {
	for _, sym := range strings.Split(raw, " ") {
		rules = append(rules, createRule(sym))
	}
	return
}

func newGen(productions map[string][]string, startpoint string) *gen {
	g := &gen{
		make(map[string]stringset),
		make(map[string]stringset),
		make(map[string][]prod),
		startpoint,
	}

	for name, union := range productions {
		prods := []prod{}

		for _, u := range union {
			prods = append(prods, prod{name, 0, createRules(u), g})
		}

		g.productions[name] = prods
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

func (g *gen) constructFollows(production prod) {
	for i, length := 0, len(production.r); i < length; i++ {
		current := production.at(i).content

		if isTerminal(current) {
			continue
		}

		for offset, cont := 1, true; cont; offset++ {
			if i+offset >= length {
				// TODO: This does not work in multilpe cases
				// May need a custom structure that holds a reference to all the maps
				g.follow[current] = g.follow[production.name]
				break
			}

			cont = false

			nxt := production.at(i + offset)
			nxtContent := nxt.content

			if isTerminal(nxtContent) {
				g.follow[current].add(nxtContent)
				break
			} else if nxt.flag.hasEpsilon() {
				g.follow[current] = g.follow[production.name]
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
	for k := range g.productions {
		g.follow[k] = make(stringset)
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
func (g *gen) first(rule string, stack []string) stringset {
	set := make(stringset)
	addToCache := len(stack) == 0
	cached, ok := g.firstCache[rule]

	if ok {
		return cached
	}

	if !isTerminal(rule) {
		for _, production := range g.productions[rule] {
			// Union: One of all possible productions for "rule"
			for i, cont := 0, true; i < len(production.r) && cont; i++ {
				// Normally only one iteration, but if L -> RA, and FIRST(L) -> FIRST(R) where
				// R is a derivative of epsilon we can substitue R and continue to A,
				// resulting in FIRST(L) -> FIRST(A)
				prodAt := production.at(i)

				if content := prodAt.content; content != rule && !inStack(content, stack) {
					stack := append(stack, rule)
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
		set.add(rule)
	}

	if addToCache {
		g.firstCache[rule] = set
	}
	return set
}

func (p *prod) cur() prodtoken {
	return p.at(p.dotindex)
}

func (p *prod) at(i int) prodtoken {
	return p.r[i]
}

func Test() interface{} {
	g := newGen(metaprod, "$start")

	for k, v := range g.follow {
		fmt.Printf("%s => %v\n", k, v)
	}

	return g.first("$opt_ext", nil)
}
