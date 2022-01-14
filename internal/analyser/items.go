package analyser

import (
	"fmt"
	"strings"
)

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

var flagChars = []byte{'+', '*', '?'}

type symbol struct {
	content  string
	terminal bool
	flag     prodFlag
}

func (sym symbol) String() string {
	str := sym.content

	if sym.flag&PLUS != 0 {
		return str + "+"
	} else if sym.flag&STAR != 0 {
		return str + "*"
	} else if sym.flag&OPT != 0 {
		return str + "?"
	}

	return str
}

func symsEqual(x []symbol, y []symbol) bool {
	if len(x) != len(y) {
		return false
	}

	for idx, s := range x {
		if s != y[idx] {
			return false
		}
	}

	return true
}

const (
	EPSILON = `\epsilon`
	END     = `\end`
)

var (
	ENDSYMBOL = symbol{END, true, 0}
)

type production struct {
	name    string
	symbols []symbol
}

func (prod *production) lalr(follow stringset) lalrItem {
	return lalrItem{
		dotindex:   0,
		lookaheads: follow,
		production: *prod,
	}
}

// name: symbol1 symbol2? symbol3+
type lalrItem struct {
	dotindex   int
	lookaheads stringset
	production
}

func (i *lalrItem) String() string {
	b := strings.Builder{}
	fmt.Fprintf(&b, "%s:", i.name)
	last := len(i.symbols) - 1

	for idx, sym := range i.symbols {
		if idx == i.dotindex-1 {
			b.WriteRune('.')
		}

		b.WriteString(sym.String())

		if idx != last {
			b.WriteRune(' ')
		}
	}

	return b.String()
}

func (i *lalrItem) cur() (symbol, error) {
	return i.at(i.dotindex)
}

func (i *lalrItem) equal(other lalrItem) bool {
	return i.name == other.name &&
		i.dotindex == other.dotindex &&
		symsEqual(i.symbols, other.symbols)
}

func (i *lalrItem) at(index int) (symbol, error) {
	if index >= len(i.symbols) {
		return symbol{}, fmt.Errorf("index error: attempted to access symbol %d, only %d available", index, len(i.symbols))
	}
	return i.symbols[index], nil
}

func (i *lalrItem) advance() lalrItem {
	// Returns a copy with position advanced by one
	n := *i
	//n.symbols[n.dotindex].content = terminal
	n.dotindex++

	return n
}

func prodString(prod []lalrItem) string {
	b := strings.Builder{}
	last := len(prod) - 1

	for idx, sym := range prod {
		b.WriteString(sym.String())

		if idx != last {
			b.WriteString("  |  ")
		}
	}

	return b.String()
}

func lalrMerge(kernel []lalrItem, add lalrItem) []lalrItem {
	for idx, itm := range kernel {
		if itm.equal(add) {
			kernel[idx].lookaheads.merge(add.lookaheads)
			return kernel
		}
	}

	kernel = append(kernel, add)

	return kernel
}
