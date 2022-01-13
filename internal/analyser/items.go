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

// name: symbol1 symbol2? symbol3+
type lr0Item struct {
	prodname string
	dotindex int
	symbols  []symbol
}

func (i *lr0Item) String() string {
	b := strings.Builder{}
	fmt.Fprintf(&b, "%s:", i.prodname)
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

func (i *lr0Item) cur() (symbol, error) {
	return i.at(i.dotindex)
}

func (i *lr0Item) equal(other *lr0Item) bool {
	return i.prodname == other.prodname &&
		i.dotindex == other.dotindex &&
		symsEqual(i.symbols, other.symbols)
}

func (i *lr0Item) in(slc []lr0Item) bool {
	for _, x := range slc {
		if x.equal(i) {
			return true
		}
	}

	return false
}

func (i *lr0Item) at(index int) (symbol, error) {
	if index >= len(i.symbols) {
		return symbol{}, fmt.Errorf("index error: attempted to access symbol %d, only %d available", index, len(i.symbols))
	}
	return i.symbols[index], nil
}

func (i *lr0Item) advance() lr0Item {
	// Returns a copy with position advanced by one
	n := *i
	//n.symbols[n.dotindex].content = terminal
	n.dotindex++

	return n
}

func (i *lr0Item) lalr(follow stringset) lalrItem {
	return lalrItem{follow, *i}
}

func uniqueItemSet(set []lr0Item) []lr0Item {
	newset := []lr0Item{}

	if set == nil {
		return nil
	}

	for _, itm := range set {
		if !itm.in(newset) {
			newset = append(newset, itm)
		}
	}

	return newset
}

func prodString(prod []lr0Item) string {
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

type lalrItem struct {
	lookahead stringset
	lr0Item
}
