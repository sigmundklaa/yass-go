package parser

type Production uint

const (
	VarDef Production = iota
	Expr
)

var prods = map[Production]string{
	VarDef: "%Identifier %Assign Expr",
	Expr:   "",
}

type NonTerminal struct {
	kind    uint
	literal string
}

type Parser struct {
	stack []uint
	table map[uint]map[uint]uint
}
