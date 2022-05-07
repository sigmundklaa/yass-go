package types

type Callable interface{}

type Parameter struct {
	Type    TypeIface
	Name    string
	Default *Expression
}

type NamedFunction struct {
	Name       string
	Parameters []*Parameter
	Returns    *Pack
	Body       []StmtIface
}

type AnonFunction struct {
	Parameters []*Parameter
	Returns    *Pack
	Body       []StmtIface
}

type FunctionCall struct {
	Function Callable
	Args     []*Expression
}
