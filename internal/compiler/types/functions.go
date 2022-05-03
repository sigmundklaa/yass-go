package types

type Parameter struct {
	Type    TypeIface
	Name    string
	Default *Expression
}

type NamedFunction struct {
	Name       string
	Parameters []*Parameter
	Returns    []TypeIface
	Body       []StmtIface
}

type AnonFunction struct {
	Parameters []*Parameter
	Returns    []TypeIface
	Body       []StmtIface
}

type FunctionCall struct {
	Function interface{}
	Args     []*Expression
}
