package types

type Module struct {
	Name string
	Body []StmtIface
}

type Import struct {
	Module *Module
	Alias  string
}

type PartialImport struct {
	Module *Module
	Part   string
	Alias  string
}
