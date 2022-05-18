package compiler

import (
	"fmt"

	"github.com/SigJig/yass-go/internal/compiler/types"
)

// returns first value (token)
// if more than one exists, panics
func onlyValue(ast *AstNode) *Token {
	if length := len(ast.Value); length != 1 {
		panic(fmt.Errorf("expected exactly one value, got %d", length))
	}

	return ast.Value[0]
}

type Generator struct {
	scopes  []*Scope // stack of scopes
	modules map[string][]*types.Module
}

func (gen *Generator) curScope() *Scope {
	if length := len(gen.scopes); length > 0 {
		return gen.scopes[length-1]
	}

	return nil
}

func (gen *Generator) makeStmts(ast *AstNode) {

}

func (gen *Generator) makeModule(ast *AstNode) error {
	// TODO: Should check file location of module. if defined
	// in another folder than the others, should atleast warn
	if sc := gen.curScope(); sc != nil && sc.kind != MODULE_SCOPE {
		// module inside anything other than module (e.g. function)
		return fmt.Errorf("modules must be defined at the top level")
	}

	name := string(onlyValue(ast).Lexeme)

	mod := &types.Module{
		Name: name,
		Body: []types.StmtIface{},
	}

	gen.modules[name] = append(gen.modules[name], mod)

	return nil
}

func (gen *Generator) makeImport(ast *AstNode) error {
	return nil
}

func (gen *Generator) genOne(ast *AstNode) error {
	switch ast.Kind {
	case MODULE:
		return gen.makeModule(ast)
	case IMPORT:
		return gen.makeModule(ast)
	}

	return nil
}

func (gen *Generator) Generate(parser *Parser) {
	nodes := parser.Parse()

	for _, n := range nodes {
		err := gen.genOne(n)

		if err != nil {
			panic(err)
		}
	}
}
