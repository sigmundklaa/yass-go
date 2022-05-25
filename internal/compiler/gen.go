package compiler

import (
	"fmt"
	"strings"

	"github.com/SigJig/yass-go/internal/compiler/types"
)

var notImplemented error

var (
	funcBodyKinds []AstKind = nil
	modBodyKinds  []AstKind = nil
)

// returns first value (token)
// if more than one exists, panics
func onlyValue(ast *AstNode) (*Token, error) {
	if length := len(ast.Value); length != 1 {
		return nil, fmt.Errorf("expected exactly one value, got %d", length)
	}

	return ast.Value[0], nil
}

func strFromDotted(ast *AstNode) (string, error) {
	if ast.Kind != NAME_FULL {
		return "", fmt.Errorf("expected name_full, got %s", ast.Kind.String())
	}

	var builder strings.Builder

	for _, ch := range ast.Children {
		value, err := onlyValue(ch)

		if err != nil {
			return "", err
		}

		builder.WriteString(string(value.Lexeme))
	}

	return builder.String(), nil
}

func allowedKind(kind AstKind, allowed ...AstKind) bool {
	for _, alw := range allowed {
		if alw == kind {
			return true
		}
	}

	return false
}

type Generator struct {
	scopes  []*Scope // stack of scopes
	stack   []types.Container
	modules map[string][]*types.Module
}

func (gen *Generator) curScope() *Scope {
	if length := len(gen.scopes); length > 0 {
		return gen.scopes[length-1]
	}

	return nil
}

func (gen *Generator) curScopeIs(kinds ...ScopeKind) bool {
	sc := gen.curScope()

	if sc == nil {
		return false
	}

	for _, v := range kinds {
		if sc.kind == v {
			return true
		}
	}

	return false
}

func (gen *Generator) pushScope(kind ScopeKind) *Scope {
	sc := &Scope{kind: kind}
	gen.scopes = append(gen.scopes, sc)

	return sc
}

func (gen *Generator) popScope() *Scope {
	idx := len(gen.scopes) - 1

	if idx < 0 {
		return nil
	}

	sc := gen.scopes[idx]
	gen.scopes = gen.scopes[:idx]

	return sc
}

func (gen *Generator) push(con types.Container) types.Container {
	gen.stack = append(gen.stack, con)

	return con
}

func (gen *Generator) addStmt(stmt types.StmtIface) error {
	if len(gen.stack) < 1 {
		return fmt.Errorf("empty stack")
	}

	gen.stack[len(gen.stack)-1].Add(stmt)

	return nil
}

func (gen *Generator) pop() types.Container {
	idx := len(gen.stack) - 1

	if idx < 0 {
		return nil
	}

	con := gen.stack[idx]
	gen.stack = gen.stack[:idx]

	return con
}

func (gen *Generator) findSym(name string) (types.Symbol, bool) {
	for i := len(gen.scopes); i >= 0; i-- {
		s, ok := gen.scopes[i].get(name)

		if ok {
			return s, ok
		}
	}

	return nil, false
}

func (gen *Generator) makeStmts(ast *AstNode) {

}

func (gen *Generator) addModule(ast *AstNode) error {
	// TODO: Should check file location of module. if defined
	// in another folder than the others, should atleast warn
	if gen.curScope() != nil && !gen.curScopeIs(MODULE_SCOPE) {
		// module inside anything other than module (e.g. function)
		return fmt.Errorf("modules must be defined at the top level")
	}

	value, err := onlyValue(ast)

	if err != nil {
		return err
	}

	name := string(value.Lexeme)

	mod := &types.Module{
		Name: name,
		Body: nil,
	}

	gen.pushScope(MODULE_SCOPE)
	gen.push(mod)

	gen.genMany(ast.Children, modBodyKinds)

	gen.pop()
	gen.popScope()

	csc := gen.curScope()

	if csc != nil {
		csc.set(name, mod)
	}

	gen.modules[name] = append(gen.modules[name], mod)

	return nil
}

func (gen *Generator) addImport(ast *AstNode) error {
	return notImplemented
}

func (gen *Generator) addPartialImport(ast *AstNode) error {
	return notImplemented
}

func (gen *Generator) makeExpr(ast *AstNode) (*types.Expression, error) {
	return nil, notImplemented
}

// ast should be the ast of the function
func (gen *Generator) makeFuncParams(ast *AstNode) ([]*types.Parameter, error) {
	params := []*types.Parameter{}

	// if a parameter with a default value is encountered,
	// all following parameters must have default values
	pastDefaults := false

	for _, v := range ast.Children[1:] {
		if v.Kind != FUNC_PARAM {
			break
		}

		t, err := strFromDotted(v.Children[0])

		if err != nil {
			return nil, err
		}

		tsym, ok := gen.findSym(t)

		if !ok {
			return nil, fmt.Errorf("unrecognized type %s", t)
		}

		if !types.IsMetaType(tsym) {
			return nil, fmt.Errorf("expected type, got something else idek")
		}

		name, err := strFromDotted(v.Children[1])

		if err != nil {
			return nil, err
		}

		var def *types.Expression

		// default value
		if len(v.Children) == 3 {
			def, err = gen.makeExpr(v.Children[2])

			if err != nil {
				return nil, err
			}

			pastDefaults = true
		} else if pastDefaults {
			return nil, fmt.Errorf("past defaults")
		}

		params = append(params, &types.Parameter{
			Type:    tsym,
			Name:    name,
			Default: def,
		})
	}

	return params, nil
}

func (gen *Generator) makeFuncReturns(ast *AstNode) (*types.Pack, error) {
	idx := 1
	for ast.Children[idx].Kind != FUNC_PARAM {
		idx++
	}

	ch := ast.Children[idx]

	if ch.Kind != RETURN_TYPE {
		return nil, nil
	}

	// TODO: return pack of names
	return nil, nil
}

func (gen *Generator) addFunction(ast *AstNode) error {
	name, err := strFromDotted(ast)

	if err != nil {
		return err
	}

	_, exists := gen.findSym(name)

	if exists {
		return fmt.Errorf("symbol %s already exists", name)
	}

	params, err := gen.makeFuncParams(ast)

	if err != nil {
		return err
	}

	returns, err := gen.makeFuncReturns(ast)

	if err != nil {
		return err
	}

	fun := &types.NamedFunction{
		Name:       name,
		Parameters: params,
		Returns:    returns,
		Body:       nil,
	}

	err = gen.curScope().set(name, fun)

	if err != nil {
		return err
	}

	gen.addStmt(fun)

	// Process function body
	gen.pushScope(FUNCTION_SCOPE)
	gen.push(fun)

	csc := gen.curScope()

	// make parameters available inside function scope
	for _, p := range fun.Parameters {
		csc.set(p.Name, p)
	}

	idx := 1 // index 0 is function name
	for ast.Children[idx].Kind != FUNC_PARAM && ast.Children[idx].Kind != RETURN_TYPE {
		idx++
	}

	gen.genMany(ast.Children[idx:], funcBodyKinds)

	gen.pop()
	gen.popScope()

	return nil
}

func (gen *Generator) genOne(ast *AstNode) error {
	switch ast.Kind {
	case MODULE:
		return gen.addModule(ast)
	case IMPORT:
		return gen.addImport(ast)
	case IMPORT_PARTIAL:
		return gen.addPartialImport(ast)
	case FUNCTION:
		return gen.addFunction(ast)
	}

	return nil
}

func (gen *Generator) genMany(asts []*AstNode, allowed []AstKind) error {
	for _, ast := range asts {
		if len(allowed) > 0 && !allowedKind(ast.Kind, allowed...) {
			return fmt.Errorf("ast kind %s not allowed", ast.Kind.String())
		}

		if err := gen.genOne(ast); err != nil {
			return err
		}
	}

	return nil
}

func (gen *Generator) Generate(parser *Parser) {
	nodes := parser.Parse()

	err := gen.genMany(nodes, nil)

	if err != nil {
		panic(err)
	}
}
