package compiler

import "github.com/SigJig/yass-go/internal/compiler/types"

type ScopeKind int

const (
	MODULE_SCOPE ScopeKind = iota
	FUNCTION_SCOPE
)

type Scope struct {
	kind    ScopeKind
	symbols map[string]types.Symbol
	imports []types.Importer
}

func (s *Scope) get(key string) (types.Symbol, bool) {
	sym, ok := s.symbols[key]

	return sym, ok
}

func (s *Scope) set(key string, sym types.Symbol) error {
	s.symbols[key] = sym

	return nil
}

func (s *Scope) addImport(mod *types.Module, alias string) error {
	if len(alias) == 0 {
		alias = mod.Name
	}

	s.imports = append(s.imports, &types.Import{
		Module: mod,
		Alias:  alias,
	})

	return nil
}

func (s *Scope) addPartialImport(mod *types.Module, part, alias string) error {
	if len(alias) == 0 {
		alias = part
	}

	s.imports = append(s.imports, &types.PartialImport{
		Module: mod,
		Part:   part,
		Alias:  alias,
	})

	return nil
}
