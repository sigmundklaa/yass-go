package compiler

import "github.com/SigJig/yass-go/internal/compiler/types"

type ScopeKind int

const (
	MODULE_SCOPE ScopeKind = iota
	FUNCTION_SCOPE
)

type Scope struct {
	kind    ScopeKind
	imports []types.Importer
}

func (s *Scope) addImport(mod *types.Module, alias string) {
	if len(alias) == 0 {
		alias = mod.Name
	}

	s.imports = append(s.imports, &types.Import{
		Module: mod,
		Alias:  alias,
	})
}

func (s *Scope) addPartialImport(mod *types.Module, part, alias string) {
	if len(alias) == 0 {
		alias = part
	}

	s.imports = append(s.imports, &types.PartialImport{
		Module: mod,
		Part:   part,
		Alias:  alias,
	})
}
