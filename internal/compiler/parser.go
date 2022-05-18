package compiler

import (
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/SigJig/yass-go/internal/errhandler"
)

var expectFailed error

type AstKind int

const (
	MODULE AstKind = iota
	IMPORT
	IMPORT_PARTIAL
	CLASS
	FUNCTION
	FUNC_PARAM
	RETURN_TYPE
	INLINE_SQF
	VAR_DEC
	UNPACK
	PACK
	VAR_ASSIGN
	VAR_ADDASSIGN
	VAR_SUBASSIGN
	VAR_MULASSIGN
	VAR_DIVASSIGN
	VAR_MODASSIGN
	CALL
	TERMINAL
	MOLECULE
	ARRAY
	DICT
	DICT_ITEM
	REFERENCE
	NAME_SEQUENCE
	NAME_FULL
	SLICE
	RSLICE
	LSLICE
	INDEX
	ADDITION
	SUBTRACTION
	DIVISION
	MULTIPLICATION
	MODULO
)

var astKindStrings = map[AstKind]string{
	MODULE:         "MODULE",
	IMPORT:         "IMPORT",
	IMPORT_PARTIAL: "IMPORT_PARTIAL",
	CLASS:          "CLASS",
	FUNCTION:       "FUNCTION",
	FUNC_PARAM:     "FUNC_PARAM",
	RETURN_TYPE:    "RETURN_TYPE",
	INLINE_SQF:     "INLINE_SQF",
	VAR_DEC:        "VAR_DEC",
	UNPACK:         "UNPACK",
	PACK:           "PACK",
	VAR_ASSIGN:     "VAR_ASSIGN",
	VAR_ADDASSIGN:  "VAR_ADDASSIGN",
	VAR_SUBASSIGN:  "VAR_SUBASSIGN",
	VAR_MULASSIGN:  "VAR_MULASSIGN",
	VAR_DIVASSIGN:  "VAR_DIVASSIGN",
	VAR_MODASSIGN:  "VAR_MODASSIGN",
	CALL:           "CALL",
	TERMINAL:       "TERMINAL",
	MOLECULE:       "MOLECULE",
	ARRAY:          "ARRAY",
	DICT:           "DICT",
	DICT_ITEM:      "DICT_ITEM",
	REFERENCE:      "REFERENCE",
	NAME_SEQUENCE:  "NAME_SEQUENCE",
	NAME_FULL:      "NAME_FULL",
	SLICE:          "SLICE",
	RSLICE:         "RSLICE",
	LSLICE:         "LSLICE",
	INDEX:          "INDEX",
	ADDITION:       "ADDITION",
	SUBTRACTION:    "SUBTRACTION",
	DIVISION:       "DIVISION",
	MULTIPLICATION: "MULTIPLICATION",
	MODULO:         "MODULO",
}

func (k AstKind) String() string {
	return astKindStrings[k]
}

var ignoreKinds = map[LexKind]bool{
	COMMENT_ML:  true,
	COMMENT_SL:  true,
	SPACE_NO_NL: true,
}

var terminators = []LexKind{
	SEMICOLON,
	NEWLINE,
}

func packWithTerminators(kinds ...LexKind) []LexKind {
	kinds = append(kinds, terminators...)

	return kinds
}

type Keyword int

const (
	INVALID_KW Keyword = iota
	MODULE_DEC
	CLASS_DEF
	FUNCTION_DEF
	FOR_STMT
	IF_STMT
	ELSE_STMT
	SQF_STMT
)

var keywords = map[Keyword]string{
	MODULE_DEC:   "module",
	CLASS_DEF:    "class",
	FUNCTION_DEF: "fn",
	FOR_STMT:     "for",
	IF_STMT:      "if",
	ELSE_STMT:    "else",
	SQF_STMT:     "sqf",
}

func (kw Keyword) String() string {
	return keywords[kw]
}

func keywordsJoin() string {
	var builder strings.Builder

	comma := false
	for _, v := range keywords {
		if comma {
			builder.WriteRune(',')
		} else {
			comma = true
		}

		builder.WriteString(v)
	}

	return builder.String()
}

func isKeyword(word string) Keyword {
	for k, v := range keywords {
		if v == word {
			return k
		}
	}
	return INVALID_KW
}

func isIgnoreKind(tok *Token) bool {
	return ignoreKinds[tok.Kind]
}

type AstNode struct {
	Kind     AstKind
	Value    []*Token
	Children []*AstNode
}

func astArrJoin(asts []*AstNode) string {
	var astb strings.Builder

	comma := false
	for _, v := range asts {
		if comma {
			astb.WriteRune(',')
		} else {
			comma = true
		}

		astb.WriteString(v.String())
	}

	return astb.String()
}

func (ast *AstNode) String() string {
	var tokb strings.Builder

	comma := false
	for _, v := range ast.Value {
		if comma {
			tokb.WriteRune(',')
		} else {
			comma = true
		}

		tokb.WriteString(v.String())
	}

	return fmt.Sprintf(
		"<AstNode: {%s, [%s], [%s]}>",
		ast.Kind.String(),
		tokb.String(),
		astArrJoin(ast.Children),
	)
}

type stackItem struct {
	ast    *AstNode
	closer LexKind
}

type parsePath struct {
	eof    bool
	offset int
	par    *Parser
	stack  []*stackItem
	errors []error
}

type parseAsFn func(*AstNode) *AstNode
type parseFn func(*Token) *AstNode

type Parser struct {
	eof              bool
	lexer            *Lexer
	stack            []*stackItem
	token, lookahead *Token
	buf              []*Token
}

func NewParser(reader io.RuneReader) *Parser {
	return &Parser{
		eof:       false,
		lexer:     DefaultLexer(reader),
		token:     nil,
		lookahead: nil,
		buf:       nil,
	}
}

func (par *Parser) newPath(base *parsePath) *parsePath {
	path := &parsePath{
		eof:    false,
		offset: 0,
		par:    par,
		stack:  nil,
	}

	if base != nil {
		path.eof = base.eof
		path.offset = base.offset
	}

	return path
}

func (par *Parser) selectPath(pathaddr **parsePath) {
	path := *pathaddr
	par.buf = par.buf[path.offset:]
	par.eof = par.eof || path.eof
	par.stack = append(par.stack, path.stack...)

	//return path.ast
}

func (par *Parser) nextValidToken() (nxt *Token, err error) {
	for {
		nxt, err = par.lexer.NextToken()

		if err != nil {
			return nil, err
		}

		if !isIgnoreKind(nxt) {
			return
		}
	}
}

func (par *Parser) fillBuf(sz int) error {
	for i := 0; i < sz; i++ {
		tok, err := par.nextValidToken()

		if err != nil {
			return err
		}

		par.buf = append(par.buf, tok)
	}

	return nil
}

func (path *parsePath) errf(tok *Token, format string, args ...interface{}) error {
	e := errhandler.Err("Parser", fmt.Sprintf(format, args...), tok.Line, tok.Col)
	path.errors = append(path.errors, e)

	return e
}

func (path *parsePath) errfExpect(tok *Token, format string, args ...interface{}) error {
	path.errf(tok, format, args...)

	return expectFailed
}

func (path *parsePath) errfUnexpected(tok *Token, got, expected string) error {
	return path.errfExpect(tok, "unexpected %s (expected %s)", got, expected)
}

func selectErrorPath(paths []*parsePath) *parsePath {
	if len(paths) < 1 {
		return nil
	}

	p := paths[0]
	for _, v := range paths {
		if v.offset > p.offset {
			p = v
		}
	}

	return p
}

func (path *parsePath) makeError() error {
	if length := len(path.errors); length > 0 {
		var builder strings.Builder

		for _, e := range path.errors {
			fmt.Println(e)
		}

		return errors.New(builder.String())
	}

	return nil
}

func (path *parsePath) merge(other *parsePath) {
	if other.offset > path.offset {
		path.offset = other.offset
	}

	path.eof = path.eof || other.eof
	path.stack = append(path.stack, other.stack...)
	path.errors = append(path.errors, other.errors...)
}

func (path *parsePath) advance() (*Token, error) {
	if path.eof {
		return nil, io.EOF
	}

	path.offset += 1
	token := path.current()

	if token.Kind == EOF {
		path.eof = true
	}

	return token, nil
}

func (path *parsePath) mustAdvance() *Token {
	tok, err := path.advance()

	if err != nil {
		panic(err)
	}

	return tok
}

func stackPush(stack *[]*stackItem, ast *AstNode, closer LexKind) {
	*stack = append(*stack, &stackItem{ast: ast, closer: closer})
}

func stackPop(stack *[]*stackItem) *AstNode {
	itm := (*stack)[len(*stack)-1]
	*stack = (*stack)[:len(*stack)-1]

	return itm.ast
}

func (path *parsePath) addChild(ast *AstNode) {
	stack := path.stack

	if len(stack) < 1 {
		stack = path.par.stack

		if len(stack) < 1 {
			return
		}
	}

	itm := stack[len(stack)-1]
	itm.ast.Children = append(itm.ast.Children, ast)
}

func (path *parsePath) current() *Token {
	if diff := path.offset - (len(path.par.buf) - 1); diff > 0 {
		err := path.par.fillBuf(diff)

		if err != nil {
			panic(err)
		}
	}

	return path.par.buf[path.offset]
}

func (path *parsePath) lookahead() *Token {
	if diff := path.offset + 1 - (len(path.par.buf) - 1); diff > 0 {
		err := path.par.fillBuf(diff + 1)

		if err != nil {
			panic(err)
		}
	}

	return path.par.buf[path.offset+1]
}

func (path *parsePath) expect(tok *Token, kinds ...LexKind) *Token {
	for _, expect := range kinds {
		if tok.Kind == expect {
			return tok
		}
	}

	panic(path.errfUnexpected(tok, tok.Kind.String(), lexKindsJoin(kinds...)))
}

func (path *parsePath) mustAdvanceExpect(kinds ...LexKind) *Token {
	tok, err := path.advance()

	if err != nil {
		panic(err)
	}

	return path.expect(tok, kinds...)
}

func (path *parsePath) mustTerminate() *Token {
	return path.mustAdvanceExpect(terminators...)
}

func (path *parsePath) lookaheadIs(kinds ...LexKind) bool {
	kind := path.lookahead().Kind

	for _, k := range kinds {
		if kind == k {
			return true
		}
	}

	return false
}

func recoverExpFail(ret **AstNode) {
	if r := recover(); r != nil {
		if r == expectFailed {
			*ret = nil
		}

		panic(r)
	}
}

func tryParse(fn parseFn, tok *Token) (ret *AstNode) {
	defer recoverExpFail(&ret)

	return fn(tok)
}

func tryParseAs(fn parseAsFn, ast *AstNode) (ret *AstNode) {
	defer recoverExpFail(&ret)

	return fn(ast)
}

/*******************************************************************
End helpers
Begin parser functions
*******************************************************************/

func (path *parsePath) parseNameSequence(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     NAME_SEQUENCE,
		Value:    nil,
		Children: []*AstNode{path.parseNameFull(tok)},
	}

	for path.lookaheadIs(COMMA) {
		path.mustAdvanceExpect(COMMA)

		if !path.lookaheadIs(NAME) {
			break
		}

		ast.Children = append(ast.Children, path.parseNameFull(path.mustAdvance()))
	}

	return ast
}

// moduleDec: modulePartial | moduleFull
// modulePartial: "module" name "{" body "}"
// moduleFull: "module" name (";"|"\n")
func (path *parsePath) parseModuleDec(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     MODULE,
		Value:    nil,
		Children: []*AstNode{path.parseNameFull(path.mustAdvance())},
	}

	nxt := path.mustAdvanceExpect(packWithTerminators(CURL_OPEN)...)

	if nxt.Kind == CURL_OPEN {
		stackPush(&path.stack, ast, CURL_CLOSE)
	} else {
		// module applies to whole file
		stackPush(&path.stack, ast, EOF)
	}

	return ast
}

// classDef: "class" name ["(" [ inherits ("," inherits)* ] [","] ")"] "{" body "}"
func (path *parsePath) parseClassDef(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     CLASS,
		Value:    nil,
		Children: []*AstNode{path.parseNameFull(path.mustAdvance())}, // name
	}

	nxt := path.mustAdvanceExpect(PARAN_OPEN, CURL_OPEN)

	// Inherits
	if nxt.Kind == PARAN_OPEN {
		ast.Children = append(ast.Children, path.parseNameSequence(path.mustAdvance()))
		path.mustAdvanceExpect(PARAN_CLOSE)
		path.mustAdvanceExpect(CURL_OPEN)
	}

	stackPush(&path.stack, ast, CURL_CLOSE)

	return ast
}

// param: type name ["=" expr]
func (path *parsePath) parseFunctionParam(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:  FUNC_PARAM,
		Value: nil,
		Children: []*AstNode{
			path.parseNameFull(tok),                // type
			path.parseNameFull(path.mustAdvance()), // name
		},
	}

	if path.lookaheadIs(ASSIGN) {
		path.mustAdvanceExpect(ASSIGN)
		ast.Children = append(ast.Children, path.parseExpr(path.mustAdvance()))
	}

	return ast
}

// funcDef: "fn" name "(" [ param ("," param)* ] [","] ")" [":" ret_type ("," ret_type)* [","]] "{" body "}"
// assumes "fn" has been encountered
func (path *parsePath) parseFunctionDef(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     FUNCTION,
		Value:    nil,
		Children: []*AstNode{path.parseNameFull(path.mustAdvance())}, // name
	}

	path.mustAdvanceExpect(PARAN_OPEN)

	nxt := path.mustAdvanceExpect(PARAN_CLOSE, NAME)

	for nxt.Kind != PARAN_CLOSE {
		ast.Children = append(ast.Children, path.parseFunctionParam(nxt))
		nxt = path.mustAdvanceExpect(PARAN_CLOSE, COMMA)

		if nxt.Kind == COMMA {
			nxt = path.mustAdvanceExpect(PARAN_CLOSE, NAME)
		}
	}

	nxt = path.mustAdvanceExpect(COLON, CURL_OPEN)

	if nxt.Kind == COLON {
		ast.Children = append(ast.Children, &AstNode{
			Kind:     RETURN_TYPE,
			Value:    nil,
			Children: []*AstNode{path.parseNameSequence(path.mustAdvance())},
		})
		nxt = path.mustAdvanceExpect(COMMA, CURL_OPEN)

		if nxt.Kind == COMMA {
			path.mustAdvanceExpect(CURL_OPEN)
		}
	}

	stackPush(&path.stack, ast, CURL_CLOSE)

	return ast
}

// inlineSqf: "sqf" string (";"|"\n")
// assumes "sqf" has been encountered
func (path *parsePath) parseInlineSQF(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     INLINE_SQF,
		Value:    []*Token{path.mustAdvanceExpect(STRING)},
		Children: nil,
	}

	path.mustTerminate()

	return ast
}

func (path *parsePath) parseTerminal(tok *Token) *AstNode {
	return &AstNode{
		Kind:     TERMINAL,
		Value:    []*Token{path.expect(tok, NUMBER, STRING, NAME)},
		Children: nil,
	}
}

func (path *parsePath) parseName(tok *Token) *AstNode {
	return &AstNode{
		Kind:     TERMINAL,
		Value:    []*Token{path.expect(tok, NAME)},
		Children: nil,
	}
}

func (path *parsePath) parseNameFull(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     NAME_FULL,
		Value:    nil,
		Children: []*AstNode{path.parseName(tok)},
	}

	for path.lookaheadIs(PERIOD) {
		path.mustAdvanceExpect(PERIOD)

		ast.Children = append(ast.Children, path.parseName(path.mustAdvance()))
	}

	return ast
}

func (path *parsePath) parseArray(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     ARRAY,
		Value:    nil,
		Children: nil,
	}

	path.mustAdvanceExpect(SQBRAC_OPEN)
	nxt := path.mustAdvance()

	for nxt.Kind != SQBRAC_CLOSE {
		ast.Children = append(ast.Children, path.parseExpr(nxt))
		nxt = path.mustAdvanceExpect(COMMA, SQBRAC_CLOSE)

		if nxt.Kind == COMMA && path.lookaheadIs(SQBRAC_CLOSE) {
			path.mustAdvanceExpect(SQBRAC_CLOSE)
			break
		}
	}

	return ast
}

func (path *parsePath) parseDictItem(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     DICT_ITEM,
		Value:    nil,
		Children: []*AstNode{path.parseExpr(tok)},
	}

	path.mustAdvanceExpect(COLON)
	ast.Children = append(ast.Children, path.parseExpr(path.mustAdvance()))

	return ast
}

func (path *parsePath) parseDict(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     DICT,
		Value:    nil,
		Children: nil,
	}

	path.mustAdvanceExpect(CURL_OPEN)

	for nxt := path.mustAdvance(); nxt.Kind != CURL_CLOSE; {
		ast.Children = append(ast.Children, path.parseDictItem(tok))
		nxt = path.mustAdvanceExpect(COMMA, CURL_CLOSE)

		if nxt.Kind == COMMA && path.lookaheadIs(CURL_CLOSE) {
			path.mustAdvanceExpect(CURL_CLOSE)
			break
		}
	}

	return ast
}

// atom: NUMBER | string | array | dict | bool
func (path *parsePath) parseAtom(tok *Token) *AstNode {
	parsers := []func(*parsePath) parseFn{
		func(p *parsePath) parseFn { return p.parseTerminal },
		func(p *parsePath) parseFn { return p.parseArray },
		func(p *parsePath) parseFn { return p.parseDict },
	}

	paths := []*parsePath{}

	for _, p := range parsers {
		newpath := path.par.newPath(path)
		paths = append(paths, newpath)

		if ast := tryParse(p(newpath), tok); ast != nil {
			path.merge(newpath)
			return ast
		}
	}

	errpath := selectErrorPath(paths)

	if errpath != nil {
		if err := errpath.makeError(); err != nil {
			// TODO: Fix this, should panic unexpected (?)
			//panic(err)
		}
	}

	panic(path.errfUnexpected(tok, "atom", "not atom :D"))
}

func (path *parsePath) parseAsAtom(prev *AstNode) *AstNode {
	return path.parseAtom(path.mustAdvance())
}

// reference: NAME ("." NAME)*
func (path *parsePath) parseAsReference(prev *AstNode) *AstNode {
	ast := &AstNode{
		Kind:     REFERENCE,
		Value:    nil,
		Children: []*AstNode{prev},
	}

	path.mustAdvanceExpect(PERIOD)
	ast.Children = append(ast.Children, path.parseMolecule(path.mustAdvance()))

	return ast
}

func (path *parsePath) parseAsCall(prev *AstNode) *AstNode {
	ast := &AstNode{
		Kind:     CALL,
		Value:    nil,
		Children: []*AstNode{prev},
	}

	nxt := path.mustAdvanceExpect(PARAN_OPEN)

	for nxt.Kind != PARAN_CLOSE {
		ast.Children = append(ast.Children, path.parseExpr(path.mustAdvance()))
		nxt = path.mustAdvanceExpect(COMMA, PARAN_CLOSE)

		if nxt.Kind == COMMA && path.lookaheadIs(PARAN_CLOSE) {
			path.mustAdvanceExpect(PARAN_CLOSE)
			break
		}
	}

	return ast
}

func (path *parsePath) parseAsSlice(prev *AstNode) *AstNode {
	ast := &AstNode{
		Kind:     SLICE,
		Value:    nil,
		Children: []*AstNode{prev},
	}

	path.mustAdvanceExpect(SQBRAC_OPEN)

	nxt := path.mustAdvance()

	if nxt.Kind == COLON {
		ast.Kind = RSLICE
		ast.Children = append(ast.Children, path.parseExpr(path.mustAdvance()))

		path.mustAdvanceExpect(SQBRAC_CLOSE)
	} else if nxt.Kind != SQBRAC_CLOSE {
		ast.Children = append(ast.Children, path.parseExpr(nxt))

		nxt := path.mustAdvanceExpect(COLON, SQBRAC_CLOSE)

		if nxt.Kind == COLON {
			nxt := path.mustAdvance()

			if nxt.Kind == SQBRAC_CLOSE {
				ast.Kind = LSLICE
			} else {
				ast.Children = append(ast.Children, path.parseExpr(nxt))

				path.mustAdvanceExpect(SQBRAC_CLOSE)
			}
		} else {
			ast.Kind = INDEX
		}
	}

	return ast
}

// molecule: molecule "(" [expr ("," expr)* [","]] ")"
// 		|	 molecule "[" slice "]"
// 		| 	 atom
func (path *parsePath) parseMolecule(tok *Token) *AstNode {
	parsers := []func(*parsePath) parseAsFn{
		func(p *parsePath) parseAsFn { return p.parseAsCall },
		func(p *parsePath) parseAsFn { return p.parseAsSlice },
		func(p *parsePath) parseAsFn { return p.parseAsReference },
	}

	ast := &AstNode{
		Kind:     MOLECULE,
		Value:    nil,
		Children: nil,
	}

	prev := path.parseAtom(tok)

	for notFound := false; !notFound; {
		notFound = true
		for _, p := range parsers {
			newpath := path.par.newPath(path)

			if childAst := tryParseAs(p(newpath), prev); childAst != nil {
				notFound = false

				prev = childAst
				path.merge(newpath)
				ast.Children = append(ast.Children, prev)
				break
			}
		}
	}

	if len(ast.Children) == 0 {
		ast.Children = append(ast.Children, prev)
	}

	return ast
}

type arithExpr struct {
	operand LexKind
	output  AstKind
}

func (path *parsePath) parseArithExpr(tok *Token, exprs []*arithExpr, idx int) *AstNode {
	nxtGetter := func(tok *Token) *AstNode {
		return path.parseArithExpr(tok, exprs, idx+1)
	}

	// last iteration
	if idx >= len(exprs)-1 {
		nxtGetter = path.parseMolecule
	}

	expr := exprs[idx]

	ast := &AstNode{
		Kind:     expr.output,
		Value:    nil,
		Children: []*AstNode{nxtGetter(tok)},
	}

	for path.lookaheadIs(expr.operand) {
		path.mustAdvanceExpect(expr.operand)

		ast.Children = append(ast.Children, nxtGetter(path.mustAdvance()))
	}

	if len(ast.Children) > 1 {
		return ast
	}

	// operand wasnt encountered, return first and only argument
	return ast.Children[0]
}

// expr: add_expr
// add_expr: sub_expr ("+" sub_expr)*
// sub_expr: div_expr ("-" div_expr)*
// div_expr: mul_expr ("/" mul_expr)*
// mul_expr: mod_expr ("*" mod_expr)*
// mod_expr: atom ("%" atom)*
func (path *parsePath) parseExpr(tok *Token) *AstNode {
	exprs := []*arithExpr{
		{PLUS, ADDITION},
		{MINUS, SUBTRACTION},
		{SLASH, DIVISION},
		{STAR, MULTIPLICATION},
		{PERCENTAGE, MODULO},
	}

	return path.parseArithExpr(tok, exprs, 0)
}

func (path *parsePath) parseCompound(tok *Token) *AstNode {
	kw := isKeyword(string(tok.Lexeme))

	if tok.Kind != NAME || kw == INVALID_KW {
		panic(path.errfUnexpected(tok, tok.Kind.String(), NAME.String()))
	}

	switch kw {
	case MODULE_DEC:
		return path.parseModuleDec(tok)
	case CLASS_DEF:
		return path.parseClassDef(tok)
	case FUNCTION_DEF:
		return path.parseFunctionDef(tok)
	case SQF_STMT:
		return path.parseInlineSQF(tok)
	}

	panic(path.errfUnexpected(tok, kw.String(), keywordsJoin()))
}

func (path *parsePath) parsePack(tok *Token) *AstNode {
	ast := &AstNode{
		Kind: PACK,
		Children: []*AstNode{
			path.parseExpr(tok),
		},
	}

	for path.lookaheadIs(COMMA) {
		path.mustAdvanceExpect(COMMA)

		if !path.lookaheadIs(NAME) {
			break
		}

		ast.Children = append(ast.Children, path.parseExpr(path.mustAdvance()))
	}

	return ast
}

func (path *parsePath) parseUnpack(tok *Token) *AstNode {
	ast := path.parsePack(tok)
	ast.Kind = UNPACK

	return ast
}

func (path *parsePath) parseVarDec(tok *Token) *AstNode {
	ast := &AstNode{
		Kind: VAR_DEC,
		Children: []*AstNode{
			path.parseNameFull(tok),              // type
			path.parseUnpack(path.mustAdvance()), // name (can be slices, function calls(?) etc)
		},
	}

	nxt := path.mustAdvanceExpect(packWithTerminators(ASSIGN)...)

	if nxt.Kind == ASSIGN {
		ast.Children = append(ast.Children, path.parsePack(path.mustAdvance()))
		path.mustTerminate()
	}

	return ast
}

func (path *parsePath) parseVarAssign(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     VAR_ASSIGN,
		Children: []*AstNode{path.parseUnpack(tok)},
	}

	path.mustAdvanceExpect(ASSIGN)
	ast.Children = append(ast.Children, path.parsePack(path.mustAdvance()))

	path.mustTerminate()

	return ast
}

func (par *Parser) parseTok(path *parsePath, tok *Token) (*parsePath, *AstNode, error) {
	parsers := []func(p *parsePath) parseFn{
		func(p *parsePath) parseFn { return p.parseCompound },
		func(p *parsePath) parseFn { return p.parseVarDec },
		func(p *parsePath) parseFn { return p.parseVarAssign },
	}

	paths := []*parsePath{}

	for _, p := range parsers {
		newpath := par.newPath(path)
		paths = append(paths, newpath)

		if ast := tryParse(p(newpath), tok); ast != nil {
			// we add to path because newpath might contain a new stack item,
			// meaning if we add a child, the ast becomes its own parent/child
			path.addChild(ast)
			return newpath, ast, nil
		}
	}

	errpath := selectErrorPath(paths)
	err := path.errf(tok, "unable to parse")

	if errpath != nil {
		path = errpath
	}

	return path, nil, err
}

func (par *Parser) parseOne() (*AstNode, error) {
	path := par.newPath(nil)
	pathaddr := &path
	defer par.selectPath(pathaddr)

	nxt, err := path.advance()

	for {
		if err != nil {
			return nil, err
		}

		if len(par.stack) > 0 && nxt.Kind == (par.stack[len(par.stack)-1].closer) {
			stackPop(&par.stack)
		} else if !(nxt.Kind == SEMICOLON || nxt.Kind == NEWLINE) {
			break
		}

		nxt, err = path.advance()
	}

	var ast *AstNode
	path, ast, err = par.parseTok(path, nxt)
	*pathaddr = path

	if err != nil {
		if tmp := path.makeError(); tmp != nil {
			return nil, tmp
		}

		return nil, err
	}

	return ast, nil
}

func (par *Parser) Parse() []*AstNode {
	nodes := []*AstNode{}

	for {
		nxt, err := par.parseOne()

		if err != nil {
			if err == io.EOF {
				break
			}

			panic(err)
		}

		if nxt != nil {
			nodes = append(nodes, nxt)
		}
	}

	if length := len(par.stack); length > 0 {
		var sbuilder strings.Builder

		comma := true
		for i := length; i >= 0; i-- {
			v := par.stack[i]

			if comma {
				sbuilder.WriteRune(',')
			} else {
				comma = false
			}

			sbuilder.WriteString(v.closer.String())
		}

		panic(fmt.Errorf(
			"unexpected EOF (expected %s (respectively))",
			sbuilder.String(),
		))
	}

	return nodes
}
