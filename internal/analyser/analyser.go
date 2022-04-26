package analyser

import (
	"fmt"

	"github.com/SigJig/yass-go/internal/errhandler"
)

type AstKind int

const (
	MODULE AstKind = iota
	CLASS
	FUNCTION
)

var ignoreKinds = map[LexKind]bool{
	COMMENT_ML:  true,
	COMMENT_SL:  true,
	SPACE_NO_NL: true,
}

var keywords = stringset{}.add(
	"module",
	"class",
	"fn",
	"for",
	"if",
	"else",
)

func isKeyWord(word string) bool {
	return keywords.contains(word)
}

func isIgnoreKind(tok *Token) bool {
	return ignoreKinds[tok.Kind]
}

type AstNode struct {
	Kind     AstKind
	Args     []*Token
	Children []*Token
}

type stackItem struct {
	ast    *AstNode
	closer LexKind
}

type Analyser struct {
	eof              bool
	lexer            *Lexer
	stack            []*stackItem
	token, lookahead *Token
}

func NewAnalyser() *Analyser {
	return &Analyser{}
}

func (an *Analyser) errf(tok *Token, format string, args ...interface{}) error {
	return errhandler.Err("analyser", fmt.Sprintf(format, args...), tok.Line, tok.Col)
}

func (an *Analyser) errUnexpectedKind(expected string, got *Token) error {
	return an.errf(got, "unexpected token type: expected (%s), got: %s", expected, string(got.Kind))
}

func (an *Analyser) errUnexpectedLexeme(expected string, got *Token) error {
	return an.errf(got, "unexpected token value: expected (%s), got: %s", expected, string(got.Lexeme))
}

func (an *Analyser) nextValidToken() (nxt *Token, err error) {
	for {
		nxt, err = an.lexer.NextToken()

		if err != nil {
			return nil, err
		}

		if !isIgnoreKind(nxt) {
			return
		}
	}
}

func (an *Analyser) advance() (*Token, error) {
	if an.eof {
		return nil, fmt.Errorf("eof reached")
	}

	var token *Token
	var err error

	if an.token == nil || an.lookahead == nil {
		token, err = an.nextValidToken()

		if err != nil {
			return nil, err
		}
	} else {
		token = an.lookahead
	}

	if token.Kind == EOF {
		an.eof = true
	} else {
		an.lookahead, err = an.nextValidToken()

		if err != nil {
			return nil, err
		}
	}

	return token, nil
}

func (an *Analyser) stackAdd(ast *AstNode, closer LexKind) {
	an.stack = append(an.stack, &stackItem{ast: ast, closer: closer})
}

func (an *Analyser) stackPop(tok *Token) {
	an.stack = an.stack[:len(an.stack)-1]
}

func (an *Analyser) mustAdvanceExpect(kinds ...LexKind) *Token {
	tok, err := an.advance()

	if err != nil {
		panic(err)
	}

	for _, expect := range kinds {
		if tok.Kind == expect {
			return tok
		}
	}

	panic(an.errUnexpectedKind(lexKindsJoin(kinds...), tok))
}

func (an *Analyser) lookaheadIs(kinds ...LexKind) bool {
	kind := an.lookahead.Kind

	for _, k := range kinds {
		if kind == k {
			return true
		}
	}

	return false
}

/*******************************************************************
End helpers
Begin parser functions
*******************************************************************/

func (an *Analyser) parseSequence(closerKind, seperator LexKind, allowedTypes ...LexKind) []*Token {
	toks := []*Token{}
	allowedTypes = append(allowedTypes, closerKind)

	for {
		nxt := an.mustAdvanceExpect(allowedTypes...)

		if nxt.Kind == closerKind {
			break
		}

		toks = append(toks, nxt)

		sep := an.mustAdvanceExpect(seperator, closerKind)

		if sep.Kind == closerKind {
			break
		}
	}

	return toks
}

func (an *Analyser) parseModuleDec(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	nxt := an.mustAdvanceExpect(CURL_OPEN, SEMICOLON, NEWLINE)
	ast := &AstNode{Kind: MODULE, Args: []*Token{name}, Children: nil}

	if nxt.Kind == CURL_OPEN {
		an.stackAdd(ast, CURL_CLOSE)
	} else {
		an.stackAdd(ast, EOF)
	}

	return ast
}

// classDef: "class" name ["(" inherits ("," inherits)* [","] ")"] "{" body "}"
// on inherit, returns Kind = "class_def_inherit", otherwise "class_def_noinherit"
// on inherits returns
func (an *Analyser) parseClassDef(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	nxt := an.mustAdvanceExpect(PARAN_OPEN, CURL_OPEN)
	ast := &AstNode{Kind: CLASS, Args: []*Token{name}, Children: nil}

	if nxt.Kind == PARAN_OPEN {
		// Inherits
		ast.Args = append(ast.Args, an.parseSequence(PARAN_CLOSE, COMMA, NAME)...)

		an.mustAdvanceExpect(CURL_OPEN)
	}

	an.stackAdd(ast, CURL_CLOSE)

	return ast
}

// param: type name ["=" expr]
func (an *Analyser) parseFunctionParam(tok *Token) *Token {
	// TODO: Verify type?
	//	nxt := an.mustAdvanceExpect("name")

	if an.lookaheadIs(ASSIGN) {
		// TODO: = expr
	}

	return nil
}

// funcDef: "fn" name "(" param ("," param)* [","] ")" [":" ret_type] "{" body "}"
func (an *Analyser) parseFunctionDef(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	ast := &AstNode{Kind: FUNCTION, Args: []*Token{name}, Children: nil}

	an.mustAdvanceExpect(PARAN_OPEN)
	nxt := an.mustAdvanceExpect(PARAN_CLOSE, NAME)

	for nxt.Kind != PARAN_CLOSE {
		ast.Args = append(ast.Args, an.parseFunctionParam(nxt))
		nxt = an.mustAdvanceExpect(PARAN_CLOSE, NAME)
	}

	nxt = an.mustAdvanceExpect(COLON, CURL_OPEN)

	if nxt.Kind == COLON {
		// TODO: Add return type

		//		retT := an.mustAdvanceExpect("name")

		nxt = an.mustAdvanceExpect(CURL_OPEN)
	}

	an.stackAdd(ast, CURL_CLOSE)

	return ast
}

func (an *Analyser) parseKeyword(tok *Token) *AstNode {
	switch string(tok.Lexeme) {
	case "module":
		return an.parseModuleDec(tok)
	case "class":
		return an.parseClassDef(tok)
	case "fn":
		return an.parseFunctionDef(tok)
	}

	panic(an.errUnexpectedLexeme("keyword", tok))
}

func (an *Analyser) Parse() *AstNode {
	nxt, err := an.advance()

	if err != nil {
		panic(err)
	}

	if len(an.stack) > 0 && nxt.Kind == (an.stack[len(an.stack)-1].closer) {
		an.stackPop(nxt)
		return nil
	}

	switch nxt.Kind {
	case NAME:
		if isKeyWord(string(nxt.Lexeme)) {
			return an.parseKeyword(nxt)
		}
	}

	panic(an.errf(nxt, "unable to parse"))
}
