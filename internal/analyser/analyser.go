package analyser

import (
	"fmt"
	"io"

	"github.com/SigJig/yass-go/internal/errhandler"
)

type AstKind int

const (
	MODULE AstKind = iota
	CLASS
	FUNCTION
	FUNC_PARAM
	RETURN_TYPE
)

var ignoreKinds = map[LexKind]bool{
	COMMENT_ML:  true,
	COMMENT_SL:  true,
	SPACE_NO_NL: true,
}

type KeyWord int

const (
	INVALID_KW KeyWord = iota
	MODULE_DEC
	CLASS_DEF
	FUNCTION_DEF
	FOR_STMT
	IF_STMT
	ELSE_STMT
	SQF_STMT
)

var keywords = map[KeyWord]string{
	MODULE_DEC:   "module",
	CLASS_DEF:    "class",
	FUNCTION_DEF: "fn",
	FOR_STMT:     "for",
	IF_STMT:      "if",
	ELSE_STMT:    "else",
	SQF_STMT:     "sqf",
}

func isKeyWord(word string) KeyWord {
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
	Args     []*AstNode
	Children []*AstNode
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
	return an.errf(got, "unexpected token type: expected (%s), got: %s", expected, got.Kind.String())
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
		return nil, io.EOF
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

func (an *Analyser) stackPush(ast *AstNode, closer LexKind) {
	an.stack = append(an.stack, &stackItem{ast: ast, closer: closer})
}

func (an *Analyser) stackPop(tok *Token) *AstNode {
	itm := an.stack[len(an.stack)-1]
	an.stack = an.stack[:len(an.stack)-1]

	return itm.ast
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

func (an *Analyser) mustTerminate() *Token {
	return an.mustAdvanceExpect(NEWLINE, SEMICOLON)
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

// moduleDec: modulePartial | moduleFull
// modulePartial: "module" name "{" body "}"
// moduleFull: "module" name (";"|"\n")
func (an *Analyser) parseModuleDec(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	nxt := an.mustAdvanceExpect(CURL_OPEN, SEMICOLON, NEWLINE)
	ast := &AstNode{Kind: MODULE, Value: []*Token{name}, Args: nil, Children: nil}

	if nxt.Kind == CURL_OPEN {
		an.stackPush(ast, CURL_CLOSE)
	} else {
		an.stackPush(ast, EOF)
	}

	return ast
}

// classDef: "class" name ["(" [ inherits ("," inherits)* ] [","] ")"] "{" body "}"
func (an *Analyser) parseClassDef(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	nxt := an.mustAdvanceExpect(PARAN_OPEN, CURL_OPEN)
	ast := &AstNode{Kind: CLASS, Value: []*Token{name}, Args: nil, Children: nil}

	if nxt.Kind == PARAN_OPEN {
		// Inherits
		ast.Value = append(ast.Value, an.parseSequence(PARAN_CLOSE, COMMA, NAME)...)

		an.mustAdvanceExpect(CURL_OPEN)
	}

	an.stackPush(ast, CURL_CLOSE)

	return ast
}

// param: type name ["=" expr]
func (an *Analyser) parseFunctionParam(tok *Token) *AstNode {
	ast := &AstNode{Kind: FUNC_PARAM, Value: []*Token{tok, an.mustAdvanceExpect(NAME)}, Args: nil, Children: nil}

	if an.lookaheadIs(ASSIGN) {
		an.mustAdvanceExpect(ASSIGN)
		ast.Args = append(ast.Args, an.parseExpr(nil))
	}

	return ast
}

// funcDef: "fn" name "(" [ param ("," param)* ] [","] ")" [":" ret_type] "{" body "}"
func (an *Analyser) parseFunctionDef(tok *Token) *AstNode {
	name := an.mustAdvanceExpect(NAME)
	ast := &AstNode{Kind: FUNCTION, Value: []*Token{name}, Args: nil, Children: nil}

	an.mustAdvanceExpect(PARAN_OPEN)

	nxt := an.mustAdvanceExpect(PARAN_CLOSE, NAME, COMMA)

	if nxt.Kind == COMMA {
		nxt = an.mustAdvanceExpect(PARAN_CLOSE)
	}

	for nxt.Kind != PARAN_CLOSE {
		ast.Args = append(ast.Args, an.parseFunctionParam(nxt))
		nxt = an.mustAdvanceExpect(PARAN_CLOSE, COMMA)

		if nxt.Kind == COMMA {
			nxt = an.mustAdvanceExpect(PARAN_CLOSE, NAME)
		}
	}

	nxt = an.mustAdvanceExpect(COLON, CURL_OPEN)

	if nxt.Kind == COLON {
		ast.Args = append(ast.Args, &AstNode{Kind: RETURN_TYPE, Value: []*Token{an.mustAdvanceExpect(NAME)}, Args: nil, Children: nil})
		nxt = an.mustAdvanceExpect(CURL_OPEN)
	}

	an.stackPush(ast, CURL_CLOSE)

	return ast
}

func (an *Analyser) parseExpr(tok *Token) *AstNode {
	return nil
}

func (an *Analyser) parseKeyword(tok *Token, kw KeyWord) *AstNode {
	switch kw {
	case MODULE_DEC:
		return an.parseModuleDec(tok)
	case CLASS_DEF:
		return an.parseClassDef(tok)
	case FUNCTION_DEF:
		return an.parseFunctionDef(tok)
	}

	panic(an.errUnexpectedLexeme("keyword", tok))
}

func (an *Analyser) parseOne() (*AstNode, error) {
	nxt, err := an.advance()

	if err != nil {
		return nil, err
	}

	if len(an.stack) > 0 && nxt.Kind == (an.stack[len(an.stack)-1].closer) {
		return an.stackPop(nxt), nil
	}

	switch nxt.Kind {
	case NAME:
		if kw := isKeyWord(string(nxt.Lexeme)); kw != INVALID_KW {
			return an.parseKeyword(nxt, kw), nil
		}
	}

	return nil, an.errf(nxt, "unable to parse")
}

func (an *Analyser) Parse() []*AstNode {
	nodes := []*AstNode{}

	for {
		nxt, err := an.parseOne()

		if err != nil {
			if err == io.EOF {
				break
			}
			panic(err)
		}

		nodes = append(nodes, nxt)
	}
	return nodes
}
