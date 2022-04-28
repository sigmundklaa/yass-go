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
	INLINE_SQF
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

type parsePath struct {
	eof    bool
	offset int
	an     *Analyser
	stack  []*stackItem
}

type Analyser struct {
	eof              bool
	lexer            *Lexer
	stack            []*stackItem
	token, lookahead *Token
	buf              []*Token
}

func NewAnalyser(reader io.RuneReader) *Analyser {
	return &Analyser{
		eof:       false,
		lexer:     DefaultLexer(reader),
		token:     nil,
		lookahead: nil,
		buf:       nil,
	}
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

func (an *Analyser) newPath() *parsePath {
	return &parsePath{
		eof:    false,
		offset: 0,
		an:     an,
		stack:  nil,
	}
}

func (an *Analyser) selectPath(path *parsePath) {
	an.buf = an.buf[path.offset:]
	an.eof = an.eof || path.eof
	an.stack = append(an.stack, path.stack...)

	//return path.ast
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

func (an *Analyser) fillBuf() error {
	tok, err := an.nextValidToken()

	if err != nil {
		return err
	}

	an.buf = append(an.buf, tok)

	return nil
}

func (path *parsePath) advance() (*Token, error) {
	if path.eof {
		return nil, io.EOF
	}

	var token *Token
	var err error

	if path.lookahead() == nil {
		err = path.an.fillBuf()

		if err != nil {
			return nil, err
		}
	}

	path.offset += 1
	token = path.current()

	if token.Kind == EOF {
		path.eof = true
	}

	return token, nil
}

func stackPush(stack *[]*stackItem, ast *AstNode, closer LexKind) {
	*stack = append(*stack, &stackItem{ast: ast, closer: closer})
}

func stackPop(stack *[]*stackItem, tok *Token) *AstNode {
	itm := (*stack)[len(*stack)-1]
	*stack = (*stack)[:len(*stack)-1]

	return itm.ast
}

func (path *parsePath) current() *Token {
	if path.offset >= len(path.an.buf) {
		return nil
	}

	return path.an.buf[path.offset]
}

func (path *parsePath) lookahead() *Token {
	if path.offset+1 >= len(path.an.buf) {
		return nil
	}

	return path.an.buf[path.offset+1]
}

func (path *parsePath) mustAdvanceExpect(kinds ...LexKind) *Token {
	tok, err := path.advance()

	if err != nil {
		panic(err)
	}

	for _, expect := range kinds {
		if tok.Kind == expect {
			return tok
		}
	}

	panic(path.an.errUnexpectedKind(lexKindsJoin(kinds...), tok))
}

func (path *parsePath) mustTerminate() *Token {
	return path.mustAdvanceExpect(NEWLINE, SEMICOLON)
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

/*******************************************************************
End helpers
Begin parser functions
*******************************************************************/

func (path *parsePath) parseSequence(closerKind, seperator LexKind, allowedTypes ...LexKind) []*Token {
	toks := []*Token{}
	allowedTypes = append(allowedTypes, closerKind)

	for {
		nxt := path.mustAdvanceExpect(allowedTypes...)

		if nxt.Kind == closerKind {
			break
		}

		toks = append(toks, nxt)

		sep := path.mustAdvanceExpect(seperator, closerKind)

		if sep.Kind == closerKind {
			break
		}
	}

	return toks
}

// moduleDec: modulePartial | moduleFull
// modulePartial: "module" name "{" body "}"
// moduleFull: "module" name (";"|"\n")
func (path *parsePath) parseModuleDec(tok *Token) *AstNode {
	name := path.mustAdvanceExpect(NAME)
	nxt := path.mustAdvanceExpect(CURL_OPEN, SEMICOLON, NEWLINE)
	ast := &AstNode{
		Kind:     MODULE,
		Value:    []*Token{name},
		Args:     nil,
		Children: nil,
	}

	if nxt.Kind == CURL_OPEN {
		stackPush(&path.stack, ast, CURL_CLOSE)
	} else {
		stackPush(&path.stack, ast, EOF)
	}

	return ast
}

// classDef: "class" name ["(" [ inherits ("," inherits)* ] [","] ")"] "{" body "}"
func (path *parsePath) parseClassDef(tok *Token) *AstNode {
	name := path.mustAdvanceExpect(NAME)
	nxt := path.mustAdvanceExpect(PARAN_OPEN, CURL_OPEN)
	ast := &AstNode{
		Kind:     CLASS,
		Value:    []*Token{name},
		Args:     nil,
		Children: nil,
	}

	if nxt.Kind == PARAN_OPEN {
		// Inherits
		ast.Value = append(ast.Value, path.parseSequence(PARAN_CLOSE, COMMA, NAME)...)

		path.mustAdvanceExpect(CURL_OPEN)
	}

	stackPush(&path.stack, ast, CURL_CLOSE)

	return ast
}

// param: type name ["=" expr]
func (path *parsePath) parseFunctionParam(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     FUNC_PARAM,
		Value:    []*Token{tok, path.mustAdvanceExpect(NAME)},
		Args:     nil,
		Children: nil,
	}

	if path.lookaheadIs(ASSIGN) {
		path.mustAdvanceExpect(ASSIGN)
		ast.Args = append(ast.Args, path.parseExpr(nil))
	}

	return ast
}

// funcDef: "fn" name "(" [ param ("," param)* ] [","] ")" [":" ret_type] "{" body "}"
func (path *parsePath) parseFunctionDef(tok *Token) *AstNode {
	name := path.mustAdvanceExpect(NAME)
	ast := &AstNode{
		Kind:     FUNCTION,
		Value:    []*Token{name},
		Args:     nil,
		Children: nil,
	}

	path.mustAdvanceExpect(PARAN_OPEN)

	nxt := path.mustAdvanceExpect(PARAN_CLOSE, NAME, COMMA)

	if nxt.Kind == COMMA {
		nxt = path.mustAdvanceExpect(PARAN_CLOSE)
	}

	for nxt.Kind != PARAN_CLOSE {
		ast.Args = append(ast.Args, path.parseFunctionParam(nxt))
		nxt = path.mustAdvanceExpect(PARAN_CLOSE, COMMA)

		if nxt.Kind == COMMA {
			nxt = path.mustAdvanceExpect(PARAN_CLOSE, NAME)
		}
	}

	nxt = path.mustAdvanceExpect(COLON, CURL_OPEN)

	if nxt.Kind == COLON {
		ast.Args = append(ast.Args, &AstNode{
			Kind:     RETURN_TYPE,
			Value:    []*Token{path.mustAdvanceExpect(NAME)},
			Args:     nil,
			Children: nil,
		})
		nxt = path.mustAdvanceExpect(CURL_OPEN)
	}

	stackPush(&path.stack, ast, CURL_CLOSE)

	return ast
}

// inlineSqf: "sqf" string (";"|"\n")
func (path *parsePath) parseInlineSQF(tok *Token) *AstNode {
	ast := &AstNode{
		Kind:     INLINE_SQF,
		Value:    []*Token{path.mustAdvanceExpect(STRING)},
		Args:     nil,
		Children: nil,
	}

	path.mustTerminate()

	return ast
}

func (path *parsePath) parseExpr(tok *Token) *AstNode {
	return nil
}

func (path *parsePath) parseKeyword(tok *Token, kw KeyWord) *AstNode {
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

	panic(path.an.errUnexpectedLexeme("keyword", tok))
}

func (an *Analyser) parseTok(path *parsePath, tok *Token) (*parsePath, *AstNode, error) {
	switch tok.Kind {
	case NAME:
		if kw := isKeyWord(string(tok.Lexeme)); kw != INVALID_KW {
			return path, path.parseKeyword(tok, kw), nil
		}
	}

	return path, nil, path.an.errf(tok, "unable to parse")
}

func (an *Analyser) parseOne() (*AstNode, error) {
	path := an.newPath()
	defer an.selectPath(path)

	nxt, err := path.advance()

	if err != nil {
		return nil, err
	}

	if len(an.stack) > 0 && nxt.Kind == (an.stack[len(an.stack)-1].closer) {
		return stackPop(&an.stack, nxt), nil
	}

	var ast *AstNode
	path, ast, err = an.parseTok(path, nxt)

	if err != nil {
		return nil, err
	}

	return ast, nil
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
