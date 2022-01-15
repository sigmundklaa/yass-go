package analyser

import (
	"fmt"
	"strings"

	"github.com/SigJig/yass-go/internal/errhandler"
)

var ignoreKinds = stringset{}.add(
	"comment_ml",
	"comment_sl",
	"space_no_nl",
)

var keywords = stringset{}.add(
	"module",
)

func isKeyWord(word string) bool {
	return keywords.contains(word)
}

func isIgnoreKind(tok *Token) bool {
	return ignoreKinds.contains(tok.Kind)
}

type Ast struct {
	Kind string
	Data []*Token
}

type Analyser struct {
	eof              bool
	lexer            *Lexer
	stack            []string
	token, lookahead *Token
}

func NewAnalyser() *Analyser {
	return &Analyser{}
}

func (an *Analyser) errf(tok *Token, format string, args ...interface{}) error {
	return errhandler.Err("analyser", fmt.Sprintf(format, args...), tok.Line, tok.Col)
}

func (an *Analyser) errUnexpected(expected string, got *Token) error {
	return an.errf(got, "unexpected token: expected (%s), got: %s", expected, string(got.Lexeme))
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

	if token.Kind == "EOF" {
		an.eof = true
	}

	an.lookahead, err = an.nextValidToken()

	if err != nil {
		return nil, err
	}

	return token, nil
}

func (an *Analyser) mustAdvanceExpect(kinds ...string) *Token {
	tok, err := an.advance()

	if err != nil {
		panic(err)
	}

	for _, expect := range kinds {
		if tok.Kind == expect {
			return tok
		}
	}

	panic(an.errUnexpected(strings.Join(kinds, ","), tok))
}

func (an *Analyser) lookaheadIs(kinds ...string) bool {
	kind := an.lookahead.Kind

	for _, k := range kinds {
		if kind == k {
			return true
		}
	}

	return false
}

func (an *Analyser) parseModuleDec(tok *Token) *Ast {
	name := an.mustAdvanceExpect("name")
	an.mustAdvanceExpect("semicolon", "newline")

	return &Ast{Kind: "module_dec", Data: []*Token{name}}
}

func (an *Analyser) Parse() *Ast {
	nxt, err := an.advance()

	if err != nil {
		panic(err)
	}

	switch nxt.Kind {
	case "name":
		if isKeyWord(string(nxt.Lexeme)) {
			return an.parseModuleDec(nxt)
		}
	}

	panic(an.errf(nxt, "unable to parse"))
}
