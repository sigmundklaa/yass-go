package lexer

import (
	"fmt"
	"io"
	"regexp"
)

const (
	DEFAULT_RUNEBUF_SIZE = 10000
)

type Token struct {
	Kind      string
	Lexeme    []rune
	Line, Col uint
}

func (t *Token) String() string {
	return fmt.Sprintf("<Token: {%s, %#v, line %d, column %d}>", t.Kind, string(t.Lexeme), t.Line, t.Col)
}

type Lexer struct {
	buf       []rune
	reader    io.RuneReader
	channel   chan *Token
	reg       *regexp.Regexp
	eof       bool
	bufSize   uint
	bufpos    uint
	line, col uint
}

func NewLexer(reader io.RuneReader, pattern *regexp.Regexp, bufSize int) *Lexer {
	if bufSize < 0 {
		bufSize = DEFAULT_RUNEBUF_SIZE
	}

	l := &Lexer{
		make([]rune, 0),
		reader,
		nil,
		pattern,
		false,
		uint(bufSize),
		0,
		1,
		1,
	}

	if err := l.fillBuf(uint(bufSize)); err != nil {
		panic(err)
	}

	return l
}

func (l *Lexer) fillBuf(length uint) error {
	if l.eof {
		return nil
	}

	if max := l.bufSize - uint(len(l.buf)); length > max {
		return l.errf("can not add %d elements to buffer, maximum: %d", length, max)
	}

	for i := uint(0); i < length; i++ {
		r, _, err := l.reader.ReadRune()

		if err != nil {
			if err == io.EOF {
				l.eof = true

				return nil
			}
			return err
		}

		l.buf = append(l.buf, r)
	}

	return nil
}

func (l *Lexer) shiftBuf(length uint) error {
	if length <= 0 {
		panic(l.errf("attempted to shift negative value: %d", length))
	}

	l.buf = l.buf[length:]

	if length > l.bufpos {
		panic(l.errf("attempted shift with length %d, can only max shift %d", length, l.bufpos))
	}
	l.bufpos -= length

	return l.fillBuf(length)
}

func (l *Lexer) NextToken() (*Token, error) {
	lexeme, key, err := l.nextLexeme()

	if err != nil {
		if err == io.EOF {
			return l.createToken("EOF", nil)
		}

		return nil, err
	}

	tok, err := l.createToken(key, []rune(lexeme))

	if err != nil {
		panic(err)
	}

	l.advanceWith(tok)
	l.shiftBuf(uint(len(tok.Lexeme)))

	return tok, nil
}

func (l *Lexer) nextLexeme() ([]rune, string, error) {
	sub := l.buf[l.bufpos:]

	if len(sub) == 0 {
		// Assuming that the buffer has been properly initalized and maintained, reaching its end
		// will mean EOF
		return nil, "", io.EOF
	}

	matches := l.reg.FindStringSubmatch(string(sub))

	if matches != nil {
		for idx, name := range l.reg.SubexpNames()[1:] {
			lexeme := []rune(matches[idx+1])

			if len(lexeme) > 0 {
				return lexeme, name, nil
			}
		}

		panic(l.errf("unnamed submatch found at: %s", matches[0]))
	}

	return nil, "", l.errf("unrecognized character(s) at: %#v", string(sub[:5]))
}

func (l *Lexer) advanceWith(tok *Token) {
	length := len(tok.Lexeme)

	for _, r := range tok.Lexeme {
		if r == '\n' {
			l.line += 1
			l.col = 1
		} else {
			l.col += 1
		}
	}

	l.bufpos += uint(length)
}

func (l *Lexer) errf(fmtstr string, args ...interface{}) error {
	return fmt.Errorf("lexer error at line %d, column %d: %s", l.line, l.col, fmt.Sprintf(fmtstr, args...))
}

func (l *Lexer) createToken(kind string, lexeme []rune) (*Token, error) {
	return &Token{kind, lexeme, l.line, l.col}, nil
}
