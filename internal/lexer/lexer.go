package lexer

import (
	"fmt"
	"io"
	"regexp"
)

const DEFAULT_CHAN_BUF_SIZE = 0

type Lexer struct {
	data            []rune
	channel         chan *Token
	reg             *regexp.Regexp
	rpos, line, col int
}

func NewLexer(s []rune, pattern *regexp.Regexp) *Lexer {
	if pattern == nil {
		pattern = CompilePattern()
	}

	l := &Lexer{s, nil, pattern, 0, 1, 1}

	return l
}

func (l *Lexer) InitStream(bufSize int) chan *Token {
	if l.channel != nil {
		// TODO: Error / warning
		return l.channel
	}

	if bufSize < 0 {
		bufSize = DEFAULT_CHAN_BUF_SIZE
	}

	l.channel = make(chan *Token, bufSize)

	go func() {
		for {
			tok, err := l.nextToken()

			if err != nil {
				if err == io.EOF {
					close(l.channel)
					return
				} else {
					panic(err)
				}
			}

			l.channel <- tok
		}
	}()

	return l.channel
}

func (l *Lexer) nextToken() (*Token, error) {
	value, key, err := l.nextValue()

	if err != nil {
		return nil, err
	}

	tok, err := l.createToken(Kind(key), []rune(value))

	if err != nil {
		panic(err)
	}

	l.advanceWith(tok)

	return tok, nil
}

func (l *Lexer) nextValue() ([]rune, string, error) {
	sub := l.data[l.rpos:]

	if len(sub) == 0 {
		return nil, "", io.EOF
	}

	matches := l.reg.FindStringSubmatch(string(sub))

	for idx, key := range l.reg.SubexpNames()[1:] {
		item := []rune(matches[idx+1])

		if len(item) > 0 {
			return item, key, nil
		}
	}

	return nil, "", fmt.Errorf("unexpected character: %v", sub)
}

func (l *Lexer) advanceWith(tok *Token) {
	length := len(tok.data)

	for _, r := range tok.data {
		if r == '\n' {
			l.line += 1
			l.col = 0
		} else {
			l.col += 1
		}
	}

	l.rpos += length
}

func (l *Lexer) createToken(kind TokenKind, data []rune) (*Token, error) {
	return &Token{kind, data, l.rpos, l.line, l.col}, nil
}
