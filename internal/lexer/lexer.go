package lexer

import (
	"fmt"
	"io"
	"regexp"
)

const (
	DEFAULT_CHANBUF_SIZE = 0
	DEFAULT_RUNEBUF_SIZE = 100
)

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
	if pattern == nil {
		pattern = CompilePattern()
	}

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

func (l *Lexer) Stream(bufSize int) chan *Token {
	if l.channel != nil {
		// TODO: Error / warning
		return l.channel
	}

	if bufSize < 0 {
		bufSize = DEFAULT_CHANBUF_SIZE
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

func (l *Lexer) fillBuf(length uint) error {
	if l.eof {
		return nil
	}

	if max := l.bufSize - uint(len(l.buf)); length > max {
		return fmt.Errorf("can not add %d elements to buffer, maximum: %d", length, max)
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
		panic(fmt.Errorf("attempted to shift negative value: %d", length))
	}

	l.buf = l.buf[length:]

	if length > l.bufpos {
		panic(fmt.Errorf("attempted shift with length %d, can only max shift %d", length, l.bufpos))
	}
	l.bufpos -= length

	return l.fillBuf(length)
}

func (l *Lexer) nextToken() (*Token, error) {
	lexeme, key, err := l.nextLexeme()

	if err != nil {
		return nil, err
	}

	tok, err := l.createToken(Kind(key), []rune(lexeme))

	if err != nil {
		panic(err)
	}

	l.advanceWith(tok)
	l.shiftBuf(uint(len(tok.lexeme)))

	return tok, nil
}

func (l *Lexer) nextLexeme() ([]rune, string, error) {
	sub := l.buf[l.bufpos:]

	if len(sub) == 0 {
		return nil, "", io.EOF
	}

	matches := l.reg.FindStringSubmatch(string(sub))

	if matches != nil {
		for idx, key := range l.reg.SubexpNames()[1:] {
			lexeme := []rune(matches[idx+1])

			if len(lexeme) > 0 {
				return lexeme, key, nil
			}
		}

		panic(fmt.Errorf("unnamed submatch found at: %s", matches[0]))
	}

	return nil, "", fmt.Errorf("unexpected character: %v", sub)
}

func (l *Lexer) advanceWith(tok *Token) {
	length := len(tok.lexeme)

	for _, r := range tok.lexeme {
		if r == '\n' {
			l.line += 1
			l.col = 1
		} else {
			l.col += 1
		}
	}

	l.bufpos += uint(length)
}

func (l *Lexer) createToken(kind TokenKind, lexeme []rune) (*Token, error) {
	return &Token{kind, lexeme, l.line, l.col}, nil
}
