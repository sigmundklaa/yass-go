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

func (l *Lexer) Stream(bufSize int) (chan *Token, chan error) {
	if l.channel != nil {
		panic(l.errf("attempted to initialize stream, already initialized"))
	}

	if bufSize < 0 {
		bufSize = DEFAULT_CHANBUF_SIZE
	}

	l.channel = make(chan *Token, bufSize)
	errchan := make(chan error)

	go func() {
		for {
			tok, err := l.nextToken()

			if err != nil {
				if err != io.EOF {
					errchan <- err
				}

				close(l.channel)
				close(errchan)
				return
			}

			l.channel <- tok
		}
	}()

	return l.channel, errchan
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

func (l *Lexer) nextLexeme() ([]rune, int, error) {
	sub := l.buf[l.bufpos:]

	if len(sub) == 0 {
		// Assuming that the buffer has been properly initalized and maintained, reaching its end
		// will mean EOF
		return nil, -1, io.EOF
	}

	matches := l.reg.FindStringSubmatch(string(sub))

	if matches != nil {
		for idx, m := range matches[1:] {
			lexeme := []rune(m)

			if len(lexeme) > 0 {
				return lexeme, idx, nil
			}
		}

		panic(l.errf("unnamed submatch found at: %s", matches[0]))
	}

	return nil, -1, l.errf("unable to process character at: %#v", string(sub))
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

func (l *Lexer) errf(fmtstr string, args ...interface{}) error {
	return fmt.Errorf("Lexer error at line %d, column %d: %s", l.line, l.col, fmt.Sprintf(fmtstr, args...))
}

func (l *Lexer) createToken(kind TokenKind, lexeme []rune) (*Token, error) {
	return &Token{kind, lexeme, l.line, l.col}, nil
}
