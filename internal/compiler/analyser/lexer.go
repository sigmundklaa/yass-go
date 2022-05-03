package analyser

import (
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"

	"github.com/SigJig/yass-go/internal/compiler/errhandler"
)

type LexKind int

const (
	INVALID LexKind = iota
	NAME
	SEMICOLON
	COLON
	ASSIGN
	PLUS
	MINUS
	SLASH
	STAR
	PERCENTAGE
	COMMA
	STRING
	NUMBER
	PERIOD
	SQBRAC_OPEN
	SQBRAC_CLOSE
	PARAN_OPEN
	PARAN_CLOSE
	CURL_OPEN
	CURL_CLOSE
	NEWLINE
	SPACE_NO_NL
	COMMENT_ML
	COMMENT_SL
	EOF
)

var kindStrings = map[LexKind]string{
	INVALID:      "INVALID",
	NAME:         "NAME",
	SEMICOLON:    "SEMICOLON",
	COLON:        "COLON",
	PLUS:         "PLUS",
	MINUS:        "MINUS",
	SLASH:        "SLASH",
	STAR:         "STAR",
	PERCENTAGE:   "PERCENTAGE",
	ASSIGN:       "ASSIGN",
	PERIOD:       "PERIOD",
	COMMA:        "COMMA",
	STRING:       "STRING",
	NUMBER:       "NUMBER",
	SQBRAC_OPEN:  "SQBRAC_OPEN",
	SQBRAC_CLOSE: "SQBRAC_CLOSE",
	PARAN_OPEN:   "PARAN_OPEN",
	PARAN_CLOSE:  "PARAN_CLOSE",
	CURL_OPEN:    "CURL_OPEN",
	CURL_CLOSE:   "CURL_CLOSE",
	NEWLINE:      "NEWLINE",
	SPACE_NO_NL:  "SPACE_NO_NL",
	COMMENT_ML:   "COMMENT_ML",
	COMMENT_SL:   "COMMENT_SL",
	EOF:          "EOF",
}

func (kind LexKind) String() string {
	return kindStrings[kind]
}

func lexKindsJoin(kinds ...LexKind) string {
	var builder strings.Builder

	if len(kinds) > 0 {
		fmt.Fprint(&builder, kinds[0].String())
	}

	for _, kind := range kinds[1:] {
		fmt.Fprintf(&builder, "%s,", kind.String())
	}

	return builder.String()
}

// TODO: Convert strings to numbered constants
var defaultPattern = map[LexKind]string{
	NAME:         `[a-zA-Z_]\w*`,
	SEMICOLON:    ";",
	COLON:        ":",
	ASSIGN:       "=",
	PLUS:         "\\+",
	MINUS:        "-",
	SLASH:        "\\/",
	STAR:         "\\*",
	PERCENTAGE:   "%",
	COMMA:        ",",
	PERIOD:       "\\.",
	STRING:       `"(?:[^"\\]|[\\](?:[\\]{2})*[^\"])*"`,
	NUMBER:       `\d+`,
	SQBRAC_OPEN:  "\\[",
	SQBRAC_CLOSE: "\\]",
	PARAN_OPEN:   "\\(",
	PARAN_CLOSE:  "\\)",
	CURL_OPEN:    "\\{",
	CURL_CLOSE:   "\\}",
	NEWLINE:      "\n+",
	SPACE_NO_NL:  `(?:[^\S\r\n]+)`,
	COMMENT_ML:   `(?:\/\*(?:[^\*]|\*[^\/])*(?:\*\/|$))`,
	COMMENT_SL:   `(?://[^\n]*\n)`,
}

func CompilePattern(pattern map[LexKind]string) *regexp.Regexp {
	var builder strings.Builder
	builder.WriteString("^(?m)(?:")
	addOr := false

	for kind, pattern := range pattern {
		if addOr {
			builder.WriteRune('|')
		} else {
			addOr = true
		}
		fmt.Fprintf(&builder, "(?P<%s>%s)", strconv.Itoa(int(kind)), pattern)
	}
	builder.WriteRune(')')

	return regexp.MustCompile(builder.String())
}

func DefaultPattern() *regexp.Regexp {
	return CompilePattern(defaultPattern)
}

const (
	DEFAULT_RUNEBUF_SIZE = 10000
)

type Token struct {
	Kind      LexKind
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

func DefaultLexer(reader io.RuneReader) *Lexer {
	return NewLexer(reader, DefaultPattern(), -1)
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
			return l.createToken(EOF, nil)
		}

		return nil, err
	}

	tok, err := l.createToken(key, lexeme)

	if err != nil {
		panic(err)
	}

	l.advanceWith(tok)
	l.shiftBuf(uint(len(tok.Lexeme)))

	return tok, nil
}

func (l *Lexer) nextLexeme() ([]rune, LexKind, error) {
	sub := l.buf[l.bufpos:]

	if len(sub) == 0 {
		// Assuming that the buffer has been properly initalized and maintained, reaching its end
		// will mean EOF
		return nil, INVALID, io.EOF
	}

	matches := l.reg.FindStringSubmatch(string(sub))

	if matches != nil {
		for idx, name := range l.reg.SubexpNames()[1:] {
			lexeme := []rune(matches[idx+1])

			if len(lexeme) > 0 {
				kind, err := strconv.Atoi(name)

				if err != nil {
					panic(err)
				}

				return lexeme, LexKind(kind), nil
			}
		}

		panic(l.errf("unnamed submatch found at: %s", matches[0]))
	}

	return nil, INVALID, l.errf("unrecognized character(s) at: %#v", string(sub[:5]))
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
	return errhandler.Err("lexer", fmt.Sprintf(fmtstr, args...), l.line, l.col)
}

func (l *Lexer) createToken(kind LexKind, lexeme []rune) (*Token, error) {
	return &Token{kind, lexeme, l.line, l.col}, nil
}
