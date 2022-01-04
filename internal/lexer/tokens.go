package lexer

import (
	"fmt"
	"regexp"
	"strings"
)

type TokenKind int8

const (
	Invalid   TokenKind = -1
	CommentSL TokenKind = iota
	CommentML
	Whitespace
	String
	Hexadecimal
	Decimal
	Boolean
	Identifier
	ArithOp
	LogicOp
	ComparisonOp
	Assign
	AugAssign
)

type Token struct {
	kind            TokenKind
	data            []rune
	rpos, line, col int
}

func (t *Token) String() string {
	return fmt.Sprintf("<Token: {%d, %#v, %d, %d, %d}>", t.kind, string(t.data), t.rpos, t.line, t.col)
}

type patternEntry struct {
	kind    TokenKind
	pattern string
}

var patterns = map[string]patternEntry{
	"bool":       {Boolean, "true|false"},
	"assign":     {Assign, "="},
	"augassign":  {AugAssign, `\+=|-=|\*=|/\*|\*\*=|%=`},
	"arith":      {ArithOp, `\+|-|\*|/|\*\*|%`},
	"logic":      {LogicOp, `!|&&|\|\|`},
	"compare":    {ComparisonOp, "==|!=|<|>|<=|>="},
	"hex":        {Hexadecimal, "(0x)[0-9a-fA-F]+"},
	"dec":        {Decimal, `(\.[0-9]+)|\b([0-9]+((?:\.[0-9]+)?[eE][-+]?[0-9]+)?)\b`},
	"identifier": {Identifier, "[a-zA-Z_]\\w*"},
	"ws":         {Whitespace, "\\s+"},
}

var compiled *regexp.Regexp

func CompilePattern() *regexp.Regexp {
	if compiled != nil {
		return compiled
	}

	var builder strings.Builder
	builder.WriteRune('^')
	addOr := false

	for k, v := range patterns {
		if addOr {
			builder.WriteRune('|')
		} else {
			addOr = true
		}
		fmt.Fprintf(&builder, "(?P<%s>%s)", k, v.pattern)
	}

	return regexp.MustCompile(builder.String())
}

func Kind(k string) TokenKind {
	entry, ok := patterns[k]

	if !ok {
		return Invalid
	}

	return entry.kind
}
