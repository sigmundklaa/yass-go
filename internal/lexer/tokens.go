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

var names = map[TokenKind]string{
	Invalid:      "Invalid",
	CommentSL:    "CommentSL",
	CommentML:    "CommentML",
	Whitespace:   "Whitespace",
	String:       "String",
	Hexadecimal:  "Hexadecimal",
	Decimal:      "Decimal",
	Boolean:      "Boolean",
	Identifier:   "Identifier",
	ArithOp:      "ArithOp",
	LogicOp:      "LogicOp",
	ComparisonOp: "ComparisonOp",
	Assign:       "Assign",
	AugAssign:    "AugAssign",
}

type Token struct {
	kind      TokenKind
	lexeme    []rune
	line, col uint
}

func (t *Token) String() string {
	return fmt.Sprintf("<Token: {%s, %#v, line %d, column %d}>", t.kind, string(t.lexeme), t.line, t.col)
}

func (kind TokenKind) String() string {
	return names[kind]
}

type patternEntry struct {
	kind    TokenKind
	pattern string
}

// When adding an entry, make sure the pattern does not contain capturing groups
// (can be avoided by placing ?: at begging capturing group, example: /(?:[0-9]+)/)
var patterns = []patternEntry{
	{CommentSL, `//[^\n]*\n`},
	{CommentML, `/\*(?:[^\*]|\*[^/])*\*/`},
	{Boolean, "true|false"},
	{Assign, "="},
	{AugAssign, `\+=|-=|\*=|\*\*=|%=`},
	{ArithOp, `\+|-|\*|/|\*\*|%`},
	{LogicOp, `!|&&|\|\|`},
	{ComparisonOp, "==|!=|<|>|<=|>="},
	{Hexadecimal, "0x[0-9a-fA-F]+"},
	{Decimal, `\.[0-9]+|\b(?:[0-9]+(?:(?:\.[0-9]+)?[eE][-+]?[0-9]+)?)\b`},
	{Identifier, "[a-zA-Z_]\\w*"},
	{Whitespace, "\\s+"},
}

var compiled *regexp.Regexp

func CompilePattern() *regexp.Regexp {
	if compiled != nil {
		return compiled
	}

	var builder strings.Builder
	builder.WriteRune('^')
	addOr := false

	for _, entry := range patterns {
		if addOr {
			builder.WriteRune('|')
		} else {
			addOr = true
		}
		fmt.Fprintf(&builder, "(%s)", entry.pattern)
	}

	return regexp.MustCompile(builder.String())
}

func Kind(idx int) TokenKind {
	return patterns[idx].kind
}
