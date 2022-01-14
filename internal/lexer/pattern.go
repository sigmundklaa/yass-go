package lexer

import (
	"fmt"
	"regexp"
	"strings"
)

var defaultPattern = map[string]string{
	"name":         `[a-zA-Z_]\w*`,
	"assign":       "=",
	"string":       `"(?:[^"\\]|[\\](?:[\\]{2})*[^\"])*"`,
	"sqbrac_open":  "\\[",
	"sqbrac_close": "\\]",
	"paran_open":   "\\(",
	"paran_close":  "\\)",
	"curl_open":    "\\{",
	"curl_close":   "\\}",
	"newline":      "\n+",
	"space_no_nl":  `(?:[^\S\r\n]+)`,
	"comment_ml":   `(?:/\*(?:[^\*]|\*[^/])*(?:\*/|$))`,
	"comment_sl":   `(?://[^\n]*\n)`,
}

func CompilePattern(pattern map[string]string) *regexp.Regexp {
	var builder strings.Builder
	builder.WriteString("^(?:")
	addOr := false

	for name, pattern := range pattern {
		if addOr {
			builder.WriteRune('|')
		} else {
			addOr = true
		}
		fmt.Fprintf(&builder, "(?P<%s>%s)", name, pattern)
	}
	builder.WriteRune(')')

	return regexp.MustCompile(builder.String())
}

func DefaultPattern() *regexp.Regexp {
	return CompilePattern(defaultPattern)
}
