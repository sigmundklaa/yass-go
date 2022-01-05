package patterns

import (
	"fmt"
	"regexp"
	"strings"
)

// This pattern are all regex patterns, so when adding an entry make sure it is properly escaped
var metapattern = map[string]string{
	"name":         `[a-zA-Z_]\w*`,
	"assign":       ":",
	"regex":        `/(?:[^\\/]|[\\](?:[\\]{2})*/)*/`,
	"string":       `(?:[^"\\]|[\\](?:[\\]{2})*[^\"])*`,
	"sqbrac_open":  "\\[",
	"sqbrac_close": "\\]",
	"paran_open":   "\\(",
	"paran_close":  "\\)",
}

func compile(pattern map[string]string) *regexp.Regexp {
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

type Raw map[string]struct {
	pattern string
	isRegex bool
}

func Construct(pattern Raw) *regexp.Regexp {
	var n = map[string]string{}

	for k, v := range pattern {
		if v.isRegex {
			n[k] = v.pattern
		} else {
			// Escape regex meta characters, as we want to match this string literally
			n[k] = regexp.QuoteMeta(v.pattern)
		}
	}

	return compile(n)
}

func Meta() *regexp.Regexp {
	return compile(metapattern)
}
