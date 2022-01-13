package analyser

import (
	"fmt"
	"strings"
)

// Pattern for the grammar format
// This pattern are all regex patterns, so when adding an entry make sure it is properly escaped
var Metapattern = map[string]string{
	"name":          `[a-zA-Z_]\w*`,
	"assign":        ":",
	"regex":         `/(?:[^\\/]|[\\](?:[\\]{2})*/|[\\][^/])*/`,
	"string":        `"(?:[^"\\]|[\\](?:[\\]{2})*[^\"])*"`,
	"sqbrac_open":   "\\[",
	"sqbrac_close":  "\\]",
	"paran_open":    "\\(",
	"paran_close":   "\\)",
	"question_mark": "\\?",
	"union":         "\\|",
	"line_cont":     `\\`,
	"newline":       "\n+",
	"ignore":        `(?:[^\S\r\n]+)|(?:/\*(?:[^\*]|\*[^/])*(?:\*/|$))|(?://[^\n]*\n)`,
}

var metaprod = map[string][]string{
	"$start": {
		"$prod*",
	},
	"$prod": {
		"name assign $expr newline",
	},
	"$expr": {
		"paran_open $expr paran_close",
		"$expr union $expr",
		"$expr line_cont newline $expr",
		"$opt_ext",
		"regex",
		"string",
		"$expr+",
	},
	"$opt_ext": {
		"sqbrac_open $expr sqbrac_close",
		"$expr question_mark",
	},
}

func createRule(symname string) symbol {
	var flag prodFlag

	for i := len(symname) - 1; i >= 0; i-- {
		cont := func() bool {
			for idx, r := range flagChars {
				if symname[i] == r {
					flag |= 1 << idx
					return true
				}
			}

			return false
		}()

		if !cont {
			return symbol{symname[:i+1], symname[0] != '$', flag}
		}
	}

	panic(fmt.Errorf("bad rule %s", symname))
}

func createMetaRules(raw string) (rules []symbol) {
	for _, sym := range strings.Split(raw, " ") {
		rules = append(rules, createRule(sym))
	}
	return
}
