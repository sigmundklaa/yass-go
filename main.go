package main

import (
	"bufio"
	"fmt"
	"strings"

	"github.com/SigJig/yass-go/internal/lexer"
)

func main() {
	reader := bufio.NewReader(strings.NewReader(`true 291

	// this is a single line comment
	number 3 /* end of line comment

	spanning multiples lines

    */name false

	18`))
	lexer := lexer.NewLexer(reader, nil, -1)

	for tok := range lexer.Stream(-1) {
		fmt.Println(tok)
	}
}
