package main

import (
	"bufio"
	"fmt"
	"strings"

	"github.com/SigJig/yass-go/internal/lexer"
)

func main() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
		}
	}()
	reader := bufio.NewReader(strings.NewReader(`true 291

	// this is a single line comment
	number 3 /* end of line comment

	spanning multiples lines
	
    */name false
string "this is a fat st"ring boss"

	18`))
	lexer := lexer.NewLexer(reader, nil, -1)
	ch, ech := lexer.Stream(-1)

	for {
		select {
		case tok, ok := <-ch:
			if !ok {
				return
			}
			fmt.Println(tok)
		case err := <-ech:
			fmt.Println(err)

			return
		}
	}
}
