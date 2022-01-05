package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/SigJig/yass-go/internal/lexer"
	"github.com/SigJig/yass-go/internal/patterns"
)

func main() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
		}
	}()
	pattern := patterns.Meta()
	f, err := os.Open("internal/parser/grammar")

	if err != nil {
		panic(err)
	}
	defer f.Close()

	reader := bufio.NewReader(f)
	lexer := lexer.NewLexer(reader, pattern, -1)
	ch, ech := lexer.Stream(-1)

	for {
		select {
		case tok, ok := <-ch:
			if !ok {
				return
			}

			if tok.Kind != "ignore" {
				fmt.Println(tok)
			}
		case err := <-ech:
			fmt.Println(err)

			return
		}
	}
}
