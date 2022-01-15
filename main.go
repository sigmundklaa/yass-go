package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/SigJig/yass-go/internal/analyser"
)

func testLex() {
	pattern := analyser.DefaultPattern()
	f, err := os.Open("examples.githide/api/api.yass")

	if err != nil {
		panic(err)
	}
	defer f.Close()

	reader := bufio.NewReader(f)
	lexer := analyser.NewLexer(reader, pattern, -1)

	for tok, err := lexer.NextToken(); ; tok, err = lexer.NextToken() {
		if err != nil {
			panic(err)
		}

		if tok.Kind == "EOF" {
			break
		}

		if tok.Kind == "space_no_nl" {
			continue
		}

		fmt.Println(tok)
	}

}

func testParse() {
	//fmt.Println(analyser.TestStates())
}

func main() {
	testLex()
}
