package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/SigJig/yass-go/internal/compiler/analyser"
)

func testLex() {
	pattern := analyser.DefaultPattern()
	f, err := os.Open("examples/one.ys")

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

		if tok.Kind == analyser.EOF {
			break
		}

		if tok.Kind == analyser.SPACE_NO_NL {
			continue
		}

		fmt.Println(tok)
	}

}

func printIndent(indent int) {
	for i := 0; i < indent; i++ {
		fmt.Printf(" ")
	}
}

func printAst(ast *analyser.AstNode, indent int) {
	printIndent(indent)
	fmt.Printf("%s\n", ast.Kind.String())

	for _, v := range ast.Children {
		printAst(v, indent+4)
	}
}

func testParse() {
	f, err := os.Open("examples/one.ys")

	if err != nil {
		panic(err)
	}
	defer f.Close()

	reader := bufio.NewReader(f)
	an := analyser.NewAnalyser(reader)

	for _, v := range an.Parse()[:1] {
		// fmt.Printf("%s\n", v.String())
		printAst(v, 0)
	}
}

func main() {
	testParse()
}
