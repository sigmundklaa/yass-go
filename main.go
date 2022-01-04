package main

import (
	"fmt"

	"github.com/SigJig/yass-go/internal/lexer"
)

func main() {
	lexer := lexer.NewLexer([]rune(`true 291


    name false

	18`), nil)

	for tok := range lexer.InitStream(-1) {
		fmt.Println(tok)
	}
}
