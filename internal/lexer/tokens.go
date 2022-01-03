package lexer

type TokenType int

const (
	CommentSL TokenType = iota
	CommentML
	String
	Integer
	Identifier
	ArithOp
	LogicOp
)

type Token struct {
	kind            TokenType
	data            [][]rune
	rpos, line, col int
}
