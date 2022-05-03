package analyser

import "io"

type Generator struct {
	parser *Analyser
}

func NewGenerator(reader io.RuneReader) *Generator {
	return &Generator{
		parser: NewAnalyser(reader),
	}
}

func (gen *Generator) Generate() {

}
