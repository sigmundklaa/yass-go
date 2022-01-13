package analyser

type Analyser struct {
}

type Ast struct {
}

func newAnalyser() *Analyser {
	return &Analyser{}
}

func (an *Analyser) parse() *Ast {
	return &Ast{}
}
