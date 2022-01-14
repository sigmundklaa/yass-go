package analyser

import "fmt"

type ActionKind int

const (
	SHIFT ActionKind = iota
	REDUCE
	GOTO
)

type Action struct {
	kind ActionKind
	st   *state
}

type state struct {
	kernel   []lalrItem
	complete stringset
	trans    map[string]*proxyState // Double pointer, so we can change the pointer after assignment to another state with an equal kernel
	table    map[string]Action
}

type proxyState struct {
	key string
	st  *state
}

func newProxy(key string) *proxyState {
	return &proxyState{
		key: key,
		st: &state{
			kernel:   nil,
			complete: make(stringset),
			trans:    make(map[string]*proxyState),
			table:    make(map[string]Action),
		},
	}
}

func (st *state) transition(symname string) *proxyState {
	trprox := st.trans[symname]

	if trprox == nil {
		trprox = newProxy(symname)
		st.trans[symname] = trprox
	}

	return trprox
}

func (ps *proxyState) replace(s *state) {
	ps.st = s
}

func (ps *proxyState) state() *state {
	return ps.st
}

func (ps *proxyState) String() string {
	return fmt.Sprintf("<proxyState: %s>", ps.key)
}
