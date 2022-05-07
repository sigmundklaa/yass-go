package types

type Pack struct {
}

type Unpack struct {
}

type Assignment struct {
	Left  *Unpack
	Right *Pack
}

type VarDec struct {
	Type   TypeIface
	Assign *Assignment
}
