package types

type TypeIface interface {
}

type StmtIface interface {
}

type Symbol interface {
}

func IsMetaType(sym Symbol) bool {
	var x interface{} = sym

	_, ok := x.(TypeIface)

	return ok
}
