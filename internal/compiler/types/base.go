package types

type TypeIface interface {
}

type StmtIface interface {
}

type Symbol interface {
}

type Container interface {
	Add(stmt StmtIface)
}

func IsMetaType(sym Symbol) bool {
	var x interface{} = sym

	_, ok := x.(TypeIface)

	return ok
}
