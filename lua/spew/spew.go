package spew

import (
	luamapper "github.com/corpix/lua/mapper"
	"github.com/davecgh/go-spew/spew"
	lua "github.com/yuin/gopher-lua"
)

var (
	fns = map[string]lua.LGFunction{
		"Dump": luamapper.MustToGFunction(spew.Dump),
	}

	vars = map[string]lua.LValue{}
)

func Loader(l *lua.LState) int {
	mod := l.SetFuncs(l.NewTable(), fns)

	for k, v := range vars {
		l.SetField(mod, k, v)
	}

	l.Push(mod)

	return 1
}
