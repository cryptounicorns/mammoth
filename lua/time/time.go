package time

import (
	"time"

	luamapper "github.com/corpix/lua/mapper"
	lua "github.com/yuin/gopher-lua"
)

var (
	fns = map[string]lua.LGFunction{
		"Now":           luamapper.MustToGFunction(time.Now),
		"Sleep":         luamapper.MustToGFunction(time.Sleep),
		"ParseDuration": luamapper.MustToGFunction(time.ParseDuration),
		"Since":         luamapper.MustToGFunction(time.Since),
		"Until":         luamapper.MustToGFunction(time.Until),
	}

	vars = map[string]lua.LValue{
		"Nanosecond":  luamapper.MustToValue(time.Nanosecond),
		"Microsecond": luamapper.MustToValue(time.Microsecond),
		"Millisecond": luamapper.MustToValue(time.Millisecond),
		"Second":      luamapper.MustToValue(time.Second),
		"Minute":      luamapper.MustToValue(time.Minute),
		"Hour":        luamapper.MustToValue(time.Hour),
	}
)

func Loader(l *lua.LState) int {
	mod := l.SetFuncs(l.NewTable(), fns)

	for k, v := range vars {
		l.SetField(mod, k, v)
	}

	l.Push(mod)

	return 1
}
