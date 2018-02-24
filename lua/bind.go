package lua

import (
	lua "github.com/yuin/gopher-lua"

	math "github.com/cryptounicorns/mammoth/lua/math"
	spew "github.com/cryptounicorns/mammoth/lua/spew"
	strings "github.com/cryptounicorns/mammoth/lua/strings"
	time "github.com/cryptounicorns/mammoth/lua/time"
	validator "github.com/cryptounicorns/mammoth/lua/validator"
)

func Bind(l *lua.LState) *lua.LState {
	l.PreloadModule("gospew", spew.Loader)
	l.PreloadModule("gomath", math.Loader)
	l.PreloadModule("gostrings", strings.Loader)
	l.PreloadModule("gotime", time.Loader)
	l.PreloadModule("govalidator", validator.Loader)

	return l
}
