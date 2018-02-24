package math

import (
	"strings"

	luamapper "github.com/corpix/lua/mapper"
	lua "github.com/yuin/gopher-lua"
)

var (
	fns = map[string]lua.LGFunction{
		"Compare":        luamapper.MustToGFunction(strings.Compare),
		"Contains":       luamapper.MustToGFunction(strings.Contains),
		"ContainsAny":    luamapper.MustToGFunction(strings.ContainsAny),
		"ContainsRune":   luamapper.MustToGFunction(strings.ContainsRune),
		"Count":          luamapper.MustToGFunction(strings.Count),
		"EqualFold":      luamapper.MustToGFunction(strings.EqualFold),
		"Fields":         luamapper.MustToGFunction(strings.Fields),
		"FieldsFunc":     luamapper.MustToGFunction(strings.FieldsFunc),
		"HasPrefix":      luamapper.MustToGFunction(strings.HasPrefix),
		"HasSuffix":      luamapper.MustToGFunction(strings.HasSuffix),
		"Index":          luamapper.MustToGFunction(strings.Index),
		"IndexAny":       luamapper.MustToGFunction(strings.IndexAny),
		"IndexByte":      luamapper.MustToGFunction(strings.IndexByte),
		"IndexFunc":      luamapper.MustToGFunction(strings.IndexFunc),
		"IndexRune":      luamapper.MustToGFunction(strings.IndexRune),
		"Join":           luamapper.MustToGFunction(strings.Join),
		"LastIndex":      luamapper.MustToGFunction(strings.LastIndex),
		"LastIndexAny":   luamapper.MustToGFunction(strings.LastIndexAny),
		"LastIndexByte":  luamapper.MustToGFunction(strings.LastIndexByte),
		"LastIndexFunc":  luamapper.MustToGFunction(strings.LastIndexFunc),
		"Map":            luamapper.MustToGFunction(strings.Map),
		"Repeat":         luamapper.MustToGFunction(strings.Repeat),
		"Replace":        luamapper.MustToGFunction(strings.Replace),
		"Split":          luamapper.MustToGFunction(strings.Split),
		"SplitAfter":     luamapper.MustToGFunction(strings.SplitAfter),
		"SplitAfterN":    luamapper.MustToGFunction(strings.SplitAfterN),
		"SplitN":         luamapper.MustToGFunction(strings.SplitN),
		"Title":          luamapper.MustToGFunction(strings.Title),
		"ToLower":        luamapper.MustToGFunction(strings.ToLower),
		"ToLowerSpecial": luamapper.MustToGFunction(strings.ToLowerSpecial),
		"ToTitle":        luamapper.MustToGFunction(strings.ToTitle),
		"ToTitleSpecial": luamapper.MustToGFunction(strings.ToTitleSpecial),
		"ToUpper":        luamapper.MustToGFunction(strings.ToUpper),
		"ToUpperSpecial": luamapper.MustToGFunction(strings.ToUpperSpecial),
		"Trim":           luamapper.MustToGFunction(strings.Trim),
		"TrimFunc":       luamapper.MustToGFunction(strings.TrimFunc),
		"TrimLeft":       luamapper.MustToGFunction(strings.TrimLeft),
		"TrimLeftFunc":   luamapper.MustToGFunction(strings.TrimLeftFunc),
		"TrimPrefix":     luamapper.MustToGFunction(strings.TrimPrefix),
		"TrimRight":      luamapper.MustToGFunction(strings.TrimRight),
		"TrimRightFunc":  luamapper.MustToGFunction(strings.TrimRightFunc),
		"TrimSpace":      luamapper.MustToGFunction(strings.TrimSpace),
		"TrimSuffix":     luamapper.MustToGFunction(strings.TrimSuffix),
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
