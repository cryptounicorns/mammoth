package math

import (
	"math"

	luamapper "github.com/corpix/lua/mapper"
	lua "github.com/yuin/gopher-lua"
)

var (
	fns = map[string]lua.LGFunction{
		"Abs":             luamapper.MustToGFunction(math.Abs),
		"Acos":            luamapper.MustToGFunction(math.Acos),
		"Acosh":           luamapper.MustToGFunction(math.Acosh),
		"Asin":            luamapper.MustToGFunction(math.Asin),
		"Asinh":           luamapper.MustToGFunction(math.Asinh),
		"Atan":            luamapper.MustToGFunction(math.Atan),
		"Atan2":           luamapper.MustToGFunction(math.Atan2),
		"Atanh":           luamapper.MustToGFunction(math.Atanh),
		"Cbrt":            luamapper.MustToGFunction(math.Cbrt),
		"Ceil":            luamapper.MustToGFunction(math.Ceil),
		"Copysign":        luamapper.MustToGFunction(math.Copysign),
		"Cos":             luamapper.MustToGFunction(math.Cos),
		"Cosh":            luamapper.MustToGFunction(math.Cosh),
		"Dim":             luamapper.MustToGFunction(math.Dim),
		"Erf":             luamapper.MustToGFunction(math.Erf),
		"Erfc":            luamapper.MustToGFunction(math.Erfc),
		"Exp":             luamapper.MustToGFunction(math.Exp),
		"Exp2":            luamapper.MustToGFunction(math.Exp2),
		"Expm1":           luamapper.MustToGFunction(math.Expm1),
		"Float32bits":     luamapper.MustToGFunction(math.Float32bits),
		"Float32frombits": luamapper.MustToGFunction(math.Float32frombits),
		"Float64bits":     luamapper.MustToGFunction(math.Float64bits),
		"Float64frombits": luamapper.MustToGFunction(math.Float64frombits),
		"Floor":           luamapper.MustToGFunction(math.Floor),
		"Frexp":           luamapper.MustToGFunction(math.Frexp),
		"Gamma":           luamapper.MustToGFunction(math.Gamma),
		"Hypot":           luamapper.MustToGFunction(math.Hypot),
		"Ilogb":           luamapper.MustToGFunction(math.Ilogb),
		"Inf":             luamapper.MustToGFunction(math.Inf),
		"IsInf":           luamapper.MustToGFunction(math.IsInf),
		"IsNaN":           luamapper.MustToGFunction(math.IsNaN),
		"J0":              luamapper.MustToGFunction(math.J0),
		"J1":              luamapper.MustToGFunction(math.J1),
		"Jn":              luamapper.MustToGFunction(math.Jn),
		"Ldexp":           luamapper.MustToGFunction(math.Ldexp),
		"Lgamma":          luamapper.MustToGFunction(math.Lgamma),
		"Log":             luamapper.MustToGFunction(math.Log),
		"Log10":           luamapper.MustToGFunction(math.Log10),
		"Log1p":           luamapper.MustToGFunction(math.Log1p),
		"Log2":            luamapper.MustToGFunction(math.Log2),
		"Logb":            luamapper.MustToGFunction(math.Logb),
		"Max":             luamapper.MustToGFunction(math.Max),
		"Min":             luamapper.MustToGFunction(math.Min),
		"Mod":             luamapper.MustToGFunction(math.Mod),
		"Modf":            luamapper.MustToGFunction(math.Modf),
		"NaN":             luamapper.MustToGFunction(math.NaN),
		"Nextafter":       luamapper.MustToGFunction(math.Nextafter),
		"Nextafter32":     luamapper.MustToGFunction(math.Nextafter32),
		"Pow":             luamapper.MustToGFunction(math.Pow),
		"Pow10":           luamapper.MustToGFunction(math.Pow10),
		"Remainder":       luamapper.MustToGFunction(math.Remainder),
		"Signbit":         luamapper.MustToGFunction(math.Signbit),
		"Sin":             luamapper.MustToGFunction(math.Sin),
		"Sincos":          luamapper.MustToGFunction(math.Sincos),
		"Sinh":            luamapper.MustToGFunction(math.Sinh),
		"Sqrt":            luamapper.MustToGFunction(math.Sqrt),
		"Tan":             luamapper.MustToGFunction(math.Tan),
		"Tanh":            luamapper.MustToGFunction(math.Tanh),
		"Trunc":           luamapper.MustToGFunction(math.Trunc),
		"Y0":              luamapper.MustToGFunction(math.Y0),
		"Y1":              luamapper.MustToGFunction(math.Y1),
		"Yn":              luamapper.MustToGFunction(math.Yn),
	}

	vars = map[string]lua.LValue{
		"E":          luamapper.MustToValue(math.E),
		"MaxFloat32": luamapper.MustToValue(math.MaxFloat32),
		"MaxInt8":    luamapper.MustToValue(math.MaxInt8),
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
