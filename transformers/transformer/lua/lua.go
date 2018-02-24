package lua

import (
	"github.com/corpix/loggers"
	"github.com/corpix/lua/mapper"
	"github.com/corpix/lua/pool"
	lua "github.com/yuin/gopher-lua"

	luabinding "github.com/cryptounicorns/mammoth/lua"
	"github.com/cryptounicorns/mammoth/transformers/errors"
)

const (
	Name = "lua"
)

func newVM(c Config, l loggers.Logger) func() *lua.LState {
	return func() *lua.LState {
		l.Debug("Creating new Lua VM")

		var (
			l   = lua.NewState()
			err error
		)

		luabinding.Bind(l)

		err = l.DoString(c.Code)
		if err != nil {
			panic(err)
		}

		return l
	}
}

type Lua struct {
	config Config
	pool   *pool.Pool
	log    loggers.Logger
}

func (l Lua) Transform(v interface{}) (interface{}, error) {
	var (
		vm   = l.pool.Get()
		lv   lua.LValue
		r    interface{}
		rerr interface{}
		ok   bool
		err  error
	)
	defer l.pool.Put(vm)

	lv, err = mapper.ToValue(v)
	if err != nil {
		return nil, err
	}

	err = vm.CallByParam(
		lua.P{
			Fn:      vm.GetGlobal(l.config.FunctionName),
			NRet:    2,
			Protect: true,
		},
		lv,
	)
	if err != nil {
		return nil, err
	}

	lv = vm.Get(-1)
	vm.Pop(1)
	rerr, err = mapper.FromValue(lv)
	if err != nil {
		return nil, err
	}

	err, ok = rerr.(error)
	if ok && err != nil {
		return nil, errors.NewErrTransformationFailed(err)
	}

	lv = vm.Get(-1)
	vm.Pop(1)
	r, err = mapper.FromValue(lv)
	if err != nil {
		return nil, err
	}

	return r, nil
}

func (l Lua) Close() error {
	l.log.Debug("Closing")
	l.pool.Close()

	return nil
}

func FromConfig(c Config, l loggers.Logger) (Lua, error) {
	var (
		p = pool.New(newVM(c, l))
	)

	return Lua{
		config: c,
		pool:   p,
		log:    l,
	}, nil
}

func MustFromConfig(c Config, l loggers.Logger) Lua {
	var (
		v   Lua
		err error
	)

	v, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return v
}
