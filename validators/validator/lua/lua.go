package lua

import (
	"errors"

	"github.com/corpix/loggers"
	"github.com/corpix/lua/mapper"
	"github.com/corpix/lua/pool"
	lua "github.com/yuin/gopher-lua"

	luamath "github.com/cryptounicorns/mammoth/lua/math"
	luaspew "github.com/cryptounicorns/mammoth/lua/spew"
	luatime "github.com/cryptounicorns/mammoth/lua/time"
	luavalidator "github.com/cryptounicorns/mammoth/lua/validator"
	validatorsError "github.com/cryptounicorns/mammoth/validators/errors"
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

		l.PreloadModule("gospew", luaspew.Loader)
		l.PreloadModule("gomath", luamath.Loader)
		l.PreloadModule("gotime", luatime.Loader)
		l.PreloadModule("govalidator", luavalidator.Loader)

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

func (l Lua) Validate(v interface{}) error {
	var (
		vm  = l.pool.Get()
		lv  lua.LValue
		lvs string
		r   interface{}
		ok  bool
		err error
	)
	defer l.pool.Put(vm)

	lv, err = mapper.ToValue(v)
	if err != nil {
		return err
	}

	err = vm.CallByParam(
		lua.P{
			Fn:      vm.GetGlobal(l.config.FunctionName),
			NRet:    1,
			Protect: true,
		},
		lv,
	)
	if err != nil {
		return err
	}

	lv = vm.Get(-1)
	vm.Pop(1)

	r, err = mapper.FromValue(lv)
	if err != nil {
		return err
	}
	if r != nil {
		lvs, ok = r.(string)
		if ok {
			return validatorsError.NewErrValidationFailed(errors.New(lvs))
		} else {
			l.log.Errorf(
				"Failed to assert result type '%T' to string",
				r,
			)
		}
	}

	return nil
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
