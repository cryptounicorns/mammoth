package validators

import (
	"fmt"
	"strings"

	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"

	"github.com/cryptounicorns/mammoth/validators/validator/lua"
)

type Validator interface {
	Validate(interface{}) error
	Close() error
}

func FromConfig(c Config, l loggers.Logger) (Validator, error) {
	var (
		t   = strings.ToLower(c.Type)
		log = prefixwrapper.New(
			fmt.Sprintf("Validator %s: ", t),
			l,
		)
	)

	switch t {
	case lua.Name:
		return lua.FromConfig(c.Lua, log)
	default:
		return nil, NewErrUnknownValidatorType(c.Type)
	}
}

func MustFromConfig(c Config, l loggers.Logger) Validator {
	var (
		v   Validator
		err error
	)

	v, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return v
}
