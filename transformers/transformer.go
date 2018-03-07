package transformers

import (
	"fmt"
	"strings"

	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"

	"github.com/cryptounicorns/mammoth/transformers/transformer/lua"
)

type Transformer interface {
	Transform(interface{}) (interface{}, error)
	Close() error
}

func FromConfig(c Config, l loggers.Logger) (Transformer, error) {
	var (
		t   = strings.ToLower(c.Type)
		log = prefixwrapper.New(
			fmt.Sprintf("Transformer %s: ", t),
			l,
		)
	)

	switch t {
	case lua.Name:
		return lua.FromConfig(c.Lua, log)
	default:
		return nil, NewErrUnknownTransformerType(c.Type)
	}
}

func MustFromConfig(c Config, l loggers.Logger) Transformer {
	var (
		v   Transformer
		err error
	)

	v, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return v
}
