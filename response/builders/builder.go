package builders

import (
	"fmt"
	"strings"

	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"

	"github.com/cryptounicorns/mammoth/response/builders/builder/columns"
	"github.com/cryptounicorns/mammoth/response/builders/errors"
)

type Builder interface {
	Build(interface{}) (interface{}, error)
}

func FromConfig(c Config, l loggers.Logger) (Builder, error) {
	var (
		t   = strings.ToLower(c.Type)
		log = prefixwrapper.New(
			fmt.Sprintf("Builder %s: ", t),
			l,
		)
	)

	switch t {
	case columns.Name:
		return columns.FromConfig(
			*c.Columns,
			log,
		)
	default:
		return nil, errors.NewErrUnknownBuilderType(c.Type)
	}
}

func MustFromConfig(c Config, l loggers.Logger) Builder {
	var (
		b   Builder
		err error
	)

	b, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return b
}
