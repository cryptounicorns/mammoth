package columns

import (
	"strings"

	"github.com/corpix/loggers"

	"github.com/cryptounicorns/mammoth/response/builders/builder/columns/influxdb"
	"github.com/cryptounicorns/mammoth/response/builders/errors"
)

const (
	Name = "columns"
)

func FromConfig(c Config, l loggers.Logger) (func(v interface{}) (interface{}, error), error) {
	var (
		db = strings.ToLower(c.Database)
	)

	switch db {
	case influxdb.Name:
		return influxdb.Build, nil
	default:
		return nil, errors.NewErrUnsupportedDatabase(c.Database)
	}
}

func MustFromConfig(c Config, l loggers.Logger) func(v interface{}) (interface{}, error) {
	var (
		b   func(v interface{}) (interface{}, error)
		err error
	)

	b, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return b
}
