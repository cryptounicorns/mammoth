package columns

import (
	"math"
	"strings"

	"github.com/corpix/loggers"
	"github.com/corpix/reflect"

	"github.com/cryptounicorns/mammoth/response/builders/builder/columns/influxdb"
	"github.com/cryptounicorns/mammoth/response/builders/builder/columns/result"
	"github.com/cryptounicorns/mammoth/response/builders/errors"
)

const (
	Name = "columns"
)

type name struct {
	index int
	name  string
}

type Columns struct {
	config  Config
	names   []name
	Builder func(v interface{}) (result.Result, error)
}

func (c *Columns) Build(v interface{}) (interface{}, error) {
	var (
		filtersNum = len(c.config.Filter)
		r          result.Result
		err        error
	)

	r, err = c.Builder(v)
	if err != nil {
		return nil, err
	}

	if len(c.names) > 0 {
		for _, v := range c.names {
			r.Columns[v.index] = v.name
		}
	}

	if filtersNum > 0 {
		nr := result.Result{
			Name:    r.Name,
			Columns: make([]string, filtersNum),
			Values:  make([][]interface{}, len(r.Values)),
		}

		for k, index := range c.config.Filter {
			nr.Columns[k] = r.Columns[index]
		}

		for k, v := range r.Values {
			nv := make([]interface{}, filtersNum)
			for kk, vk := range c.config.Filter {
				nv[kk] = v[vk]
			}
			nr.Values[k] = nv
		}
		r = nr
	}

	if c.config.Name != "" {
		r.Name = c.config.Name
	}

	return r, nil
}

func FromConfig(c Config, l loggers.Logger) (*Columns, error) {
	var (
		db = strings.ToLower(c.Database)
		r  = &Columns{
			config: c,
			names:  make([]name, len(c.Names)),
		}
	)

	for k, v := range c.Names {
		i, ok := v[0].(float64)
		if !ok {
			return nil, reflect.NewErrCanNotAssertType(
				v[0],
				reflect.TypeFloat64,
			)
		}

		n, ok := v[1].(string)
		if !ok {
			return nil, reflect.NewErrCanNotAssertType(
				v[1],
				reflect.TypeString,
			)
		}

		r.names[k] = name{
			index: int(math.Abs(i)),
			name:  n,
		}
	}

	switch db {
	case influxdb.Name:
		r.Builder = influxdb.Build
	default:
		return nil, errors.NewErrUnsupportedDatabase(c.Database)
	}

	return r, nil
}

func MustFromConfig(c Config, l loggers.Logger) *Columns {
	var (
		b   *Columns
		err error
	)

	b, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return b
}
