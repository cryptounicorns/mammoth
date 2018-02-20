package parameter

import (
	"github.com/corpix/reflect"
)

type Parameter struct {
	Name string
	Type reflect.Type
}

func (p Parameter) ValueOf(v interface{}) (interface{}, error) {
	return reflect.ConvertToType(v, p.Type)
}

func FromConfig(c Config) (Parameter, error) {
	var (
		t   reflect.Type
		err error
	)

	t, err = reflect.ParseType(c.Type)
	if err != nil {
		return Parameter{}, err
	}

	return Parameter{
		Name: c.Name,
		Type: t,
	}, nil
}

func MustFromConfig(c Config) Parameter {
	var (
		p   Parameter
		err error
	)

	p, err = FromConfig(c)
	if err != nil {
		panic(err)
	}

	return p
}
