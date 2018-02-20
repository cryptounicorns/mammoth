package parameters

import (
	"github.com/cryptounicorns/mammoth/parameters/parameter"
)

type Parameters []parameter.Parameter

func FromConfig(c Config) (Parameters, error) {
	var (
		ps  = make(Parameters, len(c))
		err error
	)

	for k, v := range c {
		ps[k], err = parameter.FromConfig(v)
		if err != nil {
			return nil, err
		}
	}

	return ps, nil
}

func MustFromConfig(c Config) Parameters {
	var (
		ps  Parameters
		err error
	)

	ps, err = FromConfig(c)
	if err != nil {
		panic(err)
	}

	return ps
}
