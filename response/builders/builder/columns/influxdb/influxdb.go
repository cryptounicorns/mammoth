package influxdb

import (
	"github.com/corpix/reflect"
	client "github.com/influxdata/influxdb/client/v2"
)

const (
	Name = "influxdb"
)

type Response struct {
	Name    string
	Columns []string
	Values  [][]interface{}
}

func Build(v interface{}) (interface{}, error) {
	var (
		r   []client.Result
		res = []Response{}
		ok  bool
	)

	r, ok = v.([]client.Result)
	if !ok {
		return nil, reflect.NewErrCanNotAssertType(
			v,
			reflect.TypeOf(&client.Response{}),
		)
	}

	for _, rr := range r {
		for _, s := range rr.Series {
			res = append(
				res,
				Response{
					Name:    s.Name,
					Values:  s.Values,
					Columns: s.Columns,
				},
			)
		}
	}

	return res, nil
}
