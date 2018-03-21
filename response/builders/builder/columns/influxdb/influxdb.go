package influxdb

import (
	"github.com/corpix/reflect"
	client "github.com/influxdata/influxdb/client/v2"

	"github.com/cryptounicorns/mammoth/response/builders/builder/columns/result"
)

const (
	Name = "influxdb"
)

func Build(v interface{}) (result.Result, error) {
	var (
		r   []client.Result
		res result.Result
		ok  bool
	)

	r, ok = v.([]client.Result)
	if !ok {
		return res, reflect.NewErrCanNotAssertType(
			v,
			reflect.TypeOf(&client.Result{}),
		)
	}

	for _, rr := range r {
		for _, s := range rr.Series {
			res = result.Result{
				Name:    s.Name,
				Values:  s.Values,
				Columns: s.Columns,
			}

			// XXX: C'mon, our statements will have mostly one result
			break
		}
	}

	return res, nil
}
