package influxdb

import (
	"github.com/corpix/template"

	client "github.com/influxdata/influxdb/client/v2"
)

// XXX: Influxdb is "awesome"
// No query escaping functions
// No query builder
// Just stupid NewQueryWithParameters which is not working with things like time(1h)

type QueryBuilder struct {
	template  *template.Template
	database  string
	precision string
}

func (q *QueryBuilder) WithParameters(parameters map[string]interface{}) (client.Query, error) {
	var (
		buf []byte
		err error
	)

	buf, err = q.template.Apply(
		context{
			Parameters: parameters,
		},
	)
	if err != nil {
		return client.Query{}, err
	}

	return client.NewQuery(
		string(buf),
		q.database,
		q.precision,
	), nil
}

func QueryBuilderFromTemplate(tpl string, database string, precision string) (*QueryBuilder, error) {
	var (
		t   *template.Template
		err error
	)

	t, err = template.Parse(tpl)
	if err != nil {
		return nil, err
	}

	return &QueryBuilder{
		template:  t,
		database:  database,
		precision: precision,
	}, nil
}
