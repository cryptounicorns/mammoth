package influxdb

import (
	"github.com/corpix/loggers"
	client "github.com/influxdata/influxdb/client/v2"
)

const (
	Name = "influxdb"
)

type InfluxDB struct {
	config Config
	client client.Client
	log    loggers.Logger
}

func (d *InfluxDB) Query(query string, parameters map[string]interface{}) (interface{}, error) {
	var (
		b   *QueryBuilder
		q   client.Query
		r   *client.Response
		err error
	)

	b, err = QueryBuilderFromTemplate(query, d.config.Database, d.config.Precision)
	if err != nil {
		return nil, err
	}

	q, err = b.WithParameters(parameters)
	if err != nil {
		return nil, err
	}

	d.log.Debug("Running query template: ", query)

	r, err = d.client.Query(q)
	if err != nil {
		return nil, err
	}

	err = r.Error()
	if err != nil {
		return nil, err
	}

	return r.Results, nil
}

func (d *InfluxDB) Close() error { return nil }

func FromConfig(c Config, cl client.Client, l loggers.Logger) (*InfluxDB, error) {
	return &InfluxDB{
		config: c,
		client: cl,
		log:    l,
	}, nil
}

func MustFromConfig(c Config, cl client.Client, l loggers.Logger) *InfluxDB {
	var (
		i   *InfluxDB
		err error
	)

	i, err = FromConfig(c, cl, l)
	if err != nil {
		panic(err)
	}

	return i
}
