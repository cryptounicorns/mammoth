package influxdb

import (
	"github.com/corpix/loggers"
	client "github.com/influxdata/influxdb/client/v2"
)

const (
	Name = "influxdb"
)

type InfluxDB struct {
	config       Config
	client       client.Client
	queryBuilder *QueryBuilder
	log          loggers.Logger
}

func (d *InfluxDB) Query(parameters map[string]interface{}) (interface{}, error) {
	var (
		q   client.Query
		r   *client.Response
		err error
	)

	q, err = d.queryBuilder.WithParameters(parameters)
	if err != nil {
		return nil, err
	}

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

func FromConfig(c Config, cl client.Client, l loggers.Logger) (*InfluxDB, error) {
	var (
		qb  *QueryBuilder
		err error
	)

	qb, err = QueryBuilderFromTemplate(c.Query, c.Database, c.Precision)
	if err != nil {
		return nil, err
	}

	return &InfluxDB{
		config:       c,
		client:       cl,
		queryBuilder: qb,
		log:          l,
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
