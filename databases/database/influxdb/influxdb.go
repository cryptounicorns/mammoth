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

func (d *InfluxDB) Query(parameters map[string]interface{}) (interface{}, error) {
	var (
		r   *client.Response
		err error
	)

	r, err = d.client.Query(
		client.NewQueryWithParameters(
			d.config.Query,
			d.config.Database,
			d.config.Precision,
			parameters,
		),
	)
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
