package influxdb

import (
	client "github.com/influxdata/influxdb/client/v2"
)

type Config struct {
	Database  string            `validate:"required"`
	Client    client.HTTPConfig `validate:"required"`
	Precision string            `validate:"required,eq=nanosecond|eq=microsecond|eq=millisecond|eq=second"`
	Query     string            `validate:"required"`
}
