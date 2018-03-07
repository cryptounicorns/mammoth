package tsdbs

import (
	"github.com/cryptounicorns/tsdbs/database/influxdb"
)

type Config struct {
	Type     string `validate:"required"`
	Influxdb *influxdb.Config
}
