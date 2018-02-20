package databases

import (
	"github.com/cryptounicorns/mammoth/databases/database/influxdb"
)

type Config struct {
	Type     string `validate:"required"`
	Influxdb *influxdb.Config
}
