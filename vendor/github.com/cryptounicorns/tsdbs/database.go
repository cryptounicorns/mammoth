package tsdbs

import (
	"fmt"
	"strings"

	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"
	client "github.com/influxdata/influxdb/client/v2"

	"github.com/cryptounicorns/tsdbs/database/influxdb"
)

type Database interface {
	Query(string, map[string]interface{}) (interface{}, error)
	Close() error
}

func FromConfig(c Config, conn Connection, l loggers.Logger) (Database, error) {
	var (
		t   = strings.ToLower(c.Type)
		log = prefixwrapper.New(
			fmt.Sprintf("Database %s: ", t),
			l,
		)
	)

	switch t {
	case influxdb.Name:
		return influxdb.FromConfig(
			*c.Influxdb,
			conn.(client.Client),
			log,
		)
	default:
		return nil, NewErrUnknownDatabaseType(c.Type)
	}
}

func MustFromConfig(c Config, conn Connection, l loggers.Logger) Database {
	var (
		db  Database
		err error
	)

	db, err = FromConfig(c, conn, l)
	if err != nil {
		panic(err)
	}

	return db
}
