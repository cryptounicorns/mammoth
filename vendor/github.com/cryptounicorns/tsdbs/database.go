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

type DatabaseWithConnection struct {
	Database
	Conn Connection
}

func (d DatabaseWithConnection) Close() error {
	var (
		err error
	)
	err = d.Database.Close()
	if err != nil {
		return err
	}

	return d.Conn.Close()
}

func FromConfig(c Config, l loggers.Logger) (Database, error) {
	var (
		d    Database
		conn Connection
		err  error
	)

	conn, err = Connect(c, l)
	if err != nil {
		return nil, err
	}

	d, err = FromConfigWithConnection(c, conn, l)
	if err != nil {
		return nil, err
	}

	return DatabaseWithConnection{
		Database: d,
		Conn:     conn,
	}, nil
}

func MustFromConfig(c Config, l loggers.Logger) Database {
	var (
		db  Database
		err error
	)

	db, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return db
}

func FromConfigWithConnection(c Config, conn Connection, l loggers.Logger) (Database, error) {
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
