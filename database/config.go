package database

import (
	"github.com/cryptounicorns/tsdbs"
)

type Config struct {
	tsdbs.Config
	Query string `validate:"required"`
}
