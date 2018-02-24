package builders

import (
	"github.com/cryptounicorns/mammoth/response/builders/builder/columns"
)

type Config struct {
	Type    string `validate:"require"`
	Columns *columns.Config
}
