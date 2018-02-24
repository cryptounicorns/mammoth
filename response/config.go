package response

import (
	"github.com/cryptounicorns/mammoth/response/builders"
)

type Config struct {
	Format  string          `validate:"required"`
	Builder builders.Config `validate:"required"`
}
