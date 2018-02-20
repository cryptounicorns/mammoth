package validators

import (
	"github.com/cryptounicorns/mammoth/validators/validator/lua"
)

type Config struct {
	Type string `validate:"required"`
	Lua  lua.Config
}
