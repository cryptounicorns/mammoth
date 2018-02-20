package transformers

import (
	"github.com/cryptounicorns/mammoth/transformers/transformer/lua"
)

type Config struct {
	Type string `validate:"required"`
	Lua  lua.Config
}
