package handler

import (
	"github.com/cryptounicorns/mammoth/databases"
	"github.com/cryptounicorns/mammoth/parameters"
	"github.com/cryptounicorns/mammoth/transformers"
	"github.com/cryptounicorns/mammoth/validators"
)

type Config struct {
	Format      string            `validate:"required"`
	Parameters  parameters.Config `validate:"required,dive"`
	Validator   validators.Config `validate:"required"`
	Transformer transformers.Config
	Database    databases.Config `validate:"required"`
}
