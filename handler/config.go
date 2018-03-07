package handler

import (
	"github.com/cryptounicorns/tsdbs"
	"github.com/cryptounicorns/mammoth/parameters"
	"github.com/cryptounicorns/mammoth/response"
	"github.com/cryptounicorns/mammoth/transformers"
	"github.com/cryptounicorns/mammoth/validators"
)

type Config struct {
	Parameters  parameters.Config `validate:"required,dive"`
	Validator   validators.Config `validate:"required"`
	Transformer transformers.Config
	Database    tsdbs.Config `validate:"required"`
	Response    response.Config  `validate:"required"`
}
