package route

import (
	"github.com/cryptounicorns/mammoth/handler"
)

type Config struct {
	Path   string `validate:"required"`
	Method string `validate:"required"`

	Description struct {
		Title   string
		Version string
		Short   string
		Long    string
		License struct {
			Name string
			URL  string
		}
	}

	Handler handler.Config `validate:"required"`
}
