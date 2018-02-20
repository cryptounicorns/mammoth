package http

type Config struct {
	Addr string `validate:"required"`
}
