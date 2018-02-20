package parameter

type Config struct {
	Name string `validate:"required"`
	Type string `validate:"required"`
}
