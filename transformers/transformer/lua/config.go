package lua

type Config struct {
	FunctionName string `validate:"required"`
	Code         string `validate:"required"`
}
