package columns

type Config struct {
	Database string `validate:"required"`
	Names    [][]interface{}
	Filter   []int
}
