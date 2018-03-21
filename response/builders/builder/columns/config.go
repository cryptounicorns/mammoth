package columns

type Config struct {
	Database string `validate:"required"`
	Name     string
	Names    [][]interface{}
	Filter   []int
}
