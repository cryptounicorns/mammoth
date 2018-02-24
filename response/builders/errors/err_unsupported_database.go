package errors

import (
	"fmt"
)

type ErrUnsupportedDatabase struct {
	t string
}

func (e *ErrUnsupportedDatabase) Error() string {
	return fmt.Sprintf(
		"Unsupported database '%s'",
		e.t,
	)
}
func NewErrUnsupportedDatabase(t string) error {
	return &ErrUnsupportedDatabase{t}
}
