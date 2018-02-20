package errors

import (
	"fmt"
)

type ErrValidationFailed struct {
	reason error
}

func (e ErrValidationFailed) Error() string {
	return fmt.Sprintf(
		"Validation failed with '%s'",
		e.reason,
	)
}
func NewErrValidationFailed(reason error) error {
	return ErrValidationFailed{reason}
}
