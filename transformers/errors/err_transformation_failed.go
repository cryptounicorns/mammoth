package errors

import (
	"fmt"
)

type ErrTransformationFailed struct {
	reason error
}

func (e ErrTransformationFailed) Error() string {
	return fmt.Sprintf(
		"Transformation failed with '%s'",
		e.reason,
	)
}
func NewErrTransformationFailed(reason error) error {
	return ErrTransformationFailed{reason}
}
