package errors

import (
	"fmt"
)

type ErrUnknownBuilderType struct {
	t string
}

func (e *ErrUnknownBuilderType) Error() string {
	return fmt.Sprintf(
		"Unknown builder type '%s'",
		e.t,
	)
}
func NewErrUnknownBuilderType(t string) error {
	return &ErrUnknownBuilderType{t}
}
