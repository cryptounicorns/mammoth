package validators

import (
	"fmt"
)

type ErrUnknownValidatorType struct {
	t string
}

func (e *ErrUnknownValidatorType) Error() string {
	return fmt.Sprintf(
		"Unknown validator type '%s'",
		e.t,
	)
}
func NewErrUnknownValidatorType(t string) error {
	return &ErrUnknownValidatorType{t}
}
