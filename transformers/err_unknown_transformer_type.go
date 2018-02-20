package transformers

import (
	"fmt"
)

type ErrUnknownTransformerType struct {
	t string
}

func (e *ErrUnknownTransformerType) Error() string {
	return fmt.Sprintf(
		"Unknown transformer type '%s'",
		e.t,
	)
}
func NewErrUnknownTransformerType(t string) error {
	return &ErrUnknownTransformerType{t}
}
