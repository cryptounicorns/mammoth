package errcomposer

import (
	"reflect"
)

// ComposedError is an error that aggregates multiple
// errors.
type ComposedError struct {
	errors []error
}

// Error returns the aggregation of the composed error messages.
func (ce *ComposedError) Error() string {
	var errStr string
	for k, err := range ce.errors {
		if k > 0 {
			errStr += " > "
		}
		errStr += err.Error()
	}
	return errStr
}

// Compose multiple errors.
func Compose(errors ...error) error {
	return &ComposedError{errors}
}

// Decompose the composed error
// It tries to decompose the error and if it can
// will return a slice of errors and true, if it failed
// to decompose the error it will return nil and false.
// Just like the map value retrieval by the key.
func Decompose(err error) ([]error, bool) {
	if composed, ok := err.(*ComposedError); ok {
		return composed.errors, true
	}
	return nil, false
}

// Has checks if err is inside composedErr
// If err is one of composedErr members then it will return true.
// If composedErr is not a ComposedError then it will compare
// composedErr with err returning the result.
// All composed errors processsed recursively, it is ok to have
// nested composed errors.
func Has(composedErr, err error) bool {
	if errs, ok := Decompose(composedErr); ok {
		for _, e := range errs {
			if Has(e, err) {
				return true
			}
		}
		return false
	}

	return isSame(composedErr, err)
}

func isSame(a, b error) bool {
	return a == b || reflect.TypeOf(a).String() == reflect.TypeOf(b).String()
}
