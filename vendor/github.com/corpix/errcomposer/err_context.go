package errcomposer

// ErrContext adds a context message to the original error message.
type ErrContext struct {
	OriginalError error
	Context       string
}

func (e ErrContext) Error() string {
	return e.Context + e.OriginalError.Error()
}

func NewErrContext(context string, err error) ErrContext {
	return ErrContext{
		OriginalError: err,
		Context:       context,
	}
}
