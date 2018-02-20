package main

import (
	"errors"
	"fmt"

	"github.com/davecgh/go-spew/spew"
	"github.com/corpix/errcomposer"
)

var (
	ErrFoo = errors.New("This is a foo error")
	ErrBar = errors.New("This is a bar error")
	ErrBaz = errors.New("This is a baz error")
)

func main() {
	errs := errcomposer.Compose(
		ErrFoo,
		ErrBar,
	)

	fmt.Println("This is how composed error looks like:")
	spew.Dump(errs)

	fmt.Println("")

	fmt.Println(
		"Do we have a ErrFoo in our chain?",
		errcomposer.Has(
			errs,
			ErrFoo,
		),
	)
	fmt.Println(
		"Do we have a ErrBaz in out chain?",
		errcomposer.Has(
			errs,
			ErrBaz,
		),
	)

	fmt.Println("")

	fmt.Println("Great! Now give me a slice of all errors, please!")
	spew.Dump(errcomposer.Decompose(errs))

	fmt.Println("")

	fmt.Println("What if I will ask you to decompose a ErrBaz for me?")
	spew.Dump(errcomposer.Decompose(ErrBaz))
}
