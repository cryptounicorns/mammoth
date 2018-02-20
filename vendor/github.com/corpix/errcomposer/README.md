errcomposer
-------------

[![Build Status](https://travis-ci.org/corpix/errcomposer.svg?branch=master)](https://travis-ci.org/corpix/errcomposer)

This is a simple `error` composition package.

## Usage

There are only 3 functions in the package:

* `Compose(...error) -> error`
* `Decompose(error)  -> error, bool`
* `Has(error, error) -> bool`

Here is simple use [example](example):

``` go
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
```

Output:

``` text
This is how composed error looks like:
(*errcomposer.ComposedError)(0xc420010460)(This is a foo error > This is a bar error)

Do we have a ErrFoo in our chain? true
Do we have a ErrBaz in out chain? false

Great! Now give me a slice of all errors, please!
([]error) (len=2 cap=2) {
 (*errors.errorString)(0xc42000a540)(This is a foo error),
 (*errors.errorString)(0xc42000a550)(This is a bar error)
}
(bool) true

What if I will ask you to decompose a ErrBaz for me?
([]error) <nil>
(bool) false
```

## License

MIT
