mammoth
---------

[![Build Status](https://travis-ci.org/cryptounicorns/mammoth.svg?branch=master)](https://travis-ci.org/cryptounicorns/mammoth)

This is a frontend service for a database queries, where queries represented as HTTP endpoints.

The idea:
- describe a input parameters
- write validation for input parameters
- apply input parameters to query template
- respond with data

## Development

You could use docker or nix-shell.

### Running mammoth

Run it for development:

``` console
$ go run ./mammoth/mammoth.go --debug
```

Or build a binary release:

``` console
$ GOOS=linux make
# This will put a binary into ./build/mammoth
```

