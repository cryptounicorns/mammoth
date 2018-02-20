package main

import (
	"runtime"

	"github.com/cryptounicorns/mammoth/cli"
)

func init() { runtime.GOMAXPROCS(runtime.NumCPU()) }
func main() { cli.Execute() }

