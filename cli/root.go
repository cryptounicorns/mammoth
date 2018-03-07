package cli

import (
	"time"

	"github.com/urfave/cli"

	"github.com/cryptounicorns/mammoth/http"
	"github.com/cryptounicorns/mammoth/router"
)

var (
	// RootCommands is a list of subcommands for the application.
	RootCommands = []cli.Command{}

	// RootFlags is a list of flags for the application.
	RootFlags = []cli.Flag{
		cli.StringFlag{
			Name:   "config, c",
			Usage:  "application configuration file",
			EnvVar: "CONFIG",
			Value:  "config.json",
		},
		cli.BoolFlag{
			Name:  "debug",
			Usage: "add this flag to enable debug mode",
		},
	}
)

// RootAction is executing when program called without any subcommand.
func RootAction(c *cli.Context) error {
	var (
		rr  *router.Router
		s   *http.Server
		err error
	)

	rr, err = router.FromConfig(Config.Router, log)
	if err != nil {
		return err
	}
	defer rr.Close()

	for {
		s = http.New(
			Config.HTTP,
			rr.Mux,
			log,
		)

		err = s.Serve()
		if err != nil {
			log.Error(err)
		}
		time.Sleep(1 * time.Second)
	}
}
