package router

import (
	"net/http"

	"github.com/corpix/loggers"
	"github.com/gorilla/mux"
	"github.com/savaki/swag"
	"github.com/savaki/swag/swagger"

	"github.com/cryptounicorns/mammoth/router/route"
)

type Router struct {
	Mux    *mux.Router
	routes []route.Route
	log    loggers.Logger
}

func (rr Router) Close() error {
	var (
		err error
	)

	for _, r := range rr.routes {
		err = r.Close()
		if err != nil {
			return err
		}
	}

	return nil
}

func FromRoutes(rs []route.Route, l loggers.Logger) (*Router, error) {
	var (
		rr = mux.NewRouter()
		es = make(
			[]*swagger.Endpoint,
			len(rs),
		)
	)

	for k, r := range rs {
		es[k] = r.Endpoint()
	}

	var (
		api = swag.New(
			swag.Title(""),
			swag.Tag("default", "", swag.TagDescription(""), swag.TagURL("default")),
			swag.Endpoints(es...),
		)
	)

	api.Walk(
		func(p string, e *swagger.Endpoint) {
			rr.
				Path(swag.ColonPath(p)).
				Methods(e.Method).
				Handler(e.Handler.(http.Handler))
		},
	)

	rr.
		Path("/swagger").
		Methods("GET").
		Handler(api.Handler(true))

	return &Router{
		Mux:    rr,
		routes: rs,
		log:    l,
	}, nil
}

func FromConfig(c Config, l loggers.Logger) (*Router, error) {
	var (
		rs = make(
			[]route.Route,
			len(c),
		)
		err error
	)

	for k, v := range c {
		rs[k], err = route.FromConfig(v, l)
		if err != nil {
			return nil, err
		}
	}

	return FromRoutes(rs, l)
}
