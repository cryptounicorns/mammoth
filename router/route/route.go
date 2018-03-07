package route

import (
	"net/http"

	"github.com/corpix/loggers"
	"github.com/savaki/swag/endpoint"
	"github.com/savaki/swag/swagger"

	"github.com/cryptounicorns/mammoth/handler"
)

type Route struct {
	Handler handler.Handler
	config  Config
}

func (r Route) Endpoint() *swagger.Endpoint {
	var (
		ps = []endpoint.Option{}
	)

	for _, v := range r.Handler.Parameters {
		// FIXME: We don't know if this parameter is required at this moment :(
		// So just pass false :(
		ps = append(
			ps,
			endpoint.Query(
				v.Name,
				v.Type.String(),
				"",
				false,
			),
		)
	}

	ps = append(
		ps,
		[]endpoint.Option{
			endpoint.Handler(r.Handler),
			endpoint.Description(r.config.Description.Long),
			endpoint.Response(http.StatusOK, struct{}{}, "Requested data"),
			endpoint.Response(http.StatusBadRequest, struct{}{}, "Validation failed"),
			endpoint.Response(http.StatusInternalServerError, struct{}{}, "Something went wrong, error should be logged"),
		}...,
	)

	return endpoint.New(
		r.config.Method,
		r.config.Path,
		r.config.Description.Short,
		ps...,
	)
}

func (r Route) Close() error {
	return r.Handler.Close()
}

func FromConfig(c Config, l loggers.Logger) (Route, error) {
	var (
		h   handler.Handler
		err error
	)

	h, err = handler.FromConfig(c.Handler, l)
	if err != nil {
		return Route{}, err
	}

	return Route{
		Handler: h,
		config:  c,
	}, nil
}

func MustFromConfig(c Config, l loggers.Logger) Route {
	var (
		r   Route
		err error
	)

	r, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return r
}
