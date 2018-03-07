package handler

import (
	"fmt"
	"net/http"

	"github.com/corpix/errcomposer"
	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"
	"github.com/corpix/reflect"
	"github.com/gorilla/mux"

	"github.com/cryptounicorns/tsdbs"
	"github.com/cryptounicorns/mammoth/parameters"
	"github.com/cryptounicorns/mammoth/response"
	"github.com/cryptounicorns/mammoth/transformers"
	transformersErrors "github.com/cryptounicorns/mammoth/transformers/errors"
	"github.com/cryptounicorns/mammoth/validators"
	validatorsErrors "github.com/cryptounicorns/mammoth/validators/errors"
)

type Handler struct {
	Parameters         parameters.Parameters
	validator          validators.Validator
	transformer        transformers.Transformer
	databaseConnection tsdbs.Connection
	database           tsdbs.Database
	response           *response.Response
	config             Config
	log                loggers.Logger
}

func (h Handler) ServeHTTP(rw http.ResponseWriter, r *http.Request) {
	var (
		capture = mux.Vars(r)
		query   = r.URL.Query()
		ps      = make(
			map[string]interface{},
			len(h.Parameters),
		)
		c string
		q []string
		v interface{}

		ok  bool
		err error
	)

	for _, p := range h.Parameters {
		c, ok = capture[p.Name]
		if !ok {
			q, _ = query[p.Name]

			if len(q) > 0 {
				c = q[0]
			}
		}

		ps[p.Name], err = p.ValueOf(c)
		if err != nil {
			h.handleError(
				errcomposer.NewErrContext(
					fmt.Sprintf("Parameter '%s': ", p.Name),
					err,
				),
				rw,
			)
			return
		}
	}

	err = h.validator.Validate(ps)
	if err != nil {
		h.handleError(err, rw)
		return
	}

	if h.transformer != nil {
		v, err = h.transformer.Transform(ps)
		if err != nil {
			h.handleError(err, rw)
			return
		}
		v, err = reflect.ConvertToType(v, reflect.TypeOf(ps))
		if err != nil {
			h.handleError(err, rw)
			return
		}
		ps = v.(map[string]interface{})
	}

	v, err = h.database.Query(ps)
	if err != nil {
		h.handleError(err, rw)
		return
	}

	err = h.response.Write(v, rw, r)
	if err != nil {
		h.handleError(err, rw)
		return
	}
}

func (h Handler) handleError(err error, rw http.ResponseWriter) {
	var (
		statusCode int
		statusText string
	)

	switch e := err.(type) {
	case errcomposer.ErrContext:
		switch e.OriginalError.(type) {
		case reflect.ErrCanNotConvertType:
			statusCode = http.StatusBadRequest
			statusText = e.Error()
		}
	case validatorsErrors.ErrValidationFailed:
		statusCode = http.StatusBadRequest
		statusText = e.Error()
	case transformersErrors.ErrTransformationFailed:
		statusCode = http.StatusInternalServerError
		statusText = e.Error()
	}

	// XXX: Default handler
	if statusCode == 0 {
		h.log.Error(err)
		statusCode = http.StatusInternalServerError
		statusText = http.StatusText(statusCode)
	}

	rw.WriteHeader(statusCode)
	rw.Write([]byte(statusText))
}

func FromConfig(c Config, l loggers.Logger) (Handler, error) {
	var (
		log = prefixwrapper.New("Handler: ", l)
		ps  parameters.Parameters
		vr  validators.Validator
		tr  transformers.Transformer
		dbc tsdbs.Connection
		db  tsdbs.Database
		r   *response.Response
		err error
	)

	ps, err = parameters.FromConfig(c.Parameters)
	if err != nil {
		return Handler{}, err
	}

	vr, err = validators.FromConfig(c.Validator, log)
	if err != nil {
		return Handler{}, err
	}

	if c.Transformer.Type != "" {
		tr, err = transformers.FromConfig(c.Transformer, log)
		if err != nil {
			return Handler{}, err
		}
	}

	dbc, err = tsdbs.Connect(c.Database, log)
	if err != nil {
		return Handler{}, err
	}

	db, err = tsdbs.FromConfig(c.Database, dbc, log)
	if err != nil {
		return Handler{}, err
	}

	r, err = response.FromConfig(c.Response, log)
	if err != nil {
		return Handler{}, err
	}

	return Handler{
		Parameters:         ps,
		validator:          vr,
		transformer:        tr,
		databaseConnection: dbc,
		database:           db,
		response:           r,
		config:             c,
		log:                log,
	}, nil
}

func MustFromConfig(c Config, l loggers.Logger) Handler {
	var (
		h   Handler
		err error
	)

	h, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return h
}
