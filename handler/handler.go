package handler

import (
	"fmt"
	"net/http"

	"github.com/corpix/errcomposer"
	"github.com/corpix/formats"
	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"
	"github.com/corpix/reflect"
	"github.com/gorilla/mux"

	"github.com/cryptounicorns/mammoth/databases"
	"github.com/cryptounicorns/mammoth/parameters"
	"github.com/cryptounicorns/mammoth/transformers"
	transformersErrors "github.com/cryptounicorns/mammoth/transformers/errors"
	"github.com/cryptounicorns/mammoth/validators"
	validatorsErrors "github.com/cryptounicorns/mammoth/validators/errors"
)

type Handler struct {
	Parameters         parameters.Parameters
	validator          validators.Validator
	transformer        transformers.Transformer
	databaseConnection databases.Connection
	database           databases.Database
	format             formats.Format
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
		c   string
		q   []string
		buf []byte
		v   interface{}

		ok  bool
		err error
	)

	rw.Header().Set("content-type", "application/"+h.format.Name())

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

	buf, err = h.format.Marshal(v)
	if err != nil {
		h.handleError(err, rw)
		return
	}

	rw.WriteHeader(http.StatusOK)
	rw.Write(buf)
}

func (h Handler) Close() error {
	return h.databaseConnection.Close()
}

func (h Handler) handleError(err error, rw http.ResponseWriter) {
	var (
		statusCode       int
		statusText       string
		statusFormatText []byte
		innerErr         error
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

	statusFormatText, innerErr = h.format.Marshal(statusText)
	if innerErr != nil {
		h.log.Error(err)
	}

	rw.WriteHeader(statusCode)
	rw.Write(statusFormatText)
}

func FromConfig(c Config, l loggers.Logger) (Handler, error) {
	var (
		log = prefixwrapper.New("Handler: ", l)
		ps  parameters.Parameters
		vr  validators.Validator
		tr  transformers.Transformer
		dbc databases.Connection
		db  databases.Database
		f   formats.Format
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

	dbc, err = databases.Connect(c.Database, log)
	if err != nil {
		return Handler{}, err
	}

	db, err = databases.FromConfig(c.Database, dbc, log)
	if err != nil {
		return Handler{}, err
	}

	f, err = formats.New(c.Format)
	if err != nil {
		return Handler{}, err
	}

	return Handler{
		Parameters:         ps,
		validator:          vr,
		transformer:        tr,
		databaseConnection: dbc,
		database:           db,
		format:             f,
		config:             c,
		log:                log,
	}, nil
}

func MustFromConfig(ps parameters.Parameters, c Config, l loggers.Logger) Handler {
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
