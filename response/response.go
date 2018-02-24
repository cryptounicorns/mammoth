package response

import (
	"net/http"

	"github.com/corpix/formats"
	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"

	"github.com/cryptounicorns/mammoth/response/builders"
)

type Response struct {
	config Config
	format formats.Format
	log    loggers.Logger
	build  builders.Builder
}

func (r *Response) Write(v interface{}, rw http.ResponseWriter, req *http.Request) error {
	var (
		// FIXME: Streaming(better - move to json streams)!
		res interface{}
		buf []byte
		err error
	)

	res, err = r.build(v)
	if err != nil {
		return err
	}

	buf, err = r.format.Marshal(res)
	if err != nil {
		return err
	}

	rw.Header().Set("content-type", "application/"+r.format.Name())
	rw.Write(buf)

	return nil
}

func FromConfig(c Config, l loggers.Logger) (*Response, error) {
	var (
		f   formats.Format
		b   builders.Builder
		err error
		log = prefixwrapper.New(
			"Response: ",
			l,
		)
	)

	f, err = formats.New(c.Format)
	if err != nil {
		return nil, err
	}

	b, err = builders.FromConfig(c.Builder, log)
	if err != nil {
		return nil, err
	}

	return &Response{
		config: c,
		format: f,
		log:    log,
		build:  b,
	}, nil
}

func MustFromConfig(c Config, l loggers.Logger) *Response {
	var (
		r   *Response
		err error
	)

	r, err = FromConfig(c, l)
	if err != nil {
		panic(err)
	}

	return r
}
