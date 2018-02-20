package http

import (
	"net/http"

	"github.com/corpix/loggers"
	"github.com/corpix/loggers/logger/prefixwrapper"
	"github.com/gorilla/mux"
)

type Server struct {
	Config Config
	router *mux.Router
	log    loggers.Logger
}

func (s *Server) Serve() error {
	s.log.Printf(
		"Starting server on '%s'...",
		s.Config.Addr,
	)

	return http.ListenAndServe(
		s.Config.Addr,
		s.router,
	)
}

func New(c Config, r *mux.Router, l loggers.Logger) *Server {
	return &Server{
		Config: c,
		router: r,
		log: prefixwrapper.New(
			"HTTP: ",
			l,
		),
	}
}
