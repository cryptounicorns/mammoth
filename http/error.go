package http

import (
	"net/http"

	"github.com/corpix/loggers"
)

func HandleError(rw http.ResponseWriter, err error, l loggers.Logger, sendHeader bool) bool {
	if err != nil {
		l.Error(err)
		if sendHeader {
			rw.WriteHeader(500)
		}
		return true
	}

	return false
}
