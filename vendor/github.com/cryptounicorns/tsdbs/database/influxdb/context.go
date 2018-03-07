package influxdb

import (
	"strings"
)

var (
	// https://github.com/influxdata/influxdb/blob/390a16925d8bce2955ef7a27bc423762566cd931/pkg/escape/strings.go
	escaper = strings.NewReplacer(`,`, `\,`, `"`, `\"`, ` `, `\ `, `=`, `\=`)
)

type context struct {
	Parameters map[string]interface{}
}

func (c context) Escape(s string) string {
	return escaper.Replace(s)
}
