package template

import (
	"bytes"
	"text/template"
)

type Template struct {
	t *template.Template
}

func (t *Template) Apply(ctx interface{}) ([]byte, error) {
	var (
		buf = bytes.NewBuffer(nil)
		err error
	)

	err = t.t.Execute(buf, ctx)
	if err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

func Parse(tpl string) (*Template, error) {
	var (
		t   *template.Template
		err error
	)

	t, err = template.New("").Parse(tpl)
	if err != nil {
		return nil, err
	}

	return &Template{
		t: t,
	}, nil
}
