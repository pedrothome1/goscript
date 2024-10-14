package parser_test

import (
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"testing"
)

func TestParser_Parse(t *testing.T) {
	tests := []struct {
		Src []byte
	}{
		{
			Src: []byte("3 + 4 * 5"),
		},
		{
			Src: []byte("(3 + 4) * 5"),
		},
	}

	for _, test := range tests {
		p := &parser.Parser{}
		p.Init(test.Src)
		expr, err := p.Parse()
		if err != nil {
			t.Errorf("err not nil: %s\n", err.Error())
		}

		pr := &printer.Printer{}
		pr.Visit(expr)
	}
}
