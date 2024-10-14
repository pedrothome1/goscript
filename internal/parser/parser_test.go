package parser_test

import (
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"testing"
)

func TestParser_Parse(t *testing.T) {
	p := &parser.Parser{}
	p.Init([]byte("3 + 4 * 5"))
	expr, err := p.Parse()
	if err != nil {
		t.Errorf("err not nil: %s\n", err.Error())
	}

	pr := &printer.Printer{}
	pr.Visit(expr)
}
