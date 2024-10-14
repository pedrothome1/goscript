package parser_test

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/parser"
	"github.com/pedrothome1/goscript/internal/printer"
	"github.com/pedrothome1/goscript/internal/token"
	"reflect"
	"testing"
)

func TestParser_Parse(t *testing.T) {
	tests := []struct {
		Name string
		Src  []byte
		Want ast.Expr
	}{
		{
			Name: "Simple ADD",
			Src:  []byte("1 + 2"),
			Want: &ast.BinaryExpr{
				floatLit(1),
				op(token.ADD),
				floatLit(2),
			},
		},
		{
			Name: "Precende ADD, MUL",
			Src:  []byte("3 + 4 * 5"),
			Want: &ast.BinaryExpr{
				floatLit(3),
				op(token.ADD),
				&ast.BinaryExpr{
					floatLit(4),
					op(token.MUL),
					floatLit(5),
				},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL",
			Src:  []byte("(3 + 4) * 5"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						floatLit(3),
						op(token.ADD),
						floatLit(4),
					},
				},
				op(token.MUL),
				floatLit(5),
			},
		},
		{
			Name: "Nested ADDs",
			Src:  []byte("10 + 11 + 12 + 13"),
			Want: &ast.BinaryExpr{
				&ast.BinaryExpr{
					&ast.BinaryExpr{
						floatLit(10),
						op(token.ADD),
						floatLit(11),
					},
					op(token.ADD),
					floatLit(12),
				},
				op(token.ADD),
				floatLit(13),
			},
		},
	}
	pr := &printer.Printer{}

	for _, test := range tests {
		t.Run(test.Name, func(t *testing.T) {
			p := &parser.Parser{}
			p.Init(test.Src)
			expr, err := p.Parse()
			if err != nil {
				t.Errorf("err not nil: %s\n", err.Error())
			}

			if !reflect.DeepEqual(expr, test.Want) {
				t.Errorf("want != got")
			} else {
				str, _ := pr.Visit(expr)
				fmt.Print(str)
			}
		})
	}
}

func floatLit(num float64) *ast.FloatLit {
	return &ast.FloatLit{token.Token{token.FLOAT, num}}
}

func op(t token.Kind) token.Token {
	return token.Token{t, nil}
}
