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
				&ast.BasicLit{token.Token{token.INT, 1, "1", 0}},
				token.Token{token.ADD, nil, "+", 0},
				&ast.BasicLit{token.Token{token.INT, 2, "2", 0}},
			},
		},
		{
			Name: "Precende ADD, MUL",
			Src:  []byte("3 + 4 * 5"),
			Want: &ast.BinaryExpr{
				&ast.BasicLit{token.Token{token.INT, 3, "3", 0}},
				token.Token{token.ADD, nil, "+", 0},
				&ast.BinaryExpr{
					&ast.BasicLit{token.Token{token.INT, 4, "4", 0}},
					token.Token{token.MUL, nil, "*", 0},
					&ast.BasicLit{token.Token{token.INT, 5, "5", 0}},
				},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL",
			Src:  []byte("(3 + 4) * 5"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.INT, 3, "3", 0}},
						token.Token{token.ADD, nil, "+", 0},
						&ast.BasicLit{token.Token{token.INT, 4, "4", 0}},
					},
				},
				token.Token{token.MUL, nil, "*", 0},
				&ast.BasicLit{token.Token{token.INT, 5, "5", 0}},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL with floats",
			Src:  []byte("(3.2 + 4.1) * 5.659"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.FLOAT, 3.2, "3.2", 0}},
						token.Token{token.ADD, nil, "+", 0},
						&ast.BasicLit{token.Token{token.FLOAT, 4.1, "4.1", 0}},
					},
				},
				token.Token{token.MUL, nil, "*", 0},
				&ast.BasicLit{token.Token{token.FLOAT, 5.659, "5.659", 0}},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL with ints and floats",
			Src:  []byte("(3.2 + 4) * 5.659"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.FLOAT, 3.2, "3.2", 0}},
						token.Token{token.ADD, nil, "+", 0},
						&ast.BasicLit{token.Token{token.INT, 4, "4", 0}},
					},
				},
				token.Token{token.MUL, nil, "*", 0},
				&ast.BasicLit{token.Token{token.FLOAT, 5.659, "5.659", 0}},
			},
		},
		{
			Name: "Nested ADDs",
			Src:  []byte("10 + 11 + 12 + 13"),
			Want: &ast.BinaryExpr{
				&ast.BinaryExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.INT, 10, "10", 0}},
						token.Token{token.ADD, nil, "+", 0},
						&ast.BasicLit{token.Token{token.INT, 11, "11", 0}},
					},
					token.Token{token.ADD, nil, "+", 0},
					&ast.BasicLit{token.Token{token.INT, 12, "12", 0}},
				},
				token.Token{token.ADD, nil, "+", 0},
				&ast.BasicLit{token.Token{token.INT, 13, "13", 0}},
			},
		},
	}

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
				pr := &printer.Printer{}
				fmt.Println(string(test.Src))
				fmt.Print(pr.String(expr))
			}
		})
	}
}
