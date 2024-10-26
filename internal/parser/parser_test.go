package parser_test

import (
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/parser"
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
				&ast.BasicLit{token.Token{token.INT, 1, "1", -1, -1}},
				token.Token{token.ADD, nil, "+", -1, -1},
				&ast.BasicLit{token.Token{token.INT, 2, "2", -1, -1}},
			},
		},
		{
			Name: "Precende ADD, MUL",
			Src:  []byte("3 + 4 * 5"),
			Want: &ast.BinaryExpr{
				&ast.BasicLit{token.Token{token.INT, 3, "3", -1, -1}},
				token.Token{token.ADD, nil, "+", -1, -1},
				&ast.BinaryExpr{
					&ast.BasicLit{token.Token{token.INT, 4, "4", -1, -1}},
					token.Token{token.MUL, nil, "*", -1, -1},
					&ast.BasicLit{token.Token{token.INT, 5, "5", -1, -1}},
				},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL",
			Src:  []byte("(3 + 4) * 5"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.INT, 3, "3", -1, -1}},
						token.Token{token.ADD, nil, "+", -1, -1},
						&ast.BasicLit{token.Token{token.INT, 4, "4", -1, -1}},
					},
				},
				token.Token{token.MUL, nil, "*", -1, -1},
				&ast.BasicLit{token.Token{token.INT, 5, "5", -1, -1}},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL with floats",
			Src:  []byte("(3.2 + 4.1) * 5.659"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.FLOAT, 3.2, "3.2", -1, -1}},
						token.Token{token.ADD, nil, "+", -1, -1},
						&ast.BasicLit{token.Token{token.FLOAT, 4.1, "4.1", -1, -1}},
					},
				},
				token.Token{token.MUL, nil, "*", -1, -1},
				&ast.BasicLit{token.Token{token.FLOAT, 5.659, "5.659", -1, -1}},
			},
		},
		{
			Name: "Precence PAREN (ADD), MUL with ints and floats",
			Src:  []byte("(3.2 + 4) * 5.659"),
			Want: &ast.BinaryExpr{
				&ast.ParenExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.FLOAT, 3.2, "3.2", -1, -1}},
						token.Token{token.ADD, nil, "+", -1, -1},
						&ast.BasicLit{token.Token{token.INT, 4, "4", -1, -1}},
					},
				},
				token.Token{token.MUL, nil, "*", -1, -1},
				&ast.BasicLit{token.Token{token.FLOAT, 5.659, "5.659", -1, -1}},
			},
		},
		{
			Name: "Nested ADDs",
			Src:  []byte("10 + 11 + 12 + 13"),
			Want: &ast.BinaryExpr{
				&ast.BinaryExpr{
					&ast.BinaryExpr{
						&ast.BasicLit{token.Token{token.INT, 10, "10", -1, -1}},
						token.Token{token.ADD, nil, "+", -1, -1},
						&ast.BasicLit{token.Token{token.INT, 11, "11", -1, -1}},
					},
					token.Token{token.ADD, nil, "+", -1, -1},
					&ast.BasicLit{token.Token{token.INT, 12, "12", -1, -1}},
				},
				token.Token{token.ADD, nil, "+", -1, -1},
				&ast.BasicLit{token.Token{token.INT, 13, "13", -1, -1}},
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
			}
		})
	}
}
