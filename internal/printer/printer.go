package printer

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"strings"
)

const indent = 4

type Printer struct {
	level int
	comma string
}

func (p *Printer) Visit(expr ast.Expr) (any, error) {
	switch e := expr.(type) {
	case *ast.FloatLit:
		return p.visitFloatLit(e)
	case *ast.BinaryExpr:
		return p.visitBinaryExpr(e)
	case *ast.UnaryExpr:
		return p.visitUnaryExpr(e)
	case *ast.ParenExpr:
		return p.visitParenExpr(e)
	default:
		return nil, nil
	}
}

func (p *Printer) visitFloatLit(lit *ast.FloatLit) (any, error) {
	fmt.Printf("%sFloatLit(%.2f)%s\n", p.indent(), lit.Value.Lit, p.comma)
	return nil, nil
}

func (p *Printer) visitBinaryExpr(expr *ast.BinaryExpr) (any, error) {
	fmt.Printf("%sBinaryExpr(\n", p.indent())
	p.level += indent

	parentComma := p.comma

	p.comma = ","
	p.Visit(expr.Left)
	fmt.Printf("%s%s%s\n", p.indent(), expr.Op.Kind.String(), p.comma)
	p.comma = ""
	p.Visit(expr.Right)
	p.comma = parentComma

	p.level -= indent
	fmt.Printf("%s)%s\n", p.indent(), p.comma)

	return nil, nil
}

func (p *Printer) visitUnaryExpr(expr *ast.UnaryExpr) (any, error) {
	if expr.Op.Kind == token.ILLEGAL {
		if lit, ok := expr.Right.(*ast.FloatLit); ok {
			p.visitFloatLit(lit)
		}
	}
	return nil, nil
}

func (p *Printer) visitParenExpr(expr *ast.ParenExpr) (any, error) {
	fmt.Printf("%sParenExpr(\n", p.indent())
	p.level += indent
	p.Visit(expr.X)
	p.level -= indent
	fmt.Printf("%s)%s\n", p.indent(), p.comma)

	return nil, nil
}

func (p *Printer) indent() string {
	return strings.Repeat(" ", p.level)
}
