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
	sb    strings.Builder
}

func (p *Printer) Visit(expr ast.Expr) (any, error) {
	switch e := expr.(type) {
	case *ast.FloatLit:
		p.visitFloatLit(e)
	case *ast.BinaryExpr:
		p.visitBinaryExpr(e)
	case *ast.UnaryExpr:
		p.visitUnaryExpr(e)
	case *ast.ParenExpr:
		p.visitParenExpr(e)
	}

	return p.sb.String(), nil
}

func (p *Printer) visitFloatLit(lit *ast.FloatLit) {
	fmt.Fprintf(&p.sb, "%sFloatLit(%.2f)%s\n", p.indent(), lit.Value.Lit, p.comma)
}

func (p *Printer) visitBinaryExpr(expr *ast.BinaryExpr) {
	fmt.Fprintf(&p.sb, "%sBinaryExpr(\n", p.indent())
	p.level += indent

	parentComma := p.comma

	p.comma = ","
	p.Visit(expr.Left)
	fmt.Fprintf(&p.sb, "%s%s%s\n", p.indent(), expr.Op.Kind.String(), p.comma)
	p.comma = ""
	p.Visit(expr.Right)
	p.comma = parentComma

	p.level -= indent
	fmt.Fprintf(&p.sb, "%s)%s\n", p.indent(), p.comma)
}

func (p *Printer) visitUnaryExpr(expr *ast.UnaryExpr) {
	if expr.Op.Kind == token.ILLEGAL {
		if lit, ok := expr.Right.(*ast.FloatLit); ok {
			p.visitFloatLit(lit)
		}
	}
}

func (p *Printer) visitParenExpr(expr *ast.ParenExpr) {
	fmt.Fprintf(&p.sb, "%sParenExpr(\n", p.indent())
	p.level += indent
	p.Visit(expr.X)
	p.level -= indent
	fmt.Fprintf(&p.sb, "%s)%s\n", p.indent(), p.comma)
}

func (p *Printer) indent() string {
	return strings.Repeat(" ", p.level)
}
