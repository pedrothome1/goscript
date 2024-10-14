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

func (p *Printer) String(expr ast.Expr) string {
	expr.Accept(p)
	return p.sb.String()
}

func (p *Printer) VisitFloatLit(lit *ast.FloatLit) (any, error) {
	fmt.Fprintf(&p.sb, "%sFloatLit(%.2f)%s\n", p.indent(), lit.Value.Lit, p.comma)
	return nil, nil
}

func (p *Printer) VisitBinaryExpr(expr *ast.BinaryExpr) (any, error) {
	fmt.Fprintf(&p.sb, "%sBinaryExpr(\n", p.indent())
	p.level += indent

	parentComma := p.comma

	p.comma = ","
	expr.Left.Accept(p)
	fmt.Fprintf(&p.sb, "%s%s%s\n", p.indent(), expr.Op.Kind.String(), p.comma)
	p.comma = ""
	expr.Right.Accept(p)
	p.comma = parentComma

	p.level -= indent
	fmt.Fprintf(&p.sb, "%s)%s\n", p.indent(), p.comma)
	return nil, nil
}

func (p *Printer) VisitUnaryExpr(expr *ast.UnaryExpr) (any, error) {
	if expr.Op.Kind == token.ILLEGAL {
		if lit, ok := expr.Right.(*ast.FloatLit); ok {
			p.VisitFloatLit(lit)
		}
	}
	return nil, nil
}

func (p *Printer) VisitParenExpr(expr *ast.ParenExpr) (any, error) {
	fmt.Fprintf(&p.sb, "%sParenExpr(\n", p.indent())
	p.level += indent
	expr.X.Accept(p)
	p.level -= indent
	fmt.Fprintf(&p.sb, "%s)%s\n", p.indent(), p.comma)
	return nil, nil
}

func (p *Printer) indent() string {
	return strings.Repeat(" ", p.level)
}
