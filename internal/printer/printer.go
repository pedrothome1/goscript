package printer

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"strconv"
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
	defer p.sb.Reset()
	return p.sb.String()
}

func (p *Printer) VisitBasicLit(lit *ast.BasicLit) (any, error) {
	switch lit.Value.Kind {
	case token.FLOAT:
		val := strconv.FormatFloat(lit.Value.Lit.(float64), 'f', -1, 64)
		fmt.Fprintf(&p.sb, "%sBasicLit(float(%s))%s\n", p.indent(), val, p.comma)
	case token.INT:
		fmt.Fprintf(&p.sb, "%sBasicLit(int(%d))%s\n", p.indent(), lit.Value.Lit, p.comma)
	}

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
	// TODO:
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
