package printer

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"github.com/pedrothome1/goscript/internal/types"
	"strconv"
	"strings"
)

const (
	indentSize = 4
)

type Printer struct {
	level      int
	comma      string
	prevCommas []string
	sb         strings.Builder
}

func (p *Printer) Stringify(expr ast.Expr) string {
	expr.Accept(p)
	defer p.sb.Reset()
	return p.sb.String()
}

// -- ExprVisitor --
func (p *Printer) VisitEllipsis(expr *ast.Ellipsis) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitFuncLit(lit *ast.FuncLit) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitCompositeLit(lit *ast.CompositeLit) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitSelectorExpr(expr *ast.SelectorExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitIndexExpr(expr *ast.IndexExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitSliceExpr(expr *ast.SliceExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitKeyValueExpr(expr *ast.KeyValueExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitSliceType(expr *ast.SliceType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitStructType(expr *ast.StructType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitInterfaceType(expr *ast.InterfaceType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitMapType(expr *ast.MapType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitFuncType(expr *ast.FuncType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitIdent(expr *ast.Ident) (*types.Object, error) {
	p.oneLinePrintf("Indent(%q)", expr.Name.Lexeme)

	return nil, nil
}

func (p *Printer) VisitBasicLit(lit *ast.BasicLit) (*types.Object, error) {
	switch lit.Value.Kind {
	case token.FLOAT:
		val := strconv.FormatFloat(lit.Value.Lit.(float64), 'f', -1, 64)
		p.oneLinePrintf("BasicLit(float(%s))", val)
	case token.INT:
		p.oneLinePrintf("BasicLit(int(%d))", lit.Value.Lit)
	case token.STRING:
		p.oneLinePrintf("BasicLit(string(%q))", lit.Value.Lit)
	case token.BOOL:
		p.oneLinePrintf("BasicLit(bool(%v))", lit.Value.Lit)
	case token.NIL:
		p.oneLinePrintf("BasicLit(%v)", lit.Value.Lit)
	}

	return nil, nil
}

func (p *Printer) VisitBinaryExpr(expr *ast.BinaryExpr) (*types.Object, error) {
	p.startElement("BinaryExpr")

	p.inMiddleElement()
	expr.Left.Accept(p)

	p.inMiddleElement()
	p.printf("%s%s%s\n", p.indentation(), expr.Op.Kind.String(), p.comma)

	p.inLastElement()
	expr.Right.Accept(p)

	p.endElement()

	return nil, nil
}

func (p *Printer) VisitUnaryExpr(expr *ast.UnaryExpr) (*types.Object, error) {
	p.startElement("UnaryExpr")

	p.inMiddleElement()
	p.printf("%s%s%s\n", p.indentation(), expr.Op.Kind.String(), p.comma)

	p.inLastElement()
	expr.Right.Accept(p)

	p.endElement()

	return nil, nil
}

func (p *Printer) VisitCallExpr(expr *ast.CallExpr) (*types.Object, error) {
	if len(expr.Args) == 0 {
		p.oneLinePrintf("CallExpr()")
		return nil, nil
	}

	p.startElement("CallExpr")

	for i := 0; i < len(expr.Args); i++ {
		if i == len(expr.Args)-1 {
			p.inLastElement()
		} else {
			p.inMiddleElement()
		}

		expr.Args[i].Accept(p)
	}

	p.endElement()

	return nil, nil
}

func (p *Printer) VisitParenExpr(expr *ast.ParenExpr) (*types.Object, error) {
	p.startElement("ParenExpr")

	p.inLastElement()
	expr.X.Accept(p)

	p.endElement()

	return nil, nil
}

// -- StmtVisitor --
func (p *Printer) VisitDeferStmt(stmt *ast.DeferStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitSwitchStmt(stmt *ast.SwitchStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitCaseClause(stmt *ast.CaseClause) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitForRangeStmt(stmt *ast.ForRangeStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitConstDecl(stmt *ast.ConstDecl) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitTypeDecl(stmt *ast.TypeDecl) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitPrintStmt(stmt *ast.PrintStmt) error {
	p.startElement("PrintStmt")
	stmt.Expr.Accept(p)
	p.endElement()

	return nil
}

func (p *Printer) VisitExprStmt(stmt *ast.ExprStmt) error {
	p.startElement("ExprStmt")
	stmt.Expr.Accept(p)
	p.endElement()

	return nil
}

func (p *Printer) VisitAssignStmt(stmt *ast.AssignStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitBlockStmt(stmt *ast.BlockStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitIfStmt(stmt *ast.IfStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitForStmt(stmt *ast.ForStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitReturnStmt(stmt *ast.ReturnStmt) error {
	p.startElement("ExprStmt")
	stmt.Result.Accept(p)
	p.endElement()

	return nil
}

func (p *Printer) VisitBranchStmt(stmt *ast.BranchStmt) error {
	switch stmt.Tok.Kind {
	case token.BREAK:
		p.oneLinePrintf("BreakStmt()")
	case token.CONTINUE:
		p.oneLinePrintf("ContinueStmt()")
	default:
		panic("invalid branch stmt")
	}

	return nil
}

func (p *Printer) VisitIncDecStmt(stmt *ast.IncDecStmt) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitVarDecl(stmt *ast.VarDecl) error {
	//TODO implement me
	panic("implement me")
}

func (p *Printer) VisitFuncDecl(stmt *ast.FuncDecl) error {
	//TODO implement me
	panic("implement me")
}

// -- helpers --
func (p *Printer) startElement(name string) error {
	err := p.printf("%s%s(\n", p.indentation(), name)
	if err != nil {
		return err
	}

	p.indent()

	return nil
}

func (p *Printer) endElement() error {
	p.unindent()
	return p.printf("%s)%s\n", p.indentation(), p.comma)
}

func (p *Printer) oneLinePrintf(format string, args ...any) (err error) {
	err = p.print(p.indentation())
	if err != nil {
		return err
	}

	err = p.printf(format, args...)
	if err != nil {
		return err
	}

	return p.printf("%s\n", p.comma)
}

func (p *Printer) printf(format string, args ...any) error {
	_, err := fmt.Fprintf(&p.sb, format, args...)
	return err
}

func (p *Printer) print(args ...any) error {
	_, err := fmt.Fprint(&p.sb, args...)
	return err
}

func (p *Printer) indentation() string {
	return strings.Repeat(" ", p.level)
}

func (p *Printer) indent() {
	p.level += indentSize
	p.prevCommas = append(p.prevCommas, p.comma)
}

func (p *Printer) unindent() {
	p.level -= indentSize

	p.comma = p.prevCommas[len(p.prevCommas)-1]
	// This will panic if p.prevCommas is empty
	p.prevCommas = p.prevCommas[:len(p.prevCommas)-1]
}

func (p *Printer) inLastElement() {
	p.comma = ""
}

func (p *Printer) inMiddleElement() {
	p.comma = ","
}
