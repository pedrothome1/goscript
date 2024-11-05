package printer

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"reflect"
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

func (p *Printer) visitComposite(funcEl func(int) reflect.Value, funcLen func() int) {
	length := funcLen()
	for i := 0; i < length; i++ {
		if i == length-1 {
			p.inLastElement()
		} else {
			p.inMiddleElement()
		}
		p.visitValue(funcEl(i))
	}
}

func (p *Printer) visitValue(val reflect.Value) {
	typ := val.Type()

	if typ == reflect.TypeFor[token.Token]() {
		met := val.Addr().MethodByName("String")
		res := met.Call(make([]reflect.Value, 0))
		p.printf("%s%s%s\n", p.indentation(), res[0].String(), p.comma)
		return
	}

	for typ.Kind() == reflect.Pointer || typ.Kind() == reflect.Interface {
		val = val.Elem()
		if !val.IsValid() {
			return
		}
		typ = val.Type()
	}

	if typ.Kind() == reflect.Struct {
		p.startElement(typ.String())
		p.visitComposite(val.Field, val.NumField)
		p.endElement()
	} else if typ.Kind() == reflect.Slice || typ.Kind() == reflect.Array {
		p.visitComposite(val.Index, val.Len)
	}
}

func (p *Printer) StmtString(stmt ast.Stmt) string {
	stmt.Accept(p)
	defer p.sb.Reset()
	return p.sb.String()
}

func (p *Printer) StmtsString(stmts []ast.Stmt) string {
	defer p.sb.Reset()
	for _, s := range stmts {
		s.Accept(p)
	}
	return p.sb.String()
}

// -- ExprVisitor --
func (p *Printer) VisitEllipsis(expr *ast.Ellipsis) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitFuncLit(lit *ast.FuncLit) (any, error) {
	p.visitValue(reflect.ValueOf(lit))
	return nil, nil
}

func (p *Printer) VisitCompositeLit(lit *ast.CompositeLit) (any, error) {
	p.visitValue(reflect.ValueOf(lit))
	return nil, nil
}

func (p *Printer) VisitSelectorExpr(expr *ast.SelectorExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitIndexExpr(expr *ast.IndexExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitSliceExpr(expr *ast.SliceExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitKeyValueExpr(expr *ast.KeyValueExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitSliceType(expr *ast.SliceType) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitStructType(expr *ast.StructType) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitInterfaceType(expr *ast.InterfaceType) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitMapType(expr *ast.MapType) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitFuncType(expr *ast.FuncType) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitIdent(expr *ast.Ident) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitBasicLit(lit *ast.BasicLit) (any, error) {
	p.visitValue(reflect.ValueOf(lit))
	return nil, nil
}

func (p *Printer) VisitBinaryExpr(expr *ast.BinaryExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitUnaryExpr(expr *ast.UnaryExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitCallExpr(expr *ast.CallExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

func (p *Printer) VisitParenExpr(expr *ast.ParenExpr) (any, error) {
	p.visitValue(reflect.ValueOf(expr))
	return nil, nil
}

// -- StmtVisitor --
func (p *Printer) VisitDeferStmt(stmt *ast.DeferStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitSwitchStmt(stmt *ast.SwitchStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitCaseClause(stmt *ast.CaseClause) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitForRangeStmt(stmt *ast.ForRangeStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitConstDecl(stmt *ast.ConstDecl) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitTypeDecl(stmt *ast.TypeDecl) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitPrintStmt(stmt *ast.PrintStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitExprStmt(stmt *ast.ExprStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitAssignStmt(stmt *ast.AssignStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitBlockStmt(stmt *ast.BlockStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitIfStmt(stmt *ast.IfStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitForStmt(stmt *ast.ForStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitReturnStmt(stmt *ast.ReturnStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitBranchStmt(stmt *ast.BranchStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitIncDecStmt(stmt *ast.IncDecStmt) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitVarDecl(stmt *ast.VarDecl) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
}

func (p *Printer) VisitFuncDecl(stmt *ast.FuncDecl) error {
	p.visitValue(reflect.ValueOf(stmt))
	return nil
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
