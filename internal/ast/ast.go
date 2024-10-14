package ast

import (
	"github.com/pedrothome1/goscript/internal/token"
)

type Visitor interface {
	VisitFloatLit(lit *FloatLit) (any, error)
	VisitBinaryExpr(expr *BinaryExpr) (any, error)
	VisitUnaryExpr(expr *UnaryExpr) (any, error)
	VisitParenExpr(expr *ParenExpr) (any, error)
}

// --- expressions ---
type Expr interface {
	Accept(v Visitor) (any, error)
	exprNode()
}

type FloatLit struct {
	Value token.Token
}

type BinaryExpr struct {
	Left  Expr
	Op    token.Token
	Right Expr
}

type UnaryExpr struct {
	Op    token.Token
	Right Expr
}

type ParenExpr struct {
	X Expr
}

func (e *FloatLit) Accept(v Visitor) (any, error)   { return v.VisitFloatLit(e) }
func (e *FloatLit) exprNode()                       {}
func (e *BinaryExpr) Accept(v Visitor) (any, error) { return v.VisitBinaryExpr(e) }
func (e *BinaryExpr) exprNode()                     {}
func (e *UnaryExpr) Accept(v Visitor) (any, error)  { return v.VisitUnaryExpr(e) }
func (e *UnaryExpr) exprNode()                      {}
func (e *ParenExpr) Accept(v Visitor) (any, error)  { return v.VisitParenExpr(e) }
func (e *ParenExpr) exprNode()                      {}
