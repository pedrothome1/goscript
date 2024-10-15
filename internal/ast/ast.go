package ast

import (
	"github.com/pedrothome1/goscript/internal/token"
)

type Visitor interface {
	VisitBasicLit(lit *BasicLit) (any, error)
	VisitBinaryExpr(expr *BinaryExpr) (any, error)
	VisitUnaryExpr(expr *UnaryExpr) (any, error)
	VisitParenExpr(expr *ParenExpr) (any, error)
}

// --- expressions ---
type Expr interface {
	Accept(v Visitor) (any, error)
	exprNode()
}

type BasicLit struct {
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

func (e *BasicLit) Accept(v Visitor) (any, error)   { return v.VisitBasicLit(e) }
func (e *BasicLit) exprNode()                       {}
func (e *BinaryExpr) Accept(v Visitor) (any, error) { return v.VisitBinaryExpr(e) }
func (e *BinaryExpr) exprNode()                     {}
func (e *UnaryExpr) Accept(v Visitor) (any, error)  { return v.VisitUnaryExpr(e) }
func (e *UnaryExpr) exprNode()                      {}
func (e *ParenExpr) Accept(v Visitor) (any, error)  { return v.VisitParenExpr(e) }
func (e *ParenExpr) exprNode()                      {}
