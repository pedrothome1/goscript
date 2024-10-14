package ast

import "github.com/pedrothome1/goscript/internal/token"

/*
FLOAT(3) ADD(<nil>) FLOAT(4) MUL(<nil>) FLOAT(5) EOF(<nil>)
                                 ^
BinaryExpr(
	UnaryExpr(nil, 3),
	ADD,

)
*/

type Visitor interface {
	VisitExpr(expr Expr) (any, error)
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

func (e *FloatLit) Accept(v Visitor) (any, error)   { return v.VisitExpr(e) }
func (e *FloatLit) exprNode()                       {}
func (e *BinaryExpr) Accept(v Visitor) (any, error) { return v.VisitExpr(e) }
func (e *BinaryExpr) exprNode()                     {}
func (e *UnaryExpr) Accept(v Visitor) (any, error)  { return v.VisitExpr(e) }
func (e *UnaryExpr) exprNode()                      {}
func (e *ParenExpr) Accept(v Visitor) (any, error)  { return v.VisitExpr(e) }
func (e *ParenExpr) exprNode()                      {}
