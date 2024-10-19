package ast

import (
	"github.com/pedrothome1/goscript/internal/token"
	"github.com/pedrothome1/goscript/internal/types"
)

type ExprVisitor interface {
	VisitIdent(expr *Ident) (*types.Value, error)
	VisitBasicLit(lit *BasicLit) (*types.Value, error)
	VisitBinaryExpr(expr *BinaryExpr) (*types.Value, error)
	VisitUnaryExpr(expr *UnaryExpr) (*types.Value, error)
	VisitParenExpr(expr *ParenExpr) (*types.Value, error)
}

type StmtVisitor interface {
	VisitAssignStmt(stmt *AssignStmt) error
	VisitPrintStmt(stmt *PrintStmt) error
	VisitExprStmt(stmt *ExprStmt) error
	VisitVarDecl(stmt *VarDecl) error
}

// --- expressions ---
type Expr interface {
	Accept(v ExprVisitor) (*types.Value, error)
	exprNode()
}

type Ident struct {
	Name token.Token
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

func (e *Ident) Accept(v ExprVisitor) (*types.Value, error)      { return v.VisitIdent(e) }
func (e *Ident) exprNode()                                       {}
func (e *BasicLit) Accept(v ExprVisitor) (*types.Value, error)   { return v.VisitBasicLit(e) }
func (e *BasicLit) exprNode()                                    {}
func (e *BinaryExpr) Accept(v ExprVisitor) (*types.Value, error) { return v.VisitBinaryExpr(e) }
func (e *BinaryExpr) exprNode()                                  {}
func (e *UnaryExpr) Accept(v ExprVisitor) (*types.Value, error)  { return v.VisitUnaryExpr(e) }
func (e *UnaryExpr) exprNode()                                   {}
func (e *ParenExpr) Accept(v ExprVisitor) (*types.Value, error)  { return v.VisitParenExpr(e) }
func (e *ParenExpr) exprNode()                                   {}

// --- statements ---
type Stmt interface {
	Accept(v StmtVisitor) error
	stmtNode()
}

type PrintStmt struct {
	Expr Expr
}

type ExprStmt struct {
	Expr Expr
}

type VarDecl struct {
	Name  token.Token
	Type  Expr
	Value Expr
}

type AssignStmt struct {
	Name  token.Token
	Value Expr
}

func (s *PrintStmt) Accept(v StmtVisitor) error  { return v.VisitPrintStmt(s) }
func (s *PrintStmt) stmtNode()                   {}
func (s *ExprStmt) Accept(v StmtVisitor) error   { return v.VisitExprStmt(s) }
func (s *ExprStmt) stmtNode()                    {}
func (s *AssignStmt) Accept(v StmtVisitor) error { return v.VisitAssignStmt(s) }
func (s *AssignStmt) stmtNode()                  {}
func (s *VarDecl) Accept(v StmtVisitor) error    { return v.VisitVarDecl(s) }
func (s *VarDecl) stmtNode()                     {}
