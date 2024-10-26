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
	VisitCallExpr(expr *CallExpr) (*types.Value, error)
	VisitParenExpr(expr *ParenExpr) (*types.Value, error)
}

type StmtVisitor interface {
	VisitPrintStmt(stmt *PrintStmt) error
	VisitExprStmt(stmt *ExprStmt) error
	VisitAssignStmt(stmt *AssignStmt) error
	VisitBlockStmt(stmt *BlockStmt) error
	VisitIfStmt(stmt *IfStmt) error
	VisitForStmt(stmt *ForStmt) error
	VisitReturnStmt(stmt *ReturnStmt) error
	VisitBranchStmt(stmt *BranchStmt) error
	VisitIncDecStmt(stmt *IncDecStmt) error
	VisitVarDecl(stmt *VarDecl) error
	VisitFuncDecl(stmt *FuncDecl) error
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

type CallExpr struct {
	Callee Expr
	Args   []Expr
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
func (e *CallExpr) Accept(v ExprVisitor) (*types.Value, error)   { return v.VisitCallExpr(e) }
func (e *CallExpr) exprNode()                                    {}
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

type BlockStmt struct {
	List []Stmt
}

type IfStmt struct {
	Cond Expr
	Body *BlockStmt
	Else Stmt
}

type ForStmt struct {
	Cond Expr
	Body *BlockStmt
}

type ReturnStmt struct {
	Result Expr
}

// break, continue
type BranchStmt struct {
	Tok   token.Token
	Label *Ident
}

type IncDecStmt struct {
	Expr Expr
	Tok  token.Token
}

type AssignStmt struct {
	Name  token.Token
	Value Expr
}

type VarDecl struct {
	Name  token.Token
	Type  Expr
	Value Expr
}

type FuncDecl struct {
	Name   token.Token
	Params []*Field
	Result *Field
	Body   []Stmt
}

func (s *PrintStmt) Accept(v StmtVisitor) error  { return v.VisitPrintStmt(s) }
func (s *PrintStmt) stmtNode()                   {}
func (s *ExprStmt) Accept(v StmtVisitor) error   { return v.VisitExprStmt(s) }
func (s *ExprStmt) stmtNode()                    {}
func (s *BlockStmt) Accept(v StmtVisitor) error  { return v.VisitBlockStmt(s) }
func (s *BlockStmt) stmtNode()                   {}
func (s *IfStmt) Accept(v StmtVisitor) error     { return v.VisitIfStmt(s) }
func (s *IfStmt) stmtNode()                      {}
func (s *ForStmt) Accept(v StmtVisitor) error    { return v.VisitForStmt(s) }
func (s *ForStmt) stmtNode()                     {}
func (s *ReturnStmt) Accept(v StmtVisitor) error { return v.VisitReturnStmt(s) }
func (s *ReturnStmt) stmtNode()                  {}
func (s *BranchStmt) Accept(v StmtVisitor) error { return v.VisitBranchStmt(s) }
func (s *BranchStmt) stmtNode()                  {}
func (s *IncDecStmt) Accept(v StmtVisitor) error { return v.VisitIncDecStmt(s) }
func (s *IncDecStmt) stmtNode()                  {}
func (s *AssignStmt) Accept(v StmtVisitor) error { return v.VisitAssignStmt(s) }
func (s *AssignStmt) stmtNode()                  {}
func (s *VarDecl) Accept(v StmtVisitor) error    { return v.VisitVarDecl(s) }
func (s *VarDecl) stmtNode()                     {}
func (s *FuncDecl) Accept(v StmtVisitor) error   { return v.VisitFuncDecl(s) }
func (s *FuncDecl) stmtNode()                    {}

type Field struct {
	Name token.Token
	Type token.Token
}
