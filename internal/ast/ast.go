package ast

import (
	"github.com/pedrothome1/goscript/internal/token"
)

type ExprVisitor interface {
	VisitIdent(expr *Ident) (any, error)
	VisitEllipsis(expr *Ellipsis) (any, error)
	VisitBasicLit(lit *BasicLit) (any, error)
	VisitFuncLit(lit *FuncLit) (any, error)
	VisitCompositeLit(lit *CompositeLit) (any, error)
	VisitBinaryExpr(expr *BinaryExpr) (any, error)
	VisitUnaryExpr(expr *UnaryExpr) (any, error)
	VisitCallExpr(expr *CallExpr) (any, error)
	VisitParenExpr(expr *ParenExpr) (any, error)
	VisitSelectorExpr(expr *SelectorExpr) (any, error)
	VisitIndexExpr(expr *IndexExpr) (any, error)
	VisitSliceExpr(expr *SliceExpr) (any, error)
	VisitKeyValueExpr(expr *KeyValueExpr) (any, error)
	VisitSliceType(expr *SliceType) (any, error)
	VisitStructType(expr *StructType) (any, error)
	VisitInterfaceType(expr *InterfaceType) (any, error)
	VisitMapType(expr *MapType) (any, error)
	VisitFuncType(expr *FuncType) (any, error)
}

type StmtVisitor interface {
	VisitPrintStmt(stmt *PrintStmt) error
	VisitExprStmt(stmt *ExprStmt) error
	VisitAssignStmt(stmt *AssignStmt) error
	VisitDeferStmt(stmt *DeferStmt) error
	VisitSwitchStmt(stmt *SwitchStmt) error
	VisitCaseClause(stmt *CaseClause) error
	VisitBlockStmt(stmt *BlockStmt) error
	VisitIfStmt(stmt *IfStmt) error
	VisitForStmt(stmt *ForStmt) error
	VisitForRangeStmt(stmt *ForRangeStmt) error
	VisitReturnStmt(stmt *ReturnStmt) error
	VisitBranchStmt(stmt *BranchStmt) error
	VisitIncDecStmt(stmt *IncDecStmt) error
	VisitVarDecl(stmt *VarDecl) error
	VisitConstDecl(stmt *ConstDecl) error
	VisitFuncDecl(stmt *FuncDecl) error
	VisitTypeDecl(stmt *TypeDecl) error
}

// --- expressions ---
type Expr interface {
	Accept(v ExprVisitor) (any, error)
	exprNode()
}

type Ident struct {
	Name token.Token
}

type Ellipsis struct {
	Elt Expr
}

type BasicLit struct {
	Value token.Token
}

type FuncLit struct {
	Type *FuncType
	Body []Stmt
}

type CompositeLit struct {
	Type Expr
	Elts []Expr
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

type SelectorExpr struct {
	X   Expr
	Sel *Ident
}

type IndexExpr struct {
	X     Expr
	Index Expr
}

type SliceExpr struct {
	X    Expr
	Low  Expr
	High Expr
}

type KeyValueExpr struct {
	Key   Expr
	Value Expr
}

type SliceType struct {
	Elt Expr
}

type StructType struct {
	Fields []*Field
}

type InterfaceType struct {
	Methods []*Field
}

type MapType struct {
	Key   Expr
	Value Expr
}

type FuncType struct {
	Params []*Field
	Result *Field
}

func (e *Ident) Accept(v ExprVisitor) (any, error)         { return v.VisitIdent(e) }
func (e *Ident) exprNode()                                 {}
func (e *Ellipsis) Accept(v ExprVisitor) (any, error)      { return v.VisitEllipsis(e) }
func (e *Ellipsis) exprNode()                              {}
func (e *BasicLit) Accept(v ExprVisitor) (any, error)      { return v.VisitBasicLit(e) }
func (e *BasicLit) exprNode()                              {}
func (e *FuncLit) Accept(v ExprVisitor) (any, error)       { return v.VisitFuncLit(e) }
func (e *FuncLit) exprNode()                               {}
func (e *CompositeLit) Accept(v ExprVisitor) (any, error)  { return v.VisitCompositeLit(e) }
func (e *CompositeLit) exprNode()                          {}
func (e *BinaryExpr) Accept(v ExprVisitor) (any, error)    { return v.VisitBinaryExpr(e) }
func (e *BinaryExpr) exprNode()                            {}
func (e *UnaryExpr) Accept(v ExprVisitor) (any, error)     { return v.VisitUnaryExpr(e) }
func (e *UnaryExpr) exprNode()                             {}
func (e *CallExpr) Accept(v ExprVisitor) (any, error)      { return v.VisitCallExpr(e) }
func (e *CallExpr) exprNode()                              {}
func (e *ParenExpr) Accept(v ExprVisitor) (any, error)     { return v.VisitParenExpr(e) }
func (e *ParenExpr) exprNode()                             {}
func (e *SelectorExpr) Accept(v ExprVisitor) (any, error)  { return v.VisitSelectorExpr(e) }
func (e *SelectorExpr) exprNode()                          {}
func (e *IndexExpr) Accept(v ExprVisitor) (any, error)     { return v.VisitIndexExpr(e) }
func (e *IndexExpr) exprNode()                             {}
func (e *SliceExpr) Accept(v ExprVisitor) (any, error)     { return v.VisitSliceExpr(e) }
func (e *SliceExpr) exprNode()                             {}
func (e *KeyValueExpr) Accept(v ExprVisitor) (any, error)  { return v.VisitKeyValueExpr(e) }
func (e *KeyValueExpr) exprNode()                          {}
func (e *SliceType) Accept(v ExprVisitor) (any, error)     { return v.VisitSliceType(e) }
func (e *SliceType) exprNode()                             {}
func (e *StructType) Accept(v ExprVisitor) (any, error)    { return v.VisitStructType(e) }
func (e *StructType) exprNode()                            {}
func (e *InterfaceType) Accept(v ExprVisitor) (any, error) { return v.VisitInterfaceType(e) }
func (e *InterfaceType) exprNode()                         {}
func (e *MapType) Accept(v ExprVisitor) (any, error)       { return v.VisitMapType(e) }
func (e *MapType) exprNode()                               {}
func (e *FuncType) Accept(v ExprVisitor) (any, error)      { return v.VisitFuncType(e) }
func (e *FuncType) exprNode()                              {}

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
	Init Stmt
	Cond Expr
	Body *BlockStmt
	Else Stmt
}

type ForStmt struct {
	Init Stmt
	Cond Expr
	Post Stmt
	Body *BlockStmt
}

type ForRangeStmt struct {
	Key, Value Expr
	X          Expr // value to range over
	Body       *BlockStmt
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

type DeferStmt struct {
	Call *CallExpr
}

type SwitchStmt struct {
	Init Stmt
	Tag  Expr
	Body *BlockStmt
}

type CaseClause struct {
	List []Expr
	Body []Stmt
}

type VarDecl struct {
	Name  token.Token
	Type  Expr
	Value Expr
}

type ConstDecl struct {
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

type TypeDecl struct {
	Name *Ident
	Type Expr
}

func (s *PrintStmt) Accept(v StmtVisitor) error    { return v.VisitPrintStmt(s) }
func (s *PrintStmt) stmtNode()                     {}
func (s *ExprStmt) Accept(v StmtVisitor) error     { return v.VisitExprStmt(s) }
func (s *ExprStmt) stmtNode()                      {}
func (s *BlockStmt) Accept(v StmtVisitor) error    { return v.VisitBlockStmt(s) }
func (s *BlockStmt) stmtNode()                     {}
func (s *IfStmt) Accept(v StmtVisitor) error       { return v.VisitIfStmt(s) }
func (s *IfStmt) stmtNode()                        {}
func (s *ForStmt) Accept(v StmtVisitor) error      { return v.VisitForStmt(s) }
func (s *ForStmt) stmtNode()                       {}
func (s *ForRangeStmt) Accept(v StmtVisitor) error { return v.VisitForRangeStmt(s) }
func (s *ForRangeStmt) stmtNode()                  {}
func (s *ReturnStmt) Accept(v StmtVisitor) error   { return v.VisitReturnStmt(s) }
func (s *ReturnStmt) stmtNode()                    {}
func (s *BranchStmt) Accept(v StmtVisitor) error   { return v.VisitBranchStmt(s) }
func (s *BranchStmt) stmtNode()                    {}
func (s *IncDecStmt) Accept(v StmtVisitor) error   { return v.VisitIncDecStmt(s) }
func (s *IncDecStmt) stmtNode()                    {}
func (s *AssignStmt) Accept(v StmtVisitor) error   { return v.VisitAssignStmt(s) }
func (s *AssignStmt) stmtNode()                    {}
func (s *DeferStmt) Accept(v StmtVisitor) error    { return v.VisitDeferStmt(s) }
func (s *DeferStmt) stmtNode()                     {}
func (s *SwitchStmt) Accept(v StmtVisitor) error   { return v.VisitSwitchStmt(s) }
func (s *SwitchStmt) stmtNode()                    {}
func (s *CaseClause) Accept(v StmtVisitor) error   { return v.VisitCaseClause(s) }
func (s *CaseClause) stmtNode()                    {}
func (s *VarDecl) Accept(v StmtVisitor) error      { return v.VisitVarDecl(s) }
func (s *VarDecl) stmtNode()                       {}
func (s *ConstDecl) Accept(v StmtVisitor) error    { return v.VisitConstDecl(s) }
func (s *ConstDecl) stmtNode()                     {}
func (s *FuncDecl) Accept(v StmtVisitor) error     { return v.VisitFuncDecl(s) }
func (s *FuncDecl) stmtNode()                      {}
func (s *TypeDecl) Accept(v StmtVisitor) error     { return v.VisitTypeDecl(s) }
func (s *TypeDecl) stmtNode()                      {}

type Field struct {
	Name *Ident
	Type Expr
}
