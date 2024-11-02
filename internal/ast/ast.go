package ast

import (
	"github.com/pedrothome1/goscript/internal/token"
)

//go:generate go run ../../cmd/gen/gen.go

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

type Stmt interface {
	Accept(v StmtVisitor) error
	stmtNode()
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

// --- statements ---
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

type Field struct {
	Name *Ident
	Type Expr
}
