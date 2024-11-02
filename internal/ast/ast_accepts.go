// Code generated by "go run ../../cmd/gen/gen.go"; DO NOT EDIT.

package ast

func (n *Ident) Accept(v ExprVisitor) (any, error) {
	return v.VisitIdent(n)
}

func (n *Ident) exprNode() {}

func (n *Ellipsis) Accept(v ExprVisitor) (any, error) {
	return v.VisitEllipsis(n)
}

func (n *Ellipsis) exprNode() {}

func (n *BasicLit) Accept(v ExprVisitor) (any, error) {
	return v.VisitBasicLit(n)
}

func (n *BasicLit) exprNode() {}

func (n *FuncLit) Accept(v ExprVisitor) (any, error) {
	return v.VisitFuncLit(n)
}

func (n *FuncLit) exprNode() {}

func (n *CompositeLit) Accept(v ExprVisitor) (any, error) {
	return v.VisitCompositeLit(n)
}

func (n *CompositeLit) exprNode() {}

func (n *BinaryExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitBinaryExpr(n)
}

func (n *BinaryExpr) exprNode() {}

func (n *UnaryExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitUnaryExpr(n)
}

func (n *UnaryExpr) exprNode() {}

func (n *CallExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitCallExpr(n)
}

func (n *CallExpr) exprNode() {}

func (n *ParenExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitParenExpr(n)
}

func (n *ParenExpr) exprNode() {}

func (n *SelectorExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitSelectorExpr(n)
}

func (n *SelectorExpr) exprNode() {}

func (n *IndexExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitIndexExpr(n)
}

func (n *IndexExpr) exprNode() {}

func (n *SliceExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitSliceExpr(n)
}

func (n *SliceExpr) exprNode() {}

func (n *KeyValueExpr) Accept(v ExprVisitor) (any, error) {
	return v.VisitKeyValueExpr(n)
}

func (n *KeyValueExpr) exprNode() {}

func (n *SliceType) Accept(v ExprVisitor) (any, error) {
	return v.VisitSliceType(n)
}

func (n *SliceType) exprNode() {}

func (n *StructType) Accept(v ExprVisitor) (any, error) {
	return v.VisitStructType(n)
}

func (n *StructType) exprNode() {}

func (n *InterfaceType) Accept(v ExprVisitor) (any, error) {
	return v.VisitInterfaceType(n)
}

func (n *InterfaceType) exprNode() {}

func (n *MapType) Accept(v ExprVisitor) (any, error) {
	return v.VisitMapType(n)
}

func (n *MapType) exprNode() {}

func (n *FuncType) Accept(v ExprVisitor) (any, error) {
	return v.VisitFuncType(n)
}

func (n *FuncType) exprNode() {}

func (n *PrintStmt) Accept(v StmtVisitor) error {
	return v.VisitPrintStmt(n)
}

func (n *PrintStmt) stmtNode() {}

func (n *ExprStmt) Accept(v StmtVisitor) error {
	return v.VisitExprStmt(n)
}

func (n *ExprStmt) stmtNode() {}

func (n *BlockStmt) Accept(v StmtVisitor) error {
	return v.VisitBlockStmt(n)
}

func (n *BlockStmt) stmtNode() {}

func (n *IfStmt) Accept(v StmtVisitor) error {
	return v.VisitIfStmt(n)
}

func (n *IfStmt) stmtNode() {}

func (n *ForStmt) Accept(v StmtVisitor) error {
	return v.VisitForStmt(n)
}

func (n *ForStmt) stmtNode() {}

func (n *ForRangeStmt) Accept(v StmtVisitor) error {
	return v.VisitForRangeStmt(n)
}

func (n *ForRangeStmt) stmtNode() {}

func (n *ReturnStmt) Accept(v StmtVisitor) error {
	return v.VisitReturnStmt(n)
}

func (n *ReturnStmt) stmtNode() {}

func (n *BranchStmt) Accept(v StmtVisitor) error {
	return v.VisitBranchStmt(n)
}

func (n *BranchStmt) stmtNode() {}

func (n *IncDecStmt) Accept(v StmtVisitor) error {
	return v.VisitIncDecStmt(n)
}

func (n *IncDecStmt) stmtNode() {}

func (n *AssignStmt) Accept(v StmtVisitor) error {
	return v.VisitAssignStmt(n)
}

func (n *AssignStmt) stmtNode() {}

func (n *DeferStmt) Accept(v StmtVisitor) error {
	return v.VisitDeferStmt(n)
}

func (n *DeferStmt) stmtNode() {}

func (n *SwitchStmt) Accept(v StmtVisitor) error {
	return v.VisitSwitchStmt(n)
}

func (n *SwitchStmt) stmtNode() {}

func (n *CaseClause) Accept(v StmtVisitor) error {
	return v.VisitCaseClause(n)
}

func (n *CaseClause) stmtNode() {}

func (n *VarDecl) Accept(v StmtVisitor) error {
	return v.VisitVarDecl(n)
}

func (n *VarDecl) stmtNode() {}

func (n *ConstDecl) Accept(v StmtVisitor) error {
	return v.VisitConstDecl(n)
}

func (n *ConstDecl) stmtNode() {}

func (n *FuncDecl) Accept(v StmtVisitor) error {
	return v.VisitFuncDecl(n)
}

func (n *FuncDecl) stmtNode() {}

func (n *TypeDecl) Accept(v StmtVisitor) error {
	return v.VisitTypeDecl(n)
}

func (n *TypeDecl) stmtNode() {}

