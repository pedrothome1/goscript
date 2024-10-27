package resolver

import (
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"github.com/pedrothome1/goscript/internal/types"
)

func New() *Resolver {
	var r Resolver
	r.globals = map[string]*types.Value{
		"int":    {Type: types.TypeT, Native: types.Int},
		"float":  {Type: types.TypeT, Native: types.Float},
		"string": {Type: types.TypeT, Native: types.String},
		"char":   {Type: types.TypeT, Native: types.Int},
		"bool":   {Type: types.TypeT, Native: types.Bool},
	}

	return &r
}

type Resolver struct {
	globals map[string]*types.Value
	locals  []map[string]*types.Value
	fn      *ast.FuncDecl
	ret     *ast.ReturnStmt
	loop    *ast.ForStmt
	block   *ast.BlockStmt
}

func (r *Resolver) Resolve(program []ast.Stmt) error {
	for _, stmt := range program {
		if err := stmt.Accept(r); err != nil {
			return err
		}
	}
	return nil
}

func (r *Resolver) declareSymbolAt(scope map[string]*types.Value, name string, value *types.Value) error {
	if _, ok := scope[name]; ok {
		return resolveErrorf("%q already declared in scope", name)
	}

	scope[name] = value

	return nil
}

func (r *Resolver) declareSymbol(name string, value *types.Value) error {
	if len(r.locals) == 0 {
		return r.declareSymbolAt(r.globals, name, value)
	}

	return r.declareSymbolAt(r.locals[len(r.locals)-1], name, value)
}

func (r *Resolver) symbolDeclared(symbol string) (*types.Value, bool) {
	if sym, ok := r.globals[symbol]; ok {
		return sym, true
	}
	if len(r.locals) == 0 {
		return nil, false
	}
	for i := len(r.locals) - 1; i >= 0; i-- {
		if sym, ok := r.locals[i][symbol]; ok {
			return sym, true
		}
	}
	return nil, false
}

func (r *Resolver) visitField(kind string, field *ast.Field) error {
	if field == nil {
		return nil
	}
	sym, ok := r.symbolDeclared(field.Type.Lexeme)
	if !ok {
		return resolveErrorf("%s %q declared with invalid type", kind, field.Type.Lexeme)
	}
	if sym.Type != types.TypeT {
		if field.Name.Lexeme != "" {
			return resolveErrorf("%s %q declared with non-type %q", kind, field.Name.Lexeme, field.Type.Lexeme)
		}
		return resolveErrorf("%s declared with non-type %q", kind, field.Type.Lexeme)
	}
	return nil
}

func (r *Resolver) VisitIdent(expr *ast.Ident) (*types.Value, error) {
	if sym, ok := r.symbolDeclared(expr.Name.Lexeme); ok {
		return sym, nil
	}

	return nil, resolveErrorf("undeclared symbol %q at %d:%d", expr.Name.Lexeme, expr.Name.Line, expr.Name.Col)
}

func (r *Resolver) VisitBasicLit(lit *ast.BasicLit) (*types.Value, error) {
	return types.NewBasicValue(lit.Value.Lit), nil
}

func (r *Resolver) VisitBinaryExpr(expr *ast.BinaryExpr) (*types.Value, error) {
	left, err := expr.Left.Accept(r)
	if err != nil {
		return nil, err
	}

	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	var typ types.Type

	if left.Type == types.String && right.Type == types.String {
		typ = types.String
		if expr.Op.Kind == token.ADD {
			return &types.Value{Type: typ}, nil
		}
	}

	if left.Type == types.Bool && right.Type == types.Bool {
		typ = types.Bool
		switch expr.Op.Kind {
		case token.LAND, token.LOR:
			return &types.Value{Type: typ}, nil
		}
	}

	if left.Type == types.Int && right.Type == types.Int {
		typ = types.Int
		switch expr.Op.Kind {
		case token.AND, token.OR, token.XOR, token.SHL, token.SHR, token.AND_NOT:
			return &types.Value{Type: typ}, nil
		}
	}

	if left.Type.IsNumeric() && right.Type.IsNumeric() {
		if typ != types.Int {
			typ = types.Float
		}
		switch expr.Op.Kind {
		case token.ADD, token.SUB, token.MUL, token.QUO, token.REM:
			return &types.Value{Type: typ}, nil
		}
	}

	switch expr.Op.Kind {
	case token.EQL, token.NEQ:
		return &types.Value{Type: types.Bool}, nil
	case token.LSS, token.GTR, token.LEQ, token.GEQ:
		if typ != types.Bool {
			return &types.Value{Type: types.Bool}, nil
		}
	}

	return nil, resolveErrorf("binary operator %q not supported on type", expr.Op.Lexeme)
}

func (r *Resolver) VisitUnaryExpr(expr *ast.UnaryExpr) (*types.Value, error) {
	v, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	if expr.Op.Kind == token.NOT {
		if v.Type != types.Bool {
			return nil, resolveErrorf("'!' operator can only be used with boolean values")
		}

		return v, nil
	}

	if expr.Op.Kind == token.SUB {
		if v.Type != types.Int && v.Type != types.Float {
			return nil, resolveErrorf("unary '-' can only be used with numbers")
		}

		return v, nil
	}

	return nil, resolveErrorf("unrecognized unary operator %q", expr.Op.Lexeme)
}

func (r *Resolver) VisitCallExpr(expr *ast.CallExpr) (*types.Value, error) {
	if ident, ok := expr.Callee.(*ast.Ident); ok {
		sym, ok := r.symbolDeclared(ident.Name.Lexeme)
		if !ok {
			return nil, resolveErrorf("calling undeclared callable %q at %d:%d", ident.Name.Lexeme, ident.Name.Line, ident.Name.Col)
		}

		if sym.Type != types.Func {
			return nil, resolveErrorf("calling non-function object %q at %d:%d", ident.Name.Lexeme, ident.Name.Line, ident.Name.Col)
		}

		fnDecl := sym.Native.(*ast.FuncDecl)
		retType := types.Invalid
		if fnDecl.Result != nil {
			retType = types.FromLexeme(fnDecl.Result.Type.Lexeme)
		}
		return &types.Value{
			Type: retType,
		}, nil
	}

	if call, ok := expr.Callee.(*ast.CallExpr); ok {
		return r.VisitCallExpr(call)
	}

	return nil, resolveErrorf("unrecognized callable")
}

func (r *Resolver) VisitParenExpr(expr *ast.ParenExpr) (*types.Value, error) {
	return expr.X.Accept(r)
}

func (r *Resolver) VisitPrintStmt(stmt *ast.PrintStmt) error {
	_, err := stmt.Expr.Accept(r)
	return err
}

func (r *Resolver) VisitExprStmt(stmt *ast.ExprStmt) error {
	_, err := stmt.Expr.Accept(r)
	return err
}

func (r *Resolver) VisitAssignStmt(stmt *ast.AssignStmt) error {
	sym, ok := r.symbolDeclared(stmt.Name.Lexeme)
	if !ok {
		return resolveErrorf("assignment to undeclared variable %q", stmt.Name.Lexeme)
	}

	v, err := stmt.Value.Accept(r)
	if err != nil {
		return err
	}

	if sym.Type != v.Type {
		return resolveErrorf("variable %q assigned with wrong value type", stmt.Name.Lexeme)
	}

	return nil
}

func (r *Resolver) VisitBlockStmt(stmt *ast.BlockStmt) error {
	r.block = stmt
	r.locals = append(r.locals, make(map[string]*types.Value))
	defer func() {
		r.block = nil
		r.locals = r.locals[:len(r.locals)-1]
	}()

	for _, s := range stmt.List {
		if err := s.Accept(r); err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) VisitIfStmt(stmt *ast.IfStmt) error {
	cond, err := stmt.Cond.Accept(r)
	if err != nil {
		return err
	}

	if cond.Type != types.Bool {
		return resolveErrorf("'if' condition should be a boolean expression")
	}

	if stmt.Body == nil {
		return resolveErrorf("'if' without body")
	}

	err = r.VisitBlockStmt(stmt.Body)
	if err != nil {
		return err
	}

	if stmt.Else != nil {
		switch e := stmt.Else.(type) {
		case *ast.BlockStmt:
			return r.VisitBlockStmt(e)
		case *ast.IfStmt:
			return r.VisitIfStmt(e)
		default:
			return resolveErrorf("invalid 'else' statement, should be either 'if' or block")
		}
	}

	return nil
}

func (r *Resolver) VisitForStmt(stmt *ast.ForStmt) error {
	cond, err := stmt.Cond.Accept(r)
	if err != nil {
		return err
	}

	if cond.Type != types.Bool {
		return resolveErrorf("'for' condition should be a boolean expression")
	}

	if stmt.Body == nil {
		return resolveErrorf("'for' without body")
	}

	r.loop = stmt
	defer func() {
		r.loop = nil
	}()

	return r.VisitBlockStmt(stmt.Body)
}

func (r *Resolver) VisitReturnStmt(stmt *ast.ReturnStmt) error {
	if r.fn == nil {
		return resolveErrorf("'return' statement outside of function")
	}

	if r.fn.Result == nil && stmt.Result == nil {
		return nil
	}

	if stmt.Result == nil {
		return resolveErrorf("naked 'return', expected return type: %s", r.fn.Result.Type.Lexeme)
	}

	v, err := stmt.Result.Accept(r)
	if err != nil {
		return err
	}

	if v.Type != types.FromLexeme(r.fn.Result.Type.Lexeme) {
		return resolveErrorf("wrong return type, expected: %s", r.fn.Result.Type.Lexeme)
	}

	r.ret = stmt

	return nil
}

func (r *Resolver) VisitBranchStmt(stmt *ast.BranchStmt) error {
	if stmt.Tok.Kind == token.BREAK || stmt.Tok.Kind == token.CONTINUE {
		if r.loop == nil {
			return resolveErrorf("%q should be used inside loops", stmt.Tok.Lexeme)
		}
		return nil
	}
	return resolveErrorf("%q branch statement not implemented", stmt.Tok.Lexeme)
}

func (r *Resolver) VisitIncDecStmt(stmt *ast.IncDecStmt) error {
	v, err := stmt.Expr.Accept(r)
	if err != nil {
		return err
	}

	if v.Type != types.Int && v.Type != types.Float {
		return resolveErrorf("%q operator can only be used on number variables", stmt.Tok.Lexeme)
	}

	return nil
}

func (r *Resolver) VisitVarDecl(stmt *ast.VarDecl) error {
	declaredType := types.Invalid

	if stmt.Type != nil {
		typ, ok := stmt.Type.(*ast.Ident)
		if !ok {
			return resolveErrorf("variable %q declared with invalid type", stmt.Name.Lexeme)
		}
		sym, ok := r.symbolDeclared(typ.Name.Lexeme)
		if !ok {
			return resolveErrorf("variable %q declared with undeclared type %q", stmt.Name.Lexeme, typ.Name.Lexeme)
		}
		if sym.Type != types.TypeT {
			return resolveErrorf("variable %q declared with non-type %q", stmt.Name.Lexeme, typ.Name.Lexeme)
		}
		declaredType = sym.Native.(types.Type)
	}

	v, err := stmt.Value.Accept(r)
	if err != nil {
		return err
	}

	if declaredType != types.Invalid && declaredType != v.Type {
		return resolveErrorf("%q type and initializer don't match", stmt.Name.Lexeme)
	}

	return r.declareSymbol(stmt.Name.Lexeme, v)
}

func (r *Resolver) VisitFuncDecl(stmt *ast.FuncDecl) error {
	if r.fn != nil {
		return resolveErrorf("can't declare a function inside another function")
	}

	if r.block != nil {
		return resolveErrorf("can't declare a function inside a block")
	}

	if _, ok := r.symbolDeclared(stmt.Name.Lexeme); ok {
		return resolveErrorf("symbol %q already declared in this scope", stmt.Name.Lexeme)
	}

	for _, param := range stmt.Params {
		if err := r.visitField("parameter", param); err != nil {
			return err
		}
	}

	if err := r.visitField("return type", stmt.Result); err != nil {
		return err
	}

	locals := make(map[string]*types.Value, len(stmt.Params))
	for _, param := range stmt.Params {
		locals[param.Name.Lexeme] = &types.Value{Type: types.FromLexeme(param.Type.Lexeme)}
	}

	r.locals = append(r.locals, locals)
	r.fn = stmt
	defer func() {
		r.fn = nil
		r.ret = nil
		r.locals = r.locals[:len(r.locals)-1]
	}()

	for _, s := range stmt.Body {
		if err := s.Accept(r); err != nil {
			return err
		}
	}

	if stmt.Result != nil && r.ret == nil {
		return resolveErrorf("function %q at %d:%d should return the declared return type", stmt.Name.Lexeme, stmt.Name.Line, stmt.Name.Col)
	}

	return r.declareSymbolAt(r.globals, stmt.Name.Lexeme, &types.Value{Type: types.Func, Native: stmt})
}
