package interpreter

import (
	"errors"
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"github.com/pedrothome1/goscript/internal/types"
	"math"
	"strconv"
	"strings"
)

type Interpreter struct {
	globals *Environment
	env     *Environment
}

func New() *Interpreter {
	globals := newEnvironment(nil)
	return &Interpreter{globals: globals, env: globals}
}

func (r *Interpreter) Run(stmt ast.Stmt) error {
	return stmt.Accept(r)
}

func (r *Interpreter) RunProgram(list []ast.Stmt) error {
	for _, stmt := range list {
		err := r.Run(stmt)
		if err != nil {
			return err
		}
	}
	return nil
}

func (r *Interpreter) RunBlock(list []ast.Stmt, env *Environment) error {
	defer func(prev *Environment) {
		r.env = prev
	}(r.env)

	r.env = env
	for _, stmt := range list {
		err := r.Run(stmt)
		if err != nil {
			return err
		}
	}
	return nil
}

func (r *Interpreter) Eval(expr ast.Expr) (*types.Object, error) {
	result, err := expr.Accept(r)
	if err != nil {
		return types.NewBasic(nil), err
	}
	return result, nil
}

func (r *Interpreter) VisitEllipsis(expr *ast.Ellipsis) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitFuncLit(lit *ast.FuncLit) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitCompositeLit(lit *ast.CompositeLit) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitSelectorExpr(expr *ast.SelectorExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitIndexExpr(expr *ast.IndexExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitSliceExpr(expr *ast.SliceExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitKeyValueExpr(expr *ast.KeyValueExpr) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitSliceType(expr *ast.SliceType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitStructType(expr *ast.StructType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitInterfaceType(expr *ast.InterfaceType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitMapType(expr *ast.MapType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitFuncType(expr *ast.FuncType) (*types.Object, error) {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitIdent(expr *ast.Ident) (*types.Object, error) {
	return r.env.Get(expr.Name.Lexeme)
}

func (r *Interpreter) VisitBasicLit(lit *ast.BasicLit) (*types.Object, error) {
	return types.NewBasic(lit.Value.Lit), nil
}

func (r *Interpreter) VisitBinaryExpr(expr *ast.BinaryExpr) (*types.Object, error) {
	left, err := expr.Left.Accept(r)
	if err != nil {
		return nil, err
	}

	if expr.Op.Kind == token.LOR && left.Bool() {
		return types.NewBasic(true), nil
	}
	if expr.Op.Kind == token.LAND && !left.Bool() {
		return types.NewBasic(false), nil
	}

	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	if expr.Op.Kind == token.LOR || expr.Op.Kind == token.LAND {
		return types.NewBasic(right.Bool()), nil
	}

	var result any

	if left.IsInteger() && right.IsInteger() {
		leftInt := left.Int()
		rightInt := right.Int()

		switch expr.Op.Kind {
		case token.ADD:
			result = leftInt + rightInt
		case token.SUB:
			result = leftInt - rightInt
		case token.MUL:
			result = leftInt * rightInt
		case token.QUO:
			result = leftInt / rightInt
		case token.REM:
			result = leftInt % rightInt
		case token.AND:
			result = leftInt & rightInt
		case token.OR:
			result = leftInt | rightInt
		case token.XOR:
			result = leftInt ^ rightInt
		case token.SHL:
			result = leftInt << rightInt
		case token.SHR:
			result = leftInt >> rightInt
		case token.AND_NOT:
			result = leftInt &^ rightInt
		case token.LSS:
			result = leftInt < rightInt
		case token.GTR:
			result = leftInt > rightInt
		case token.LEQ:
			result = leftInt <= rightInt
		case token.GEQ:
			result = leftInt >= rightInt
		default:
			goto equality
		}
		if result != nil {
			return types.NewBasic(result), nil
		}
	}

	if left.IsNumeric() && right.IsNumeric() {
		leftFloat := left.Float()
		rightFloat := right.Float()

		switch expr.Op.Kind {
		case token.ADD:
			result = leftFloat + rightFloat
		case token.SUB:
			result = leftFloat - rightFloat
		case token.MUL:
			result = leftFloat * rightFloat
		case token.QUO:
			result = leftFloat / rightFloat
		case token.REM:
			result = math.Mod(leftFloat, rightFloat)
		case token.LSS:
			result = leftFloat < rightFloat
		case token.GTR:
			result = leftFloat > rightFloat
		case token.LEQ:
			result = leftFloat <= rightFloat
		case token.GEQ:
			result = leftFloat >= rightFloat
		default:
			goto equality
		}
		if result != nil {
			return types.NewBasic(result), nil
		}
	}

	if left.IsString() && right.IsString() {
		leftStr := left.Str()
		rightStr := right.Str()

		switch expr.Op.Kind {
		case token.ADD:
			result = leftStr + rightStr
		case token.LSS:
			result = strings.Compare(leftStr, rightStr) < 0
		case token.GTR:
			result = strings.Compare(leftStr, rightStr) > 0
		case token.LEQ:
			result = leftStr == rightStr || strings.Compare(leftStr, rightStr) < 0
		case token.GEQ:
			result = leftStr == rightStr || strings.Compare(leftStr, rightStr) > 0
		}
		if result != nil {
			return types.NewBasic(result), nil
		}
	}

equality:
	if expr.Op.Kind == token.EQL {
		result = left.Value == right.Value
	} else {
		result = left.Value != right.Value
	}

	return types.NewBasic(result), nil
}

func (r *Interpreter) VisitUnaryExpr(expr *ast.UnaryExpr) (*types.Object, error) {
	if expr.Op.Kind == token.SUB {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.Value.(float64); ok {
			return types.NewBasic(-1 * rval), nil
		}
		return types.NewBasic(-1 * right.Value.(int)), nil
	}

	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}
	return types.NewBasic(!right.Value.(bool)), nil
}

func (r *Interpreter) VisitCallExpr(expr *ast.CallExpr) (*types.Object, error) {
	callee, err := r.Eval(expr.Callee)
	if err != nil {
		return nil, err
	}

	var args []*types.Object
	for _, arg := range expr.Args {
		val, err := r.Eval(arg)
		if err != nil {
			return nil, err
		}
		args = append(args, val)
	}

	callable, ok := callee.Value.(Callable)
	if !ok {
		return nil, fmt.Errorf("the callee is not callable")
	}

	return callable.Call(r, args)
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (*types.Object, error) {
	return expr.X.Accept(r)
}

func (r *Interpreter) VisitDeferStmt(stmt *ast.DeferStmt) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitSwitchStmt(stmt *ast.SwitchStmt) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitCaseClause(stmt *ast.CaseClause) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitForRangeStmt(stmt *ast.ForRangeStmt) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitConstDecl(stmt *ast.ConstDecl) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitTypeDecl(stmt *ast.TypeDecl) error {
	//TODO implement me
	panic("implement me")
}

func (r *Interpreter) VisitPrintStmt(stmt *ast.PrintStmt) error {
	result, err := r.Eval(stmt.Expr)
	if err != nil {
		return err
	}
	switch v := result.Value.(type) {
	case int:
		fmt.Printf("%d\n", v)
	case rune:
		fmt.Printf("%c\n", v)
	case bool:
		fmt.Printf("%v\n", v)
	case float64:
		fmt.Printf("%s\n", strconv.FormatFloat(v, 'f', -1, 64))
	case string:
		fmt.Printf("%s\n", v)
	default:
		if result == nil {
			fmt.Printf("%v\n", result)
		}
	}
	return nil
}

func (r *Interpreter) VisitExprStmt(stmt *ast.ExprStmt) error {
	_, err := r.Eval(stmt.Expr)
	return err
}

func (r *Interpreter) VisitBlockStmt(stmt *ast.BlockStmt) error {
	return r.RunBlock(stmt.List, newEnvironment(r.env))
}

func (r *Interpreter) VisitIfStmt(stmt *ast.IfStmt) error {
	condVal, err := r.Eval(stmt.Cond)
	if err != nil {
		return err
	}
	if condVal.Value.(bool) {
		return r.Run(stmt.Body)
	}
	if stmt.Else != nil {
		return r.Run(stmt.Else)
	}
	return nil
}

func (r *Interpreter) VisitForStmt(stmt *ast.ForStmt) error {
	for {
		condVal, err := r.Eval(stmt.Cond)
		if err != nil {
			return err
		}
		if !condVal.Value.(bool) {
			break
		}
		err = r.Run(stmt.Body)
		if errors.Is(err, errBreak) {
			break
		}
		if errors.Is(err, errContinue) {
			continue
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func (r *Interpreter) VisitReturnStmt(stmt *ast.ReturnStmt) error {
	var result *types.Object
	if stmt.Result != nil {
		val, err := r.Eval(stmt.Result)
		if err != nil {
			return err
		}
		result = val
	}
	return &Return{result: result}
}

func (r *Interpreter) VisitBranchStmt(stmt *ast.BranchStmt) error {
	if stmt.Tok.Kind == token.BREAK {
		return errBreak
	}
	return errContinue
}

func (r *Interpreter) VisitIncDecStmt(stmt *ast.IncDecStmt) error {
	id, ok := stmt.Expr.(*ast.Ident)
	if !ok {
		return fmt.Errorf("%q can only be used on identifiers", stmt.Tok.Lexeme)
	}
	val, err := r.Eval(id)
	if err != nil {
		return err
	}
	if stmt.Tok.Kind == token.INC {
		return val.Inc()
	}
	return val.Dec()
}

func (r *Interpreter) VisitAssignStmt(stmt *ast.AssignStmt) error {
	v, err := r.Eval(stmt.Value)
	if err != nil {
		return err
	}
	return r.env.Assign(stmt.Name.Lexeme, v)
}

func (r *Interpreter) VisitVarDecl(stmt *ast.VarDecl) error {
	v, err := r.Eval(stmt.Value)
	if err != nil {
		return err
	}
	return r.env.Define(stmt.Name.Lexeme, v)
}

func (r *Interpreter) VisitFuncDecl(stmt *ast.FuncDecl) error {
	fn := &Func{decl: stmt}
	r.env.Define(stmt.Name.Lexeme, &types.Object{Type: types.Func, Value: fn})
	return nil
}
