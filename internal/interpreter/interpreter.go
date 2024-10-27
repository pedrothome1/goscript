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
	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	leftInt, isLeftInt := left.Value.(int)
	rightInt, isRightInt := right.Value.(int)

	if isLeftInt && isRightInt {
		switch expr.Op.Kind {
		case token.ADD:
			return types.NewBasic(leftInt + rightInt), nil
		case token.SUB:
			return types.NewBasic(leftInt - rightInt), nil
		case token.MUL:
			return types.NewBasic(leftInt * rightInt), nil
		case token.QUO:
			return types.NewBasic(leftInt / rightInt), nil
		case token.REM:
			return types.NewBasic(leftInt % rightInt), nil

		case token.AND:
			return types.NewBasic(leftInt & rightInt), nil
		case token.OR:
			return types.NewBasic(leftInt | rightInt), nil
		case token.XOR:
			return types.NewBasic(leftInt ^ rightInt), nil
		case token.SHL:
			return types.NewBasic(leftInt << rightInt), nil
		case token.SHR:
			return types.NewBasic(leftInt >> rightInt), nil
		case token.AND_NOT:
			return types.NewBasic(leftInt &^ rightInt), nil

		// comparison
		case token.EQL:
			return types.NewBasic(leftInt == rightInt), nil
		case token.NEQ:
			return types.NewBasic(leftInt != rightInt), nil
		case token.LSS:
			return types.NewBasic(leftInt < rightInt), nil
		case token.GTR:
			return types.NewBasic(leftInt > rightInt), nil
		case token.LEQ:
			return types.NewBasic(leftInt <= rightInt), nil
		case token.GEQ:
			return types.NewBasic(leftInt >= rightInt), nil

		default:
			return nil, fmt.Errorf("invalid integer operator")
		}
	}

	leftFloat, isLeftFloat := left.Value.(float64)
	rightFloat, isRightFloat := right.Value.(float64)

	if (isLeftInt || isLeftFloat) && (isRightInt || isRightFloat) {
		if isLeftInt {
			leftFloat = float64(leftInt)
		} else if isRightInt {
			rightFloat = float64(rightInt)
		}

		switch expr.Op.Kind {
		case token.ADD:
			return types.NewBasic(leftFloat + rightFloat), nil
		case token.SUB:
			return types.NewBasic(leftFloat - rightFloat), nil
		case token.MUL:
			return types.NewBasic(leftFloat * rightFloat), nil
		case token.QUO:
			return types.NewBasic(leftFloat / rightFloat), nil
		case token.REM:
			return types.NewBasic(math.Mod(leftFloat, rightFloat)), nil

		// comparison
		case token.EQL:
			return types.NewBasic(leftFloat == rightFloat), nil
		case token.NEQ:
			return types.NewBasic(leftFloat != rightFloat), nil
		case token.LSS:
			return types.NewBasic(leftFloat < rightFloat), nil
		case token.GTR:
			return types.NewBasic(leftFloat > rightFloat), nil
		case token.LEQ:
			return types.NewBasic(leftFloat <= rightFloat), nil
		case token.GEQ:
			return types.NewBasic(leftFloat >= rightFloat), nil

		default:
			return nil, fmt.Errorf("invalid float operator")
		}
	}

	leftStr, isLeftStr := left.Value.(string)
	rightStr, isRightStr := right.Value.(string)

	if isLeftStr && isRightStr {
		switch expr.Op.Kind {
		case token.ADD:
			return types.NewBasic(leftStr + rightStr), nil

		// comparison
		case token.EQL:
			return types.NewBasic(leftStr == rightStr), nil
		case token.NEQ:
			return types.NewBasic(leftStr != rightStr), nil
		case token.LSS:
			return types.NewBasic(strings.Compare(leftStr, rightStr) < 0), nil
		case token.GTR:
			return types.NewBasic(strings.Compare(leftStr, rightStr) > 0), nil
		case token.LEQ:
			return types.NewBasic(leftStr == rightStr || strings.Compare(leftStr, rightStr) < 0), nil
		case token.GEQ:
			return types.NewBasic(leftStr == rightStr || strings.Compare(leftStr, rightStr) > 0), nil
		default:
			return nil, fmt.Errorf("invalid string operator")
		}
	}

	// it's not short-circuiting.
	// TODO: review
	leftBool, isLeftBool := left.Value.(bool)
	rightBool, isRightBool := right.Value.(bool)

	if isLeftBool && isRightBool {
		switch expr.Op.Kind {
		case token.LAND:
			return types.NewBasic(leftBool && rightBool), nil
		case token.LOR:
			return types.NewBasic(leftBool || rightBool), nil
		case token.EQL:
			return types.NewBasic(leftBool == rightBool), nil
		case token.NEQ:
			return types.NewBasic(leftBool != rightBool), nil
		default:
			return nil, fmt.Errorf("invalid bool operator")
		}
	}

	return nil, fmt.Errorf("mismatched operand types")
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
		if rval, ok := right.Value.(int); ok {
			return types.NewBasic(-1 * rval), nil
		}
		return nil, fmt.Errorf("the operand must be a number")
	}

	if expr.Op.Kind == token.NOT {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.Value.(bool); ok {
			return types.NewBasic(!rval), nil
		}
		return nil, fmt.Errorf("the operand must be a bool")
	}

	return nil, fmt.Errorf("invalid unary operator")
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

	if len(args) != callable.Arity() {
		return nil, fmt.Errorf("expected %d arguments, but got %d", callable.Arity(), len(args))
	}

	return callable.Call(r, args)
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (*types.Object, error) {
	return expr.X.Accept(r)
}

func (r *Interpreter) VisitPrintStmt(stmt *ast.PrintStmt) error {
	result, err := r.Eval(stmt.Expr)
	if err != nil {
		return err
	}
	switch v := result.Value.(type) {
	case int:
		fmt.Printf("%d\n", v)
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
	if condVal.Type != types.Bool {
		return fmt.Errorf("the 'if' condition must be a boolean expression")
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
		if condVal.Type != types.Bool {
			return fmt.Errorf("the 'for' condition must be a boolean expression")
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
	switch stmt.Tok.Kind {
	case token.BREAK:
		return errBreak
	case token.CONTINUE:
		return errContinue
	default:
		return fmt.Errorf("%q branch statement not implemented", stmt.Tok.Lexeme)
	}
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
