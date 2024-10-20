package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"github.com/pedrothome1/goscript/internal/types"
	"math"
	"strconv"
	"strings"
)

type Interpreter struct {
	env *Environment
}

func New() *Interpreter {
	return &Interpreter{env: newEnvironment(nil)}
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

func (r *Interpreter) Eval(expr ast.Expr) (*types.Value, error) {
	result, err := expr.Accept(r)
	if err != nil {
		return types.NewValue(nil), err
	}
	return result, nil
}

func (r *Interpreter) VisitIdent(expr *ast.Ident) (*types.Value, error) {
	return r.env.Get(expr.Name.Lexeme)
}

func (r *Interpreter) VisitBasicLit(lit *ast.BasicLit) (*types.Value, error) {
	return types.NewValue(lit.Value.Lit), nil
}

func (r *Interpreter) VisitBinaryExpr(expr *ast.BinaryExpr) (*types.Value, error) {
	left, err := expr.Left.Accept(r)
	if err != nil {
		return nil, err
	}
	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	leftInt, isLeftInt := left.Native.(int)
	rightInt, isRightInt := right.Native.(int)

	if isLeftInt && isRightInt {
		switch expr.Op.Kind {
		case token.ADD:
			return types.NewValue(leftInt + rightInt), nil
		case token.SUB:
			return types.NewValue(leftInt - rightInt), nil
		case token.MUL:
			return types.NewValue(leftInt * rightInt), nil
		case token.QUO:
			return types.NewValue(leftInt / rightInt), nil
		case token.REM:
			return types.NewValue(leftInt % rightInt), nil
		case token.AND:
			return types.NewValue(leftInt & rightInt), nil
		case token.OR:
			return types.NewValue(leftInt | rightInt), nil
		case token.XOR:
			return types.NewValue(leftInt ^ rightInt), nil
		case token.SHL:
			return types.NewValue(leftInt << rightInt), nil
		case token.SHR:
			return types.NewValue(leftInt >> rightInt), nil
		case token.AND_NOT:
			return types.NewValue(leftInt &^ rightInt), nil

		// comparison
		case token.EQL:
			return types.NewValue(leftInt == rightInt), nil
		case token.NEQ:
			return types.NewValue(leftInt != rightInt), nil
		case token.LSS:
			return types.NewValue(leftInt < rightInt), nil
		case token.GTR:
			return types.NewValue(leftInt > rightInt), nil
		case token.LEQ:
			return types.NewValue(leftInt <= rightInt), nil
		case token.GEQ:
			return types.NewValue(leftInt >= rightInt), nil

		default:
			return nil, fmt.Errorf("invalid integer operator")
		}
	}

	leftFloat, isLeftFloat := left.Native.(float64)
	rightFloat, isRightFloat := right.Native.(float64)

	if (isLeftInt || isLeftFloat) && (isRightInt || isRightFloat) {
		if isLeftInt {
			leftFloat = float64(leftInt)
		} else if isRightInt {
			rightFloat = float64(rightInt)
		}

		switch expr.Op.Kind {
		case token.ADD:
			return types.NewValue(leftFloat + rightFloat), nil
		case token.SUB:
			return types.NewValue(leftFloat - rightFloat), nil
		case token.MUL:
			return types.NewValue(leftFloat * rightFloat), nil
		case token.QUO:
			return types.NewValue(leftFloat / rightFloat), nil
		case token.REM:
			return types.NewValue(math.Mod(leftFloat, rightFloat)), nil

		// comparison
		case token.EQL:
			return types.NewValue(leftFloat == rightFloat), nil
		case token.NEQ:
			return types.NewValue(leftFloat != rightFloat), nil
		case token.LSS:
			return types.NewValue(leftFloat < rightFloat), nil
		case token.GTR:
			return types.NewValue(leftFloat > rightFloat), nil
		case token.LEQ:
			return types.NewValue(leftFloat <= rightFloat), nil
		case token.GEQ:
			return types.NewValue(leftFloat >= rightFloat), nil

		default:
			return nil, fmt.Errorf("invalid float operator")
		}
	}

	leftStr, isLeftStr := left.Native.(string)
	rightStr, isRightStr := right.Native.(string)

	if isLeftStr && isRightStr {
		switch expr.Op.Kind {
		case token.ADD:
			return types.NewValue(leftStr + rightStr), nil

		// comparison
		case token.EQL:
			return types.NewValue(leftStr == rightStr), nil
		case token.NEQ:
			return types.NewValue(leftStr != rightStr), nil
		case token.LSS:
			return types.NewValue(strings.Compare(leftStr, rightStr) < 0), nil
		case token.GTR:
			return types.NewValue(strings.Compare(leftStr, rightStr) > 0), nil
		case token.LEQ:
			return types.NewValue(leftStr == rightStr || strings.Compare(leftStr, rightStr) < 0), nil
		case token.GEQ:
			return types.NewValue(leftStr == rightStr || strings.Compare(leftStr, rightStr) > 0), nil
		default:
			return nil, fmt.Errorf("invalid string operator")
		}
	}

	// it's not short-circuiting.
	// TODO: review
	leftBool, isLeftBool := left.Native.(bool)
	rightBool, isRightBool := right.Native.(bool)

	if isLeftBool && isRightBool {
		switch expr.Op.Kind {
		case token.LAND:
			return types.NewValue(leftBool && rightBool), nil
		case token.LOR:
			return types.NewValue(leftBool || rightBool), nil
		case token.EQL:
			return types.NewValue(leftBool == rightBool), nil
		case token.NEQ:
			return types.NewValue(leftBool != rightBool), nil
		default:
			return nil, fmt.Errorf("invalid bool operator")
		}
	}

	return nil, fmt.Errorf("mismatched operand types")
}

func (r *Interpreter) VisitUnaryExpr(expr *ast.UnaryExpr) (*types.Value, error) {
	if expr.Op.Kind == token.SUB {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.Native.(float64); ok {
			return types.NewValue(-1 * rval), nil
		}
		if rval, ok := right.Native.(int); ok {
			return types.NewValue(-1 * rval), nil
		}
		return nil, fmt.Errorf("the operand must be a number")
	}

	if expr.Op.Kind == token.NOT {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.Native.(bool); ok {
			return types.NewValue(!rval), nil
		}
		return nil, fmt.Errorf("the operand must be a bool")
	}

	return nil, fmt.Errorf("invalid unary operator")
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (*types.Value, error) {
	return expr.X.Accept(r)
}

func (r *Interpreter) VisitPrintStmt(stmt *ast.PrintStmt) error {
	result, err := r.Eval(stmt.Expr)
	if err != nil {
		return err
	}
	switch v := result.Native.(type) {
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
	if condVal.Native.(bool) {
		return r.Run(stmt.Body)
	}
	if stmt.Else != nil {
		return r.Run(stmt.Else)
	}
	return nil
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
