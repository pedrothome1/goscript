package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
)

type Interpreter struct{}

func (r *Interpreter) Run(expr ast.Expr) (float64, error) {
	result, err := expr.Accept(r)
	if err != nil {
		return 0, err
	}
	return result.(float64), nil
}

func (r *Interpreter) VisitFloatLit(lit *ast.FloatLit) (any, error) {
	return lit.Value.Lit, nil
}

func (r *Interpreter) VisitBinaryExpr(expr *ast.BinaryExpr) (any, error) {
	left, err := expr.Left.Accept(r)
	if err != nil {
		return nil, err
	}
	right, err := expr.Right.Accept(r)
	if err != nil {
		return nil, err
	}

	if lval, ok1 := left.(float64); ok1 {
		if rval, ok2 := right.(float64); ok2 {
			switch expr.Op.Kind {
			case token.ADD:
				return lval + rval, nil
			case token.SUB:
				return lval - rval, nil
			case token.MUL:
				return lval * rval, nil
			case token.QUO:
				return lval / rval, nil
			// Does not work on floats
			//case token.REM:
			//	return lval % rval, nil
			default:
				return nil, fmt.Errorf("invalid binary operator")
			}
		}
	}

	return nil, fmt.Errorf("the operands must be numbers")
}

func (r *Interpreter) VisitUnaryExpr(expr *ast.UnaryExpr) (any, error) {
	if expr.Op.Kind == token.SUB {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.(float64); ok {
			return -1 * rval, nil
		}
		return nil, fmt.Errorf("the operand must be a number")
	}

	return nil, fmt.Errorf("invalid unary operator")
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (any, error) {
	return expr.X.Accept(r)
}
