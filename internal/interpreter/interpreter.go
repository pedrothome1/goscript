package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"math"
)

type Interpreter struct{}

func (r *Interpreter) Run(expr ast.Expr) (any, error) {
	result, err := expr.Accept(r)
	if err != nil {
		return 0, err
	}
	return result, nil
}

func (r *Interpreter) VisitBasicLit(lit *ast.BasicLit) (any, error) {
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

	leftInt, isLeftInt := left.(int)
	rightInt, isRightInt := right.(int)

	if isLeftInt && isRightInt {
		switch expr.Op.Kind {
		case token.ADD:
			return leftInt + rightInt, nil
		case token.SUB:
			return leftInt - rightInt, nil
		case token.MUL:
			return leftInt * rightInt, nil
		case token.QUO:
			return leftInt / rightInt, nil
		case token.REM:
			return leftInt % rightInt, nil
		default:
			return nil, fmt.Errorf("invalid integer operator")
		}
	}

	leftFloat, isLeftFloat := left.(float64)
	rightFloat, isRightFloat := right.(float64)

	if (isLeftInt || isLeftFloat) && (isRightInt || isRightFloat) {
		if isLeftInt {
			leftFloat = float64(leftInt)
		} else if isRightInt {
			rightFloat = float64(rightInt)
		}

		switch expr.Op.Kind {
		case token.ADD:
			return leftFloat + rightFloat, nil
		case token.SUB:
			return leftFloat - rightFloat, nil
		case token.MUL:
			return leftFloat * rightFloat, nil
		case token.QUO:
			return leftFloat / rightFloat, nil
		case token.REM:
			return math.Mod(leftFloat, rightFloat), nil
		default:
			return nil, fmt.Errorf("invalid float operator")
		}
	}

	leftStr, isLeftStr := left.(string)
	rightStr, isRightStr := right.(string)

	if isLeftStr && isRightStr {
		switch expr.Op.Kind {
		case token.ADD:
			return leftStr + rightStr, nil
		default:
			return nil, fmt.Errorf("invalid string operator")
		}
	}

	return nil, fmt.Errorf("the operands must be both either numbers or strings")
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
		if rval, ok := right.(int); ok {
			return -1 * rval, nil
		}
		return nil, fmt.Errorf("the operand must be a number")
	}

	return nil, fmt.Errorf("invalid unary operator")
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (any, error) {
	return expr.X.Accept(r)
}
