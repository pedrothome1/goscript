package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/token"
	"math"
	"strings"
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
		case token.AND:
			return leftInt & rightInt, nil
		case token.OR:
			return leftInt | rightInt, nil
		case token.XOR:
			return leftInt ^ rightInt, nil
		case token.SHL:
			return leftInt << rightInt, nil
		case token.SHR:
			return leftInt >> rightInt, nil
		case token.AND_NOT:
			return leftInt &^ rightInt, nil

		// comparison
		case token.EQL:
			return leftInt == rightInt, nil
		case token.NEQ:
			return leftInt != rightInt, nil
		case token.LSS:
			return leftInt < rightInt, nil
		case token.GTR:
			return leftInt > rightInt, nil
		case token.LEQ:
			return leftInt <= rightInt, nil
		case token.GEQ:
			return leftInt >= rightInt, nil

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

		// comparison
		case token.EQL:
			return leftFloat == rightFloat, nil
		case token.NEQ:
			return leftFloat != rightFloat, nil
		case token.LSS:
			return leftFloat < rightFloat, nil
		case token.GTR:
			return leftFloat > rightFloat, nil
		case token.LEQ:
			return leftFloat <= rightFloat, nil
		case token.GEQ:
			return leftFloat >= rightFloat, nil

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

		// comparison
		case token.EQL:
			return leftStr == rightStr, nil
		case token.NEQ:
			return leftStr != rightStr, nil
		case token.LSS:
			return strings.Compare(leftStr, rightStr) < 0, nil
		case token.GTR:
			return strings.Compare(leftStr, rightStr) > 0, nil
		case token.LEQ:
			return leftStr == rightStr || strings.Compare(leftStr, rightStr) < 0, nil
		case token.GEQ:
			return leftStr == rightStr || strings.Compare(leftStr, rightStr) > 0, nil
		default:
			return nil, fmt.Errorf("invalid string operator")
		}
	}

	// it's not short-circuiting.
	// TODO: review
	leftBool, isLeftBool := left.(bool)
	rightBool, isRightBool := right.(bool)

	if isLeftBool && isRightBool {
		switch expr.Op.Kind {
		case token.LAND:
			return leftBool && rightBool, nil
		case token.LOR:
			return leftBool || rightBool, nil
		case token.EQL:
			return leftBool == rightBool, nil
		case token.NEQ:
			return leftBool != rightBool, nil
		default:
			return nil, fmt.Errorf("invalid bool operator")
		}
	}

	return nil, fmt.Errorf("mismatched operand types")
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

	if expr.Op.Kind == token.NOT {
		right, err := expr.Right.Accept(r)
		if err != nil {
			return nil, err
		}
		if rval, ok := right.(bool); ok {
			return !rval, nil
		}
		return nil, fmt.Errorf("the operand must be a bool")
	}

	return nil, fmt.Errorf("invalid unary operator")
}

func (r *Interpreter) VisitParenExpr(expr *ast.ParenExpr) (any, error) {
	return expr.X.Accept(r)
}
