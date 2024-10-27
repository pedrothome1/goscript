package interpreter

import (
	"errors"
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/types"
)

type Callable interface {
	Arity() int
	Call(r *Interpreter, args []*types.Object) (*types.Object, error)
}

type Return struct {
	result *types.Object
}

func (r *Return) Error() string {
	return "return"
}

type Func struct {
	decl *ast.FuncDecl
}

func (f *Func) Arity() int {
	return len(f.decl.Params)
}

// We assume the arity is already validated.
func (f *Func) Call(r *Interpreter, args []*types.Object) (*types.Object, error) {
	env := newEnvironment(r.globals)
	for i, p := range f.decl.Params {
		env.Define(p.Name.Lexeme, args[i])
	}
	err := r.RunBlock(f.decl.Body, env)
	var ret *Return
	if errors.As(err, &ret) {
		return ret.result, nil
	}
	if err != nil {
		return nil, fmt.Errorf("from function call: %w", err)
	}
	return nil, nil
}
