package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/types"
)

type Environment struct {
	m map[string]*types.Value
}

func newEnvironment() *Environment {
	return &Environment{m: make(map[string]*types.Value)}
}

func (e *Environment) Define(name string, value *types.Value) error {
	if _, ok := e.m[name]; ok {
		return fmt.Errorf("variable already defined")
	}
	e.m[name] = value
	return nil
}

func (e *Environment) Assign(name string, value *types.Value) error {
	v, err := e.Get(name)
	if err != nil {
		return err
	}
	if v.Type != value.Type {
		return fmt.Errorf("incompatible type")
	}
	e.m[name] = value
	return nil
}

func (e *Environment) Get(name string) (*types.Value, error) {
	if v, ok := e.m[name]; ok {
		return v, nil
	}
	return nil, fmt.Errorf("variable not defined")
}
