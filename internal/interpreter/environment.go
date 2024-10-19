package interpreter

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/types"
)

type Environment struct {
	up *Environment
	m  map[string]*types.Value
}

func newEnvironment(upper *Environment) *Environment {
	return &Environment{up: upper, m: make(map[string]*types.Value)}
}

func (e *Environment) Define(name string, value *types.Value) error {
	if _, ok := e.m[name]; ok {
		return fmt.Errorf("variable already defined")
	}
	e.m[name] = value
	return nil
}

func (e *Environment) Assign(name string, value *types.Value) error {
	v, ok := e.m[name]
	if !ok {
		if e.up != nil {
			return e.up.Assign(name, value)
		}
		return fmt.Errorf("variable not defined")
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
	if e.up != nil {
		return e.up.Get(name)
	}
	return nil, fmt.Errorf("variable not defined")
}
