package types

import (
	"fmt"
)

type Kind int

const (
	Invalid Kind = iota

	Type
	Func

	Bool
	Int
	Char
	Float
	String

	UntypedNil
)

func (t Kind) IsBasic() bool {
	switch t {
	case Bool, Int, Char, Float, String:
		return true
	}
	return false
}

func (t Kind) IsNumeric() bool {
	return t == Int || t == Float
}

func FromLexeme(lexeme string) Kind {
	tbl := map[string]Kind{
		"int":    Int,
		"char":   Char,
		"float":  Float,
		"bool":   Bool,
		"string": String,
	}

	if t, ok := tbl[lexeme]; ok {
		return t
	}

	return Invalid
}

type Object struct {
	Value any
	Type  Kind
}

func NewBasic(value any) *Object {
	var t Kind
	switch value.(type) {
	case bool:
		t = Bool
	case int:
		t = Int
	case float64:
		t = Float
	case string:
		t = String
	default:
		if value == nil {
			t = UntypedNil
		} else {
			t = Invalid
		}
	}

	return &Object{
		Value: value,
		Type:  t,
	}
}

func (v *Object) Inc() error {
	switch val := v.Value.(type) {
	case int:
		val++
		v.Value = val
	case float64:
		val++
		v.Value = val
	default:
		return fmt.Errorf("incrementing non-numeric type")
	}
	return nil
}

func (v *Object) Dec() error {
	switch val := v.Value.(type) {
	case int:
		val--
		v.Value = val
	case float64:
		val--
		v.Value = val
	default:
		return fmt.Errorf("decrementing non-numeric type")
	}
	return nil
}
