package types

import (
	"fmt"
)

type Type int

const (
	Invalid Type = iota

	TypeT

	Bool
	Int
	Float
	String
	UntypedNil

	Func
)

func (t Type) IsBasic() bool {
	switch t {
	case Bool, Int, Float, String:
		return true
	}
	return false
}

func (t Type) IsNumeric() bool {
	return t == Int || t == Float
}

func FromLexeme(lexeme string) Type {
	tbl := map[string]Type{
		"int":    Int,
		"char":   Int,
		"float":  Float,
		"bool":   Bool,
		"string": String,
	}

	if t, ok := tbl[lexeme]; ok {
		return t
	}

	return Invalid
}

type Value struct {
	Native any
	Type   Type
}

func NewBasicValue(native any) *Value {
	var t Type
	switch native.(type) {
	case bool:
		t = Bool
	case int:
		t = Int
	case float64:
		t = Float
	case string:
		t = String
	default:
		if native == nil {
			t = UntypedNil
		} else {
			t = Invalid
		}
	}

	return &Value{
		Native: native,
		Type:   t,
	}
}

func (v *Value) Inc() error {
	switch val := v.Native.(type) {
	case int:
		val++
		v.Native = val
	case float64:
		val++
		v.Native = val
	default:
		return fmt.Errorf("incrementing non-numeric type")
	}
	return nil
}

func (v *Value) Dec() error {
	switch val := v.Native.(type) {
	case int:
		val--
		v.Native = val
	case float64:
		val--
		v.Native = val
	default:
		return fmt.Errorf("decrementing non-numeric type")
	}
	return nil
}
