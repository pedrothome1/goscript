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

func (o *Object) IsNumeric() bool {
	return o.Type.IsNumeric()
}

func (o *Object) IsInteger() bool {
	return o.Type == Int
}

func (o *Object) IsString() bool {
	return o.Type == String
}

func (o *Object) IsBool() bool {
	return o.Type == Bool
}

func (o *Object) Inc() error {
	switch val := o.Value.(type) {
	case int:
		val++
		o.Value = val
	case float64:
		val++
		o.Value = val
	default:
		return fmt.Errorf("incrementing non-numeric type")
	}
	return nil
}

func (o *Object) Dec() error {
	switch val := o.Value.(type) {
	case int:
		val--
		o.Value = val
	case float64:
		val--
		o.Value = val
	default:
		return fmt.Errorf("decrementing non-numeric type")
	}
	return nil
}

func (o *Object) Int() int {
	if f, ok := o.Value.(float64); ok {
		return int(f)
	}

	return o.Value.(int)
}

func (o *Object) Float() float64 {
	if i, ok := o.Value.(int); ok {
		return float64(i)
	}

	return o.Value.(float64)
}

func (o *Object) Char() rune {
	return o.Value.(rune)
}

func (o *Object) Str() string {
	return o.Value.(string)
}

func (o *Object) Bool() bool {
	return o.Value.(bool)
}
