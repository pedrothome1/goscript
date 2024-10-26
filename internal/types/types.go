package types

import "fmt"

type Type int

const (
	Invalid Type = iota

	Bool
	Int
	Float
	String

	UntypedNil

	Func
)

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
