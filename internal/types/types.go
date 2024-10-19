package types

type Type int

const (
	Invalid Type = iota

	Bool
	Int
	Float
	String

	UntypedNil
)

type Value struct {
	Native any
	Type   Type
}

func NewValue(native any) *Value {
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

