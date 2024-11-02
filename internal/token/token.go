package token

import (
	"fmt"
	"strconv"
	"strings"
)

type Kind int

//go:generate stringer -type Kind

const (
	ILLEGAL Kind = iota
	EOF

	IDENT
	FLOAT
	INT
	STRING
	CHAR
	BOOL

	ADD
	SUB
	MUL
	QUO
	REM

	AND     // &
	OR      // |
	XOR     // ^
	SHL     // <<
	SHR     // >>
	AND_NOT // &^

	LAND  // &&
	LOR   // ||
	ARROW // <-
	INC   // ++
	DEC   // --

	EQL // ==
	NEQ // !=
	LSS // <
	GTR // >
	LEQ // <=
	GEQ // >=

	NOT // !

	ASSIGN // =
	DEFINE // :=

	LPAREN
	RPAREN
	LBRACK
	RBRACK
	LBRACE
	RBRACE

	SEMICOLON
	COLON
	COMMA
	PERIOD
	ELLIPSIS

	BREAK
	CASE
	CHAN
	CONST
	CONTINUE
	DEFAULT
	DEFER
	ELSE
	FALLTHROUGH
	FOR
	FUNC
	GO
	GOTO
	IF
	IMPORT
	INTERFACE
	MAP
	RANGE
	RETURN
	STRUCT
	SWITCH
	TYPE
	VAR
	PRINT

	NIL
)

func Keyword(s string) (Token, bool) {
	t, ok := keywords[s]
	return t, ok
}

var keywords = map[string]Token{
	"false":       {BOOL, false, "false", -1, -1},
	"true":        {BOOL, true, "true", -1, -1},
	"nil":         {NIL, nil, "nil", -1, -1},
	"break":       {BREAK, nil, "break", -1, -1},
	"case":        {CASE, nil, "case", -1, -1},
	"chan":        {CHAN, nil, "chan", -1, -1},
	"const":       {CONST, nil, "const", -1, -1},
	"continue":    {CONTINUE, nil, "continue", -1, -1},
	"default":     {DEFAULT, nil, "default", -1, -1},
	"defer":       {DEFER, nil, "defer", -1, -1},
	"else":        {ELSE, nil, "else", -1, -1},
	"fallthrough": {FALLTHROUGH, nil, "fallthrough", -1, -1},
	"for":         {FOR, nil, "for", -1, -1},
	"func":        {FUNC, nil, "func", -1, -1},
	"go":          {GO, nil, "go", -1, -1},
	"goto":        {GOTO, nil, "goto", -1, -1},
	"if":          {IF, nil, "if", -1, -1},
	"import":      {IMPORT, nil, "import", -1, -1},
	"interface":   {INTERFACE, nil, "interface", -1, -1},
	"map":         {MAP, nil, "map", -1, -1},
	"range":       {RANGE, nil, "range", -1, -1},
	"return":      {RETURN, nil, "return", -1, -1},
	"struct":      {STRUCT, nil, "struct", -1, -1},
	"switch":      {SWITCH, nil, "switch", -1, -1},
	"type":        {TYPE, nil, "type", -1, -1},
	"var":         {VAR, nil, "var", -1, -1},
	"print":       {PRINT, nil, "print", -1, -1},
}

type Token struct {
	Kind   Kind
	Lit    any
	Lexeme string
	Line   int
	Col    int
}

func (t *Token) String() string {
	switch lv := t.Lit.(type) {
	case string, rune:
		return fmt.Sprintf("%s(%q)", t.Kind.String(), lv)
	case float64:
		s := strconv.FormatFloat(lv, 'f', -1, 64)
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return fmt.Sprintf("%s(%s)", t.Kind.String(), s)
	default:
		v := lv
		if v == nil {
			v = t.Lexeme
		}
		return fmt.Sprintf("%s(%v)", t.Kind.String(), v)
	}
}
