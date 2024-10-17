package token

import "fmt"

type Kind int

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

	LAND // &&
	LOR  // ||

	EQL // ==
	NEQ // !=
	LSS // <
	GTR // >
	LEQ // <=
	GEQ // >=

	NOT // !

	ASSIGN // =

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

	NIL
)

func (k Kind) String() string {
	return kinds[k]
}

var kinds = map[Kind]string{
	ILLEGAL:     "ILLEGAL",
	EOF:         "EOF",
	IDENT:       "IDENT",
	FLOAT:       "FLOAT",
	INT:         "INT",
	STRING:      "STRING",
	CHAR:        "CHAR",
	BOOL:        "BOOL",
	ADD:         "ADD",
	SUB:         "SUB",
	MUL:         "MUL",
	QUO:         "QUO",
	REM:         "REM",
	AND:         "AND",
	OR:          "OR",
	XOR:         "XOR",
	SHL:         "SHL",
	SHR:         "SHR",
	AND_NOT:     "AND_NOT",
	LAND:        "LAND",
	LOR:         "LOR",
	EQL:         "EQL",
	NEQ:         "NEQ",
	LSS:         "LSS",
	GTR:         "GTR",
	LEQ:         "LEQ",
	GEQ:         "GEQ",
	NOT:         "NOT",
	ASSIGN:      "ASSIGN",
	LPAREN:      "LPAREN",
	RPAREN:      "RPAREN",
	LBRACK:      "LBRACK",
	RBRACK:      "RBRACK",
	LBRACE:      "LBRACE",
	RBRACE:      "RBRACE",
	SEMICOLON:   "SEMICOLON",
	COLON:       "COLON",
	COMMA:       "COMMA",
	PERIOD:      "PERIOD",
	ELLIPSIS:    "ELLIPSIS",
	BREAK:       "BREAK",
	CASE:        "CASE",
	CHAN:        "CHAN",
	CONST:       "CONST",
	CONTINUE:    "CONTINUE",
	DEFAULT:     "DEFAULT",
	DEFER:       "DEFER",
	ELSE:        "ELSE",
	FALLTHROUGH: "FALLTHROUGH",
	FOR:         "FOR",
	FUNC:        "FUNC",
	GO:          "GO",
	GOTO:        "GOTO",
	IF:          "IF",
	IMPORT:      "IMPORT",
	INTERFACE:   "INTERFACE",
	MAP:         "MAP",
	RANGE:       "RANGE",
	RETURN:      "RETURN",
	STRUCT:      "STRUCT",
	SWITCH:      "SWITCH",
	TYPE:        "TYPE",
	VAR:         "VAR",
	NIL:         "NIL",
}

func Keyword(s string) (Token, bool) {
	t, ok := keywords[s]
	return t, ok
}

var keywords = map[string]Token{
	"false": {BOOL, false, "false"},
	"true":  {BOOL, true, "true"},
	"nil":   {NIL, nil, "nil"},

	"break":       {BREAK, nil, "break"},
	"case":        {CASE, nil, "case"},
	"chan":        {CHAN, nil, "chan"},
	"const":       {CONST, nil, "const"},
	"continue":    {CONTINUE, nil, "continue"},
	"default":     {DEFAULT, nil, "default"},
	"defer":       {DEFER, nil, "defer"},
	"else":        {ELSE, nil, "else"},
	"fallthrough": {FALLTHROUGH, nil, "fallthrough"},
	"for":         {FOR, nil, "for"},
	"func":        {FUNC, nil, "func"},
	"go":          {GO, nil, "go"},
	"goto":        {GOTO, nil, "goto"},
	"if":          {IF, nil, "if"},
	"import":      {IMPORT, nil, "import"},
	"interface":   {INTERFACE, nil, "interface"},
	"map":         {MAP, nil, "map"},
	"range":       {RANGE, nil, "range"},
	"return":      {RETURN, nil, "return"},
	"struct":      {STRUCT, nil, "struct"},
	"switch":      {SWITCH, nil, "switch"},
	"type":        {TYPE, nil, "type"},
	"var":         {VAR, nil, "var"},
}

type Token struct {
	Kind   Kind
	Lit    any
	Lexeme string
}

func (t *Token) String() string {
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
