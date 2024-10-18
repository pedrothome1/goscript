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
	ARROW:       "ARROW",
	INC:         "INC",
	DEC:         "DEC",
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
	"false":       {BOOL, false, "false", 0},
	"true":        {BOOL, true, "true", 0},
	"nil":         {NIL, nil, "nil", 0},
	"break":       {BREAK, nil, "break", 0},
	"case":        {CASE, nil, "case", 0},
	"chan":        {CHAN, nil, "chan", 0},
	"const":       {CONST, nil, "const", 0},
	"continue":    {CONTINUE, nil, "continue", 0},
	"default":     {DEFAULT, nil, "default", 0},
	"defer":       {DEFER, nil, "defer", 0},
	"else":        {ELSE, nil, "else", 0},
	"fallthrough": {FALLTHROUGH, nil, "fallthrough", 0},
	"for":         {FOR, nil, "for", 0},
	"func":        {FUNC, nil, "func", 0},
	"go":          {GO, nil, "go", 0},
	"goto":        {GOTO, nil, "goto", 0},
	"if":          {IF, nil, "if", 0},
	"import":      {IMPORT, nil, "import", 0},
	"interface":   {INTERFACE, nil, "interface", 0},
	"map":         {MAP, nil, "map", 0},
	"range":       {RANGE, nil, "range", 0},
	"return":      {RETURN, nil, "return", 0},
	"struct":      {STRUCT, nil, "struct", 0},
	"switch":      {SWITCH, nil, "switch", 0},
	"type":        {TYPE, nil, "type", 0},
	"var":         {VAR, nil, "var", 0},
}

type Token struct {
	Kind   Kind
	Lit    any
	Lexeme string
	Pos    int
}

func (t *Token) String() string {
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
