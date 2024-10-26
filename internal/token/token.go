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
	DEFINE:      "DEFINE",
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
	PRINT:       "PRINT",
	NIL:         "NIL",
}

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
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
