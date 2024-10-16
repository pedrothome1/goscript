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
)

func (k Kind) String() string {
	return kinds[k]
}

var kinds = map[Kind]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	IDENT:   "IDENT",
	FLOAT:   "FLOAT",
	INT:     "INT",
	STRING:  "STRING",
	CHAR:    "CHAR",
	BOOL:    "BOOL",
	ADD:     "ADD",
	SUB:     "SUB",
	MUL:     "MUL",
	QUO:     "QUO",
	REM:     "REM",
	AND:     "AND",
	OR:      "OR",
	XOR:     "XOR",
	SHL:     "SHL",
	SHR:     "SHR",
	AND_NOT: "AND_NOT",
	LAND:    "LAND",
	LOR:     "LOR",
	EQL:     "EQL",
	NEQ:     "NEQ",
	LSS:     "LSS",
	GTR:     "GTR",
	LEQ:     "LEQ",
	GEQ:     "GEQ",
	NOT:     "NOT",
	ASSIGN:  "ASSIGN",
	LPAREN:  "LPAREN",
	RPAREN:  "RPAREN",
}

func Keyword(s string) (Token, bool) {
	t, ok := keywords[s]
	return t, ok
}

var keywords = map[string]Token{
	"false": {BOOL, false, "false"},
	"true":  {BOOL, true, "true"},
}

type Token struct {
	Kind   Kind
	Lit    any
	Lexeme string
}

func (t *Token) String() string {
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
