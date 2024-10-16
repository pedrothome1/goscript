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

	LAND
	LOR

	EQL
	NEQ
	LSS
	GTR
	LEQ
	GEQ
	NOT

	ASSIGN

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
	LAND:    "LAND",
	LOR:     "LOR",
	EQL:     "EQL",
	LSS:     "LSS",
	GTR:     "GTR",
	NOT:     "NOT",
	NEQ:     "NEQ",
	LEQ:     "LEQ",
	GEQ:     "GEQ",
	ASSIGN:  "ASSIGN",
	LPAREN:  "LPAREN",
	RPAREN:  "RPAREN",
}

type Token struct {
	Kind   Kind
	Lit    any
	Lexeme string
}

func (t *Token) String() string {
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
