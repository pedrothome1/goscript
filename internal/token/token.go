package token

import "fmt"

type Kind int

const (
	ILLEGAL Kind = iota
	EOF

	FLOAT
	INT

	ADD
	SUB
	MUL
	QUO
	REM

	LPAREN
	RPAREN
)

func (k Kind) String() string {
	return kinds[k]
}

var kinds = map[Kind]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	FLOAT:   "FLOAT",
	INT:     "INT",
	ADD:     "ADD",
	SUB:     "SUB",
	MUL:     "MUL",
	QUO:     "QUO",
	REM:     "REM",
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
