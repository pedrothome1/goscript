package token

import "fmt"

type Kind int

const (
	ILLEGAL Kind = iota
	EOF

	FLOAT

	ADD
	SUB
	MUL
	QUO
	REM

	LPAREN
	RPAREN
)

var kinds = map[Kind]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	FLOAT:   "FLOAT",
	ADD:     "ADD",
	SUB:     "SUB",
	MUL:     "MUL",
	QUO:     "QUO",
	REM:     "REM",
	LPAREN:  "LPAREN",
	RPAREN:  "RPAREN",
}

type Token struct {
	Kind Kind
	Lit  any
}

func (t *Token) String() string {
	return fmt.Sprintf("%s(%v)", kinds[t.Kind], t.Lit)
}
