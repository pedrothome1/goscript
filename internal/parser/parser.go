package parser

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/ast"
	"github.com/pedrothome1/goscript/internal/scanner"
	"github.com/pedrothome1/goscript/internal/token"
)

type Parser struct {
	toks []token.Token
	pos  int
}

func (p *Parser) Init(src []byte) error {
	s := &scanner.Scanner{}
	s.Init(src)
	toks, err := s.Scan()
	if err != nil {
		return err
	}
	p.toks = toks
	p.pos = 0
	return nil
}

/*
expression   -> logical_or
logical_or   -> logical_and ( ( '||' ) logical_and )*
logical_and  -> comparison ( ( '&&' ) comparison )*
comparison   -> term ( ( '==' | '!=' | '<' | '>' | '<=' | '>=' ) term )?
term         -> factor ( ( '+' | '-' | '|' | '^' ) factor )*
factor       -> unary ( ( '*' | '/' | '&' | '&^' | '<<' | '>>' ) unary )*
unary        -> ( '-' | '!' )? primary
primary      -> FLOAT | INT | CHAR | STRING | 'true' | 'false' | 'nil' | '(' expression ')'
*/

func (p *Parser) Parse() (ast.Expr, error) {
	return p.expression()
}

func (p *Parser) expression() (ast.Expr, error) {
	return p.logicalOr()
}

func (p *Parser) logicalOr() (ast.Expr, error) {
	expr, err := p.logicalAnd()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.LOR {
		op := p.advance()
		right, err := p.logicalAnd()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{
			Left:  expr,
			Op:    op,
			Right: right,
		}
	}

	return expr, nil
}

func (p *Parser) logicalAnd() (ast.Expr, error) {
	expr, err := p.comparison()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.LAND {
		op := p.advance()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{
			Left:  expr,
			Op:    op,
			Right: right,
		}
	}

	return expr, nil
}

func (p *Parser) comparison() (ast.Expr, error) {
	left, err := p.term()
	if err != nil {
		return nil, err
	}

	switch p.peek().Kind {
	case token.EQL, token.NEQ, token.LSS, token.GTR, token.LEQ, token.GEQ:
		op := p.advance()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		return &ast.BinaryExpr{
			Left:  left,
			Op:    op,
			Right: right,
		}, nil
	}

	return left, nil
}

func (p *Parser) term() (ast.Expr, error) {
	expr, err := p.factor()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.ADD || p.peek().Kind == token.SUB || p.peek().Kind == token.OR || p.peek().Kind == token.XOR {
		op := p.advance()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{
			Left:  expr,
			Op:    op,
			Right: right,
		}
	}

	return expr, nil
}

func (p *Parser) factor() (ast.Expr, error) {
	expr, err := p.unary()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.MUL || p.peek().Kind == token.QUO || p.peek().Kind == token.REM || p.peek().Kind == token.AND ||
		p.peek().Kind == token.AND_NOT || p.peek().Kind == token.SHL || p.peek().Kind == token.SHR {
		op := p.advance()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{
			Left:  expr,
			Op:    op,
			Right: right,
		}
	}

	return expr, nil
}

func (p *Parser) unary() (ast.Expr, error) {
	if p.peek().Kind == token.SUB || p.peek().Kind == token.NOT {
		op := p.advance()
		right, err := p.primary()
		if err != nil {
			return nil, err
		}
		return &ast.UnaryExpr{
			Op:    op,
			Right: right,
		}, nil
	}

	primary, err := p.primary()
	if err != nil {
		return nil, err
	}
	return primary, nil
}

func (p *Parser) primary() (ast.Expr, error) {
	switch p.peek().Kind {
	case token.FLOAT, token.INT, token.CHAR, token.STRING, token.BOOL, token.NIL:
		return &ast.BasicLit{Value: p.advance()}, nil
	}
	if p.peek().Kind == token.LPAREN {
		p.advance()
		expr, err := p.expression()
		if err != nil {
			return nil, err
		}
		if p.peek().Kind != token.RPAREN {
			return nil, fmt.Errorf("missing closing ')'")
		}
		p.advance()
		return &ast.ParenExpr{X: expr}, nil
	}
	return nil, fmt.Errorf("parse error")
}

func (p *Parser) advance() token.Token {
	t := p.toks[p.pos]
	if t.Kind != token.EOF {
		p.pos++
	}
	return t
}

func (p *Parser) peek() token.Token {
	return p.toks[p.pos]
}

func (p *Parser) peekNext() token.Token {
	if p.pos+1 >= len(p.toks) {
		return token.Token{token.EOF, nil, "", -1}
	}
	return p.toks[p.pos+1]
}

func (p *Parser) atEnd() bool {
	return p.toks[p.pos].Kind == token.EOF
}
