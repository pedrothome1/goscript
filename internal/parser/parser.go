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
	return nil
}

/*
expression -> term
term       -> factor ( ( '+' | '-' ) factor )*
factor     -> unary ( ( '*' | '/' ) unary )*
unary      -> ( '-' )? primary
primary    -> FLOAT | INT | '(' expression ')'
*/

func (p *Parser) Parse() (ast.Expr, error) {
	return p.expression()
}

func (p *Parser) expression() (ast.Expr, error) {
	return p.term()
}

func (p *Parser) term() (ast.Expr, error) {
	expr, err := p.factor()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.ADD || p.peek().Kind == token.SUB {
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

	for p.peek().Kind == token.MUL || p.peek().Kind == token.QUO || p.peek().Kind == token.REM {
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
	if p.peek().Kind == token.SUB {
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
	case token.FLOAT, token.INT:
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
		return token.Token{token.EOF, nil, ""}
	}
	return p.toks[p.pos+1]
}

func (p *Parser) atEnd() bool {
	return p.toks[p.pos].Kind == token.EOF
}
