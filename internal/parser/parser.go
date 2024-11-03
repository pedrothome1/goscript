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

func (p *Parser) Init(src string) error {
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
--- Lexical Grammar ---
TYPE_NAME        -> IDENT | QUALIFIED_IDENT
QUALIFIED_IDENT  -> PACKAGE_NAME '.' IDENT
PACKAGE_NAME     -> IDENT
IDENT            -> ALPHA ( ALPHA | DIGIT )*
INT              -> DIGIT+
FLOAT            -> DIGIT+ '.' DIGIT+
CHAR             -> "'" <any char except '\' and "'"> | <any ESC_SEQ except '\"'> "'"
STRING           -> '"' ( <any char except '\' and '"' | <any ESC_SEQ except '\''> )* '"'
BOOL             -> 'false' | 'true'
NIL              -> 'nil'
BASIC_LIT        -> FLOAT | INT | CHAR | STRING | BOOL | NIL
ALPHA            -> 'A' ... 'Z' | 'a' ... 'z' | '_'
DIGIT            -> '0' ... '9'
ESC_SEQ          -> '\a' | '\b' | '\f' | '\n' | '\r' | '\t' | '\v' | '\\' | '\'' | '\"'

--- Syntax Grammar ---
program          -> statement* EOF

declaration      -> varDecl | funcDecl | statement
varDecl          -> 'var' IDENT ( type | type? '=' expression ) ';'
funcDecl         -> 'func' IDENT '(' parameters? ')' type? blockStmt

statement        -> simpleStmt | printStmt | blockStmt | ifStmt | forStmt | breakStmt | continueStmt | returnStmt
simpleStmt       -> exprStmt | incDecStmt | assignStmt | shortVarDecl
ifStmt           -> 'if' expression blockStmt ( 'else' ( blockStmt | ifStmt ) )?
forStmt          -> 'for' expression blockStmt
assignStmt       -> IDENT '=' expression ';'
exprStmt         -> expression ';'
printStmt        -> 'print' expression ';'
blockStmt        -> '{' declaration* '}' ';'
incDecStmt       -> IDENT ( '++' | '--' ) ';'
breakStmt        -> 'break' IDENT? ';'
continueStmt     -> 'continue' IDENT? ';'
returnStmt       -> 'return' expression? ';'
shortVarDecl     -> IDENT ':=' expression ';'

expression       -> logical_or
logical_or       -> logical_and ( ( '||' ) logical_and )*
logical_and      -> comparison ( ( '&&' ) comparison )*
comparison       -> term ( ( '==' | '!=' | '<' | '>' | '<=' | '>=' ) term )?
term             -> factor ( ( '+' | '-' | '|' | '^' ) factor )*
factor           -> unary ( ( '*' | '/' | '&' | '&^' | '<<' | '>>' ) unary )*
unary            -> ( '-' | '!' )? call
call             -> primary ( '(' arguments? ')' )*
primary          -> QUALIFIED_IDENT |
					IDENT |
					BASIC_LIT |
					'(' expression ')' |
					primary '.' IDENT |
					primary '[' expression ']' |
					primary '[' expression ':' expression ']'
					'[' ']' type

type             -> TYPE_NAME | slice_type | map_type | struct_type
slice_type       -> '[' ']' type ( '{' expression ( ',' expression )* '}' )?
map_type         -> map '[' type ']' type ( '{' expression ':' expression ( ',' expression ':' expression )* '}' )?
struct_type      -> struct '{' IDENT type ( ';' IDENT type )* '}' ( '{' expression ( ':' expression )? ( ',' expression ( ':' expression )? )* '}' )?
arguments        -> expression ( ',' expression )*
parameters       -> IDENT type ( ',' IDENT type )*
*/

func (p *Parser) Parse() ([]ast.Stmt, error) {
	var stmts []ast.Stmt
	for !p.atEnd() {
		d, err := p.declaration()
		if err != nil {
			return stmts, err
		}
		stmts = append(stmts, d)
	}
	return stmts, nil
}

func (p *Parser) declaration() (ast.Stmt, error) {
	if p.peek().Kind == token.VAR {
		return p.varDecl()
	}
	if p.peek().Kind == token.FUNC {
		return p.funcDecl()
	}
	return p.statement()
}

// For now, only parsing declarations of the form `var name = "expr"` without type
// TODO: implement according to the syntax grammar
func (p *Parser) varDecl() (ast.Stmt, error) {
	p.advance()
	if p.peek().Kind != token.IDENT {
		return nil, fmt.Errorf("variable name expected")
	}
	name := &ast.Ident{Name: p.advance()}
	if p.peek().Kind != token.ASSIGN {
		return nil, fmt.Errorf("assignment operator expected")
	}
	p.advance()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after variable declaration")
	}
	p.advance()
	return &ast.VarDecl{
		Name:  name,
		Type:  nil,
		Value: expr,
	}, nil
}

func (p *Parser) funcDecl() (ast.Stmt, error) {
	p.advance()
	if p.peek().Kind != token.IDENT {
		return nil, fmt.Errorf("function name expected")
	}
	name := &ast.Ident{Name: p.advance()}
	if p.peek().Kind != token.LPAREN {
		return nil, fmt.Errorf("'(' expected after function name")
	}
	p.advance()
	var params []*ast.Field
	for p.peek().Kind != token.RPAREN && !p.atEnd() {
		// <ident> <type>
		var f ast.Field
		if p.peek().Kind != token.IDENT {
			return nil, fmt.Errorf("expected parameter name")
		}
		f.Name = &ast.Ident{Name: p.advance()}
		typ, err := p.expression()
		if err != nil {
			return nil, err
		}
		f.Type = typ
		if p.peek().Kind == token.COMMA {
			p.advance()
		}
		params = append(params, &f)
		if len(params) > 255 {
			return nil, fmt.Errorf("can't have more than 255 parameters")
		}
	}
	if p.peek().Kind != token.RPAREN {
		return nil, fmt.Errorf("')' expected at the end of parameter list")
	}
	p.advance()
	var result *ast.Field
	if p.peek().Kind != token.LBRACE {
		typ, err := p.expression()
		if err != nil {
			return nil, err
		}
		result = &ast.Field{Type: typ}
	}
	if p.peek().Kind != token.LBRACE {
		return nil, fmt.Errorf("'{' expected for the function declaration body")
	}
	blockStmt, err := p.blockStmt()
	if err != nil {
		return nil, err
	}
	return &ast.FuncDecl{
		Name:   name,
		Params: params,
		Result: result,
		Body:   blockStmt.(*ast.BlockStmt).List,
	}, nil
}

func (p *Parser) statement() (ast.Stmt, error) {
	if p.peek().Kind == token.PRINT {
		return p.printStmt()
	}
	if p.peek().Kind == token.LBRACE {
		return p.blockStmt()
	}
	if p.peek().Kind == token.IF {
		return p.ifStmt()
	}
	if p.peek().Kind == token.FOR {
		return p.forStmt()
	}
	if p.peek().Kind == token.BREAK {
		return p.breakStmt()
	}
	if p.peek().Kind == token.CONTINUE {
		return p.continueStmt()
	}
	if p.peek().Kind == token.RETURN {
		return p.returnStmt()
	}
	return p.simpleStmt()
}

func (p *Parser) simpleStmt() (ast.Stmt, error) {
	// TODO: this will change when assigning to "compound" lvalues
	if p.peek().Kind == token.IDENT {
		if p.peekNext().Kind == token.ASSIGN {
			return p.assignStmt()
		}
		if p.peekNext().Kind == token.DEFINE {
			return p.shortVarDecl()
		}
		if p.peekNext().Kind == token.INC || p.peekNext().Kind == token.DEC {
			return p.incDecStmt()
		}
	}
	return p.expressionStmt()
}

func (p *Parser) printStmt() (ast.Stmt, error) {
	p.advance()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after print statement")
	}
	p.advance()
	return &ast.PrintStmt{Expr: expr}, nil
}

func (p *Parser) assignStmt() (ast.Stmt, error) {
	ident := &ast.Ident{Name: p.advance()}
	p.advance() // consume '='
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after assignment statement")
	}
	p.advance()
	return &ast.AssignStmt{Name: ident, Value: expr}, nil
}

func (p *Parser) shortVarDecl() (ast.Stmt, error) {
	ident := &ast.Ident{Name: p.advance()}
	p.advance() // consume ':='
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after short variable declaration")
	}
	p.advance()
	return &ast.VarDecl{
		Name:  ident,
		Type:  nil,
		Value: expr,
	}, nil
}

func (p *Parser) blockStmt() (ast.Stmt, error) {
	list, err := p.block()
	if err != nil {
		return nil, err
	}
	return &ast.BlockStmt{List: list}, nil
}

// TODO: support simple initialization statement
func (p *Parser) ifStmt() (ast.Stmt, error) {
	p.advance()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.LBRACE {
		return nil, fmt.Errorf("'{' expected after if statement expression")
	}
	block, err := p.blockStmt()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.ELSE {
		return &ast.IfStmt{
			Cond: expr,
			Body: block.(*ast.BlockStmt),
			Else: nil,
		}, nil
	}
	p.advance()
	if p.peek().Kind == token.LBRACE {
		elseStmt, err := p.blockStmt()
		if err != nil {
			return nil, err
		}
		return &ast.IfStmt{
			Cond: expr,
			Body: block.(*ast.BlockStmt),
			Else: elseStmt,
		}, nil
	}
	if p.peek().Kind == token.IF {
		elseStmt, err := p.ifStmt()
		if err != nil {
			return nil, err
		}
		return &ast.IfStmt{
			Cond: expr,
			Body: block.(*ast.BlockStmt),
			Else: elseStmt,
		}, nil
	}
	return nil, fmt.Errorf("unexpected token %q after 'else'", p.peek().Lexeme)
}

func (p *Parser) forStmt() (ast.Stmt, error) {
	p.advance() // consume 'for'
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.LBRACE {
		return nil, fmt.Errorf("'{' expected for 'for' loop body")
	}
	body, err := p.blockStmt()
	if err != nil {
		return nil, err
	}
	return &ast.ForStmt{
		Cond: expr,
		Body: body.(*ast.BlockStmt),
	}, nil
}

// TODO: unify break and continue into one method?
// TODO: implement labels
func (p *Parser) breakStmt() (ast.Stmt, error) {
	branchTok := p.advance()
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after %q statement", branchTok.Lexeme)
	}
	p.advance()
	return &ast.BranchStmt{
		Tok:   branchTok,
		Label: nil,
	}, nil
}

// TODO: implement labels
func (p *Parser) continueStmt() (ast.Stmt, error) {
	branchTok := p.advance()
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after %q statement", branchTok.Lexeme)
	}
	p.advance()
	return &ast.BranchStmt{
		Tok:   branchTok,
		Label: nil,
	}, nil
}

func (p *Parser) returnStmt() (ast.Stmt, error) {
	p.advance() // consume 'return'
	if p.peek().Kind == token.SEMICOLON {
		p.advance()
		return &ast.ReturnStmt{Result: nil}, nil
	}
	result, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after 'return' statement")
	}
	p.advance()
	return &ast.ReturnStmt{Result: result}, nil
}

func (p *Parser) incDecStmt() (ast.Stmt, error) {
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	tok := p.advance()
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after %q statement", tok.Lexeme)
	}
	p.advance()
	return &ast.IncDecStmt{
		Expr: expr,
		Tok:  tok,
	}, nil
}

func (p *Parser) expressionStmt() (ast.Stmt, error) {
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, fmt.Errorf("';' expected after expression statement")
	}
	p.advance()
	return &ast.ExprStmt{Expr: expr}, nil
}

func (p *Parser) block() ([]ast.Stmt, error) {
	p.advance() // consume '{'
	var list []ast.Stmt
	for p.peek().Kind != token.RBRACE && !p.atEnd() {
		decl, err := p.declaration()
		if err != nil {
			return nil, err
		}
		list = append(list, decl)
	}
	if p.peek().Kind != token.RBRACE {
		return nil, fmt.Errorf("'}' expected after block")
	}
	if p.advance(); p.peek().Kind == token.SEMICOLON {
		p.advance()
	}
	return list, nil
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
		right, err := p.call()
		if err != nil {
			return nil, err
		}
		return &ast.UnaryExpr{
			Op:    op,
			Right: right,
		}, nil
	}

	primary, err := p.call()
	if err != nil {
		return nil, err
	}
	return primary, nil
}

func (p *Parser) call() (ast.Expr, error) {
	callee, err := p.primary()
	if err != nil {
		return nil, err
	}

	for p.peek().Kind == token.LPAREN {
		p.advance()
		var args []ast.Expr
		for p.peek().Kind != token.RPAREN && !p.atEnd() {
			arg, err := p.expression()
			if err != nil {
				return nil, err
			}
			args = append(args, arg)
			if len(args) > 255 {
				return nil, fmt.Errorf("can't have more than 255 arguments")
			}
			if p.peek().Kind == token.COMMA {
				p.advance()
			}
		}
		if p.peek().Kind != token.RPAREN {
			return nil, fmt.Errorf("')' expected in call expression")
		}
		p.advance()
		callee = &ast.CallExpr{
			Callee: callee,
			Args:   args,
		}
	}

	return callee, nil
}

func (p *Parser) primary() (ast.Expr, error) {
	switch p.peek().Kind {
	case token.FLOAT, token.INT, token.CHAR, token.STRING, token.BOOL, token.NIL:
		return &ast.BasicLit{Value: p.advance()}, nil
	}
	if p.peek().Kind == token.IDENT {
		if p.peekNext().Kind == token.PERIOD {
			var x ast.Expr = &ast.Ident{Name: p.peek()}
			for p.peekNext().Kind == token.PERIOD {
				p.advance() // consume IDENT
				if t := p.peekNext(); t.Kind != token.IDENT {
					return nil, fmt.Errorf("expected identifier after period, got %q at %s", t.Lexeme, t.Pos())
				}
				p.advance() // consume PERIOD
				x = &ast.SelectorExpr{
					X:   x,
					Sel: &ast.Ident{Name: p.peek()},
				}
			}
			p.advance()
			return x, nil
		}
		if p.peekNext().Kind == token.LBRACK {
			var x ast.Expr = &ast.Ident{Name: p.peek()}
			for p.peekNext().Kind == token.LBRACK {
				p.advance() // consume IDENT or RBRACK
				var lowidx, high ast.Expr
				var err error
				if p.peekNext().Kind == token.COLON {
					lowidx = &ast.BasicLit{Value: token.NewInt(0)}
					p.advance()
				} else {
					p.advance()
					lowidx, err = p.expression()
					if err != nil {
						return nil, err
					}
				}
				if p.peek().Kind == token.COLON {
					if p.peekNext().Kind == token.RBRACK {
						high = &ast.CallExpr{
							Callee: &ast.Ident{Name: token.NewIdent("len")},
							Args:   []ast.Expr{x},
						}
						p.advance()
					} else {
						p.advance()
						high, err = p.expression()
						if err != nil {
							return nil, err
						}
					}
				}
				if high != nil {
					x = &ast.SliceExpr{
						X:    x,
						Low:  lowidx,
						High: high,
					}
				} else {
					x = &ast.IndexExpr{
						X:     x,
						Index: lowidx,
					}
				}
				if p.peek().Kind != token.RBRACK {
					return nil, fmt.Errorf("']' expected at %s", p.peek().Pos())
				}
			}
			p.advance()
			return x, nil
		}
		return &ast.Ident{Name: p.advance()}, nil
	}
	if p.peek().Kind == token.LBRACK && p.peekNext().Kind == token.RBRACK {
		p.advance()
		p.advance()
		typ, err := p.primary()
		if err != nil {
			return nil, err
		}
		sliceType := &ast.SliceType{Elt: typ}
		if p.peek().Kind == token.LBRACE {
			p.advance()
			var elts []ast.Expr
			for p.peek().Kind != token.RBRACE && !p.atEnd() {
				expr, err := p.expression()
				if err != nil {
					return nil, err
				}
				elts = append(elts, expr)
				if p.peek().Kind == token.RBRACE {
					break
				}
				if p.peek().Kind != token.COMMA {
					return nil, fmt.Errorf("',' expected between slice elements")
				}
				p.advance()
			}
			if p.peek().Kind != token.RBRACE {
				return nil, fmt.Errorf("'}' expected in slice composite literal")
			}
			p.advance()
			return &ast.CompositeLit{
				Type: sliceType,
				Elts: elts,
			}, nil
		}
		return sliceType, nil
	}
	if p.peek().Kind == token.MAP {
		if p.peekNext().Kind != token.LBRACK {
			return nil, fmt.Errorf("'[' expected at %s for the map key type", p.peekNext().Pos())
		}
		p.advance()
		p.advance()
		keyType, err := p.primary()
		if err != nil {
			return nil, err
		}
		if p.peek().Kind != token.RBRACK {
			return nil, fmt.Errorf("']' expected at %s for the map key type", p.peekNext().Pos())
		}
		p.advance()
		valType, err := p.primary()
		if err != nil {
			return nil, err
		}
		mapType := &ast.MapType{
			Key:   keyType,
			Value: valType,
		}
		if p.peek().Kind == token.LBRACE {
			p.advance()
			var kvs []ast.Expr
			for p.peek().Kind != token.RBRACE && !p.atEnd() {
				key, err := p.expression()
				if err != nil {
					return nil, err
				}
				if p.peek().Kind != token.COLON {
					return nil, fmt.Errorf("':' expected between key and value for map literal")
				}
				p.advance()
				val, err := p.expression()
				if err != nil {
					return nil, err
				}
				kvs = append(kvs, &ast.KeyValueExpr{
					Key:   key,
					Value: val,
				})
				if p.peek().Kind == token.RBRACE {
					break
				}
				if p.peek().Kind != token.COMMA {
					return nil, fmt.Errorf("',' expected between map literal key-value pairs")
				}
				p.advance()
			}
			if p.peek().Kind != token.RBRACE {
				return nil, fmt.Errorf("'}' expected at the end of map composite literal")
			}
			p.advance()
			return &ast.CompositeLit{
				Type: mapType,
				Elts: kvs,
			}, nil
		}
		return mapType, nil
	}
	if p.peek().Kind == token.STRUCT {
		if p.peekNext().Kind != token.LBRACE {
			return nil, fmt.Errorf("'{' expected at %s for struct literal", p.peekNext().Pos())
		}
		p.advance()
		p.advance()
		var fields []*ast.Field
		for p.peek().Kind != token.RBRACE && !p.atEnd() {
			id, err := p.primary()
			if err != nil {
				return nil, err
			}
			if _, ok := id.(*ast.Ident); !ok {
				return nil, fmt.Errorf("field name should be a valid identifier")
			}
			typ, err := p.primary()
			if err != nil {
				return nil, err
			}
			fields = append(fields, &ast.Field{
				Name: id.(*ast.Ident),
				Type: typ,
			})
			if p.peek().Kind == token.RBRACE {
				break
			} else if p.peek().Kind != token.SEMICOLON {
				return nil, fmt.Errorf("';' expected at %s for struct literal field", p.peek().Pos())
			}
			p.advance()
		}
		if p.peek().Kind != token.RBRACE {
			return nil, fmt.Errorf("'}' expected at %s for struct literal", p.peek().Pos())
		}
		p.advance()
		structType := &ast.StructType{Fields: fields}
		if p.peek().Kind == token.LBRACE {
			p.advance()
			var exprs []ast.Expr
			for p.peek().Kind != token.RBRACE && !p.atEnd() {
				valname, err := p.expression()
				if err != nil {
					return nil, err
				}
				if p.peek().Kind == token.COLON {
					p.advance()
					val, err := p.expression()
					if err != nil {
						return nil, err
					}
					exprs = append(exprs, &ast.KeyValueExpr{
						Key:   valname,
						Value: val,
					})
				} else {
					exprs = append(exprs, valname)
				}
				if p.peek().Kind == token.RBRACE {
					break
				}
				if p.peek().Kind != token.COMMA {
					return nil, fmt.Errorf("',' expected between struct literal key-value pairs")
				}
				p.advance()
			}
			if p.peek().Kind != token.RBRACE {
				return nil, fmt.Errorf("'}' expected at the end of struct composite literal")
			}
			p.advance()
			return &ast.CompositeLit{
				Type: structType,
				Elts: exprs,
			}, nil
		}
		return structType, nil
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
	return nil, fmt.Errorf("invalid primary expression at %s", p.peek().Pos())
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
		return token.Token{token.EOF, nil, "", -1, -1}
	}
	return p.toks[p.pos+1]
}

func (p *Parser) atEnd() bool {
	return p.toks[p.pos].Kind == token.EOF
}
