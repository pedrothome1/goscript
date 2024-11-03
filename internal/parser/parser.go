package parser

import (
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
		return nil, parseErrorf("variable name expected")
	}
	name := &ast.Ident{Name: p.advance()}
	if p.peek().Kind != token.ASSIGN {
		return nil, parseErrorf("assignment operator expected")
	}
	p.advance()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.SEMICOLON {
		return nil, parseErrorf("';' expected after variable declaration")
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
	if !p.match(token.IDENT) {
		return nil, parseErrorf("function name expected")
	}
	name := &ast.Ident{Name: p.previous()}
	if !p.match(token.LPAREN) {
		return nil, parseErrorf("'(' expected after function name")
	}
	params, err := p.fieldsSignature(true)
	if err != nil {
		return nil, err
	}
	if !p.match(token.RPAREN) {
		return nil, parseErrorf("')' expected at the end of parameter list")
	}
	result, err := p.funcResult()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.LBRACE {
		return nil, parseErrorf("'{' expected for the function declaration body")
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
		return nil, parseErrorf("';' expected after print statement")
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
		return nil, parseErrorf("';' expected after assignment statement")
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
		return nil, parseErrorf("';' expected after short variable declaration at %s %s", p.peek().String(), p.peek().Pos())
	}
	p.advance()
	return &ast.VarDecl{
		Name:  ident,
		Type:  nil,
		Value: expr,
	}, nil
}

func (p *Parser) blockStmt() (ast.Stmt, error) {
	list, err := p.block(true)
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
		return nil, parseErrorf("'{' expected after if statement expression")
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
	return nil, parseErrorf("unexpected token %q after 'else'", p.peek().Lexeme)
}

func (p *Parser) forStmt() (ast.Stmt, error) {
	p.advance() // consume 'for'
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.LBRACE {
		return nil, parseErrorf("'{' expected for 'for' loop body")
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
		return nil, parseErrorf("';' expected after %q statement", branchTok.Lexeme)
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
		return nil, parseErrorf("';' expected after %q statement", branchTok.Lexeme)
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
		return nil, parseErrorf("';' expected after 'return' statement")
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
		return nil, parseErrorf("';' expected after %q statement", tok.Lexeme)
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
		return nil, parseErrorf("';' expected after expression statement")
	}
	p.advance()
	return &ast.ExprStmt{Expr: expr}, nil
}

func (p *Parser) block(eatSemi bool) ([]ast.Stmt, error) {
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
		return nil, parseErrorf("'}' expected after block")
	}
	if p.advance(); p.peek().Kind == token.SEMICOLON {
		if eatSemi {
			p.advance()
		}
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
				return nil, parseErrorf("can't have more than 255 arguments")
			}
			if p.peek().Kind == token.COMMA {
				p.advance()
			}
		}
		if p.peek().Kind != token.RPAREN {
			return nil, parseErrorf("')' expected in call expression")
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
	if p.peek().Kind == token.LPAREN {
		return p.parenExpr()
	}
	if p.peek().Kind == token.LBRACE {
		return p.compositeLit(nil)
	}
	typ, err := p.typeName()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind == token.LBRACE {
		if ft, ok := typ.(*ast.FuncType); ok {
			return p.funcLit(ft)
		}
		return p.compositeLit(typ)
	}
	if p.peek().Kind == token.LBRACK {
		return p.indexSliceExpr(typ)
	}
	return typ, nil
}

func (p *Parser) typeName() (ast.Expr, error) {
	if p.peek().Kind == token.IDENT {
		return p.ident()
	}
	if p.peek().Kind == token.LBRACK && p.peekNext().Kind == token.RBRACK {
		return p.sliceType()
	}
	if p.peek().Kind == token.MAP {
		return p.mapType()
	}
	if p.peek().Kind == token.STRUCT {
		return p.structType()
	}
	if p.peek().Kind == token.FUNC {
		return p.funcType()
	}
	return nil, parseErrorf("invalid type at %s %s", p.peek().String(), p.peek().Pos())
}

func (p *Parser) ident() (ast.Expr, error) {
	if p.peekNext().Kind == token.PERIOD {
		return p.selectorExpr()
	}
	return &ast.Ident{Name: p.advance()}, nil
}

func (p *Parser) selectorExpr() (ast.Expr, error) {
	var x ast.Expr = &ast.Ident{Name: p.peek()}
	for p.peekNext().Kind == token.PERIOD {
		p.advance() // consume IDENT
		if t := p.peekNext(); t.Kind != token.IDENT {
			return nil, parseErrorf("expected identifier after period, got %q at %s", t.Lexeme, t.Pos())
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

func (p *Parser) indexSliceExpr(typ ast.Expr) (ast.Expr, error) {
	for p.peek().Kind == token.LBRACK {
		p.advance() // consume LBRACK
		var lowidx, high ast.Expr
		var err error
		if p.peek().Kind == token.COLON {
			lowidx = &ast.BasicLit{Value: token.NewInt(0)}
		} else {
			lowidx, err = p.expression()
			if err != nil {
				return nil, err
			}
		}
		if p.peek().Kind == token.COLON {
			if p.peekNext().Kind == token.RBRACK {
				high = &ast.CallExpr{
					Callee: &ast.Ident{Name: token.NewIdent("len")},
					Args:   []ast.Expr{typ},
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
			typ = &ast.SliceExpr{
				X:    typ,
				Low:  lowidx,
				High: high,
			}
		} else {
			typ = &ast.IndexExpr{
				X:     typ,
				Index: lowidx,
			}
		}
		if p.peek().Kind != token.RBRACK {
			return nil, parseErrorf("']' expected at %s", p.peek().Pos())
		}
		p.advance()
	}
	return typ, nil
}

func (p *Parser) sliceType() (ast.Expr, error) {
	p.advance()
	p.advance()
	typ, err := p.typeName()
	if err != nil {
		return nil, err
	}
	return &ast.SliceType{Elt: typ}, nil
}

func (p *Parser) mapType() (ast.Expr, error) {
	if p.peekNext().Kind != token.LBRACK {
		return nil, parseErrorf("'[' expected at %s for the map key type", p.peekNext().Pos())
	}
	p.advance()
	p.advance()
	keyType, err := p.typeName()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.RBRACK {
		return nil, parseErrorf("']' expected at %s for the map key type", p.peekNext().Pos())
	}
	p.advance()
	valType, err := p.typeName()
	if err != nil {
		return nil, err
	}
	return &ast.MapType{
		Key:   keyType,
		Value: valType,
	}, nil
}

func (p *Parser) structType() (ast.Expr, error) {
	if p.peekNext().Kind != token.LBRACE {
		return nil, parseErrorf("'{' expected at %s for struct literal", p.peekNext().Pos())
	}
	p.advance()
	p.advance()
	var fields []*ast.Field
	for p.peek().Kind != token.RBRACE && !p.atEnd() {
		name, err := p.typeName()
		if err != nil {
			return nil, err
		}
		if _, ok := name.(*ast.Ident); !ok {
			return nil, parseErrorf("field name should be a valid identifier")
		}
		typ, err := p.typeName()
		if err != nil {
			return nil, err
		}
		fields = append(fields, &ast.Field{
			Name: name.(*ast.Ident),
			Type: typ,
		})
		if p.peek().Kind == token.RBRACE {
			break
		}
		if p.peek().Kind != token.SEMICOLON {
			return nil, parseErrorf("';' expected at %s for struct literal field", p.peek().Pos())
		}
		p.advance()
	}
	if p.peek().Kind != token.RBRACE {
		return nil, parseErrorf("'}' expected at %s for struct literal", p.peek().Pos())
	}
	p.advance()
	return &ast.StructType{Fields: fields}, nil
}

func (p *Parser) funcType() (ast.Expr, error) {
	p.advance()
	if !p.match(token.LPAREN) {
		return nil, parseErrorf("expected '(' after 'func'")
	}
	params, err := p.fieldsSignature(false)
	if err != nil {
		return nil, err
	}
	if !p.match(token.RPAREN) {
		return nil, parseErrorf("expected ')' after parameter list")
	}
	result, err := p.funcResult()
	if err != nil {
		return nil, err
	}
	return &ast.FuncType{
		Params: params,
		Result: result,
	}, nil
}

func (p *Parser) funcResult() ([]*ast.Field, error) {
	if p.match(token.LPAREN) {
		results, err := p.fieldsSignature(false)
		if err != nil {
			return nil, err
		}
		if !p.match(token.RPAREN) {
			return nil, parseErrorf("expected ')' after result list")
		}
		return results, nil
	}
	if p.peek().Kind == token.LBRACE {
		return nil, nil
	}
	result, err := p.typeName()
	if err != nil {
		return nil, err
	}
	return []*ast.Field{{Type: result}}, nil
}

func (p *Parser) fieldsSignature(requireNames bool) ([]*ast.Field, error) {
	if p.peek().Kind == token.RPAREN {
		return nil, nil
	}

	var fields []*ast.Field

	for {
		var exprs []ast.Expr
		for {
			left, err := p.typeName()
			if err != nil {
				return nil, err
			}
			exprs = append(exprs, left)
			if !p.match(token.COMMA) {
				break
			}
		}
		if p.peek().Kind == token.RPAREN {
			if requireNames {
				return nil, parseErrorf("parameter names are required")
			}
			for _, typ := range exprs {
				fields = append(fields, &ast.Field{
					Type: typ,
				})
			}
			break
		}
		rightType, err := p.typeName()
		if err != nil {
			return nil, err
		}
		names := make([]*ast.Ident, 0, len(exprs))
		for _, e := range exprs {
			id, ok := e.(*ast.Ident)
			if !ok {
				return nil, parseErrorf("parameter name should be a valid identifier")
			}
			names = append(names, id)
		}
		fields = append(fields, &ast.Field{Names: names, Type: rightType})
		if !p.match(token.COMMA) {
			break
		}
	}

	return fields, nil
}

func (p *Parser) funcLit(typ *ast.FuncType) (ast.Expr, error) {
	body, err := p.block(false)
	if err != nil {
		return nil, err
	}
	return &ast.FuncLit{
		Type: typ,
		Body: body,
	}, nil
}

func (p *Parser) compositeLit(typ ast.Expr) (ast.Expr, error) {
	p.advance()
	var exprs []ast.Expr
	for p.peek().Kind != token.RBRACE && !p.atEnd() {
		key, err := p.expression()
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
				Key:   key,
				Value: val,
			})
		} else {
			exprs = append(exprs, key)
		}
		if p.peek().Kind == token.RBRACE {
			break
		}
		if p.peek().Kind != token.COMMA {
			return nil, parseErrorf("',' expected between composite literal entries")
		}
		p.advance()
	}
	if p.peek().Kind != token.RBRACE {
		return nil, parseErrorf("'}' expected at the end of composite literal")
	}
	p.advance()
	return &ast.CompositeLit{
		Type: typ,
		Elts: exprs,
	}, nil
}

func (p *Parser) parenExpr() (ast.Expr, error) {
	p.advance()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != token.RPAREN {
		return nil, parseErrorf("missing closing ')'")
	}
	p.advance()
	return &ast.ParenExpr{X: expr}, nil
}

// -- helpers --
func (p *Parser) advance() token.Token {
	t := p.toks[p.pos]
	if t.Kind != token.EOF {
		p.pos++
	}
	return t
}

func (p *Parser) match(k token.Kind) bool {
	if p.peek().Kind == k {
		p.advance()
		return true
	}
	return false
}

func (p *Parser) previous() token.Token {
	if p.pos-1 >= 0 {
		return p.toks[p.pos-1]
	}
	return token.NewIllegal()
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
