package scanner

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/token"
	"strconv"
)

type Scanner struct {
	src   string
	start int
	pos   int
	toks  []token.Token
}

func (s *Scanner) Init(src []byte) {
	s.src = string(src)
	s.start = 0
	s.pos = 0
	s.toks = make([]token.Token, 0)
}

func (s *Scanner) Scan() ([]token.Token, error) {
	for !s.atEnd() {
		s.start = s.pos
		ch := s.advance()

		switch ch {
		case '+':
			s.addToken(token.ADD, nil)
		case '-':
			s.addToken(token.SUB, nil)
		case '*':
			s.addToken(token.MUL, nil)
		case '/':
			s.addToken(token.QUO, nil)
		case '%':
			s.addToken(token.REM, nil)
		case '(':
			s.addToken(token.LPAREN, nil)
		case ')':
			s.addToken(token.RPAREN, nil)
		case ' ', '\n', '\r', '\t':
			break
		default:
			if s.isDigit(ch) {
				if err := s.addNumber(); err != nil {
					return s.toks, err
				}
			} else {
				s.addToken(token.ILLEGAL, nil)
				return s.toks, fmt.Errorf("invalid character: %q", ch)
			}
		}
	}
	s.toks = append(s.toks, token.Token{token.EOF, nil, ""})
	return s.toks, nil
}

func (s *Scanner) addToken(kind token.Kind, lit any) {
	s.toks = append(s.toks, token.Token{kind, lit, s.src[s.start:s.pos]})
}

func (s *Scanner) addNumber() error {
	for s.isDigit(s.peek()) {
		s.advance()
	}
	if s.peek() == '.' {
		if s.advance(); s.isDigit(s.peek()) {
			for s.isDigit(s.peek()) {
				s.advance()
			}
			num, err := strconv.ParseFloat(s.src[s.start:s.pos], 64)
			if err != nil {
				return err
			}
			s.addToken(token.FLOAT, num)
			return nil
		}
	}
	num, err := strconv.Atoi(s.src[s.start:s.pos])
	if err != nil {
		return err
	}
	s.addToken(token.INT, num)
	return nil
}

func (s *Scanner) atEnd() bool {
	return s.pos >= len(s.src)
}

func (s *Scanner) advance() byte {
	if s.atEnd() {
		return '\x00'
	}
	ch := s.src[s.pos]
	s.pos++
	return ch
}

func (s *Scanner) peek() byte {
	if s.atEnd() {
		return '\x00'
	}
	return s.src[s.pos]
}

func (s *Scanner) isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}
