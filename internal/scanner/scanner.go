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
		case '!':
			s.addToken(token.NOT, nil)
		case ' ', '\n', '\r', '\t':
			break
		case '\'':
			if err := s.addChar(); err != nil {
				return s.toks, err
			}
		case '"':
			if err := s.addString(); err != nil {
				return s.toks, err
			}
		case '`':
			if err := s.addRawString(); err != nil {
				return s.toks, err
			}
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

func (s *Scanner) addChar() error {
	if s.peek() == '\'' {
		return fmt.Errorf("empty char literal")
	}
	if s.peek() == '\n' {
		return fmt.Errorf("char literal with newline")
	}
	var escaped uint8
	if s.peek() == '\\' {
		s.advance()
		switch s.peek() {
		case 'a':
			escaped = '\a'
		case 'b':
			escaped = '\b'
		case 'f':
			escaped = '\f'
		case 'n':
			escaped = '\n'
		case 'r':
			escaped = '\r'
		case 't':
			escaped = '\t'
		case 'v':
			escaped = '\v'
		case '\\':
			escaped = '\\'
		case '\'':
			escaped = '\''
		default:
			return fmt.Errorf("invalid char literal escape sequence")
		}
	}
	if s.advance(); s.peek() != '\'' {
		return fmt.Errorf("char literal with more than one character")
	}
	s.advance()
	if escaped != 0 {
		s.toks = append(s.toks, token.Token{token.CHAR, int(escaped), s.src[s.start:s.pos]})
	} else {
		s.toks = append(s.toks, token.Token{token.CHAR, int(s.src[s.start+1]), s.src[s.start:s.pos]})
	}
	return nil
}

func (s *Scanner) addString() error {
	var b []byte
	for s.peek() != '"' && !s.atEnd() {
		if s.peek() == '\n' {
			return fmt.Errorf("string literal with newline")
		}
		if s.peek() == '\\' {
			s.advance()
			switch s.peek() {
			case 'a':
				b = append(b, '\a')
			case 'b':
				b = append(b, '\b')
			case 'f':
				b = append(b, '\f')
			case 'n':
				b = append(b, '\n')
			case 'r':
				b = append(b, '\r')
			case 't':
				b = append(b, '\t')
			case 'v':
				b = append(b, '\v')
			case '\\':
				b = append(b, '\\')
			case '"':
				b = append(b, '"')
			default:
				return fmt.Errorf("invalid string literal escape sequence")
			}
		} else {
			b = append(b, s.peek())
		}
		s.advance()
	}
	if s.atEnd() {
		return fmt.Errorf("unterminated string literal")
	}
	s.advance()
	if len(b) == 0 {
		s.toks = append(s.toks, token.Token{token.STRING, "", `""`})
		return nil
	}
	s.toks = append(s.toks, token.Token{token.STRING, string(b), s.src[s.start:s.pos]})
	return nil
}

func (s *Scanner) addRawString() error {
	var b []byte
	for s.peek() != '`' && !s.atEnd() {
		if s.peek() != '\r' {
			b = append(b, s.peek())
		}
		s.advance()
	}
	if s.atEnd() {
		return fmt.Errorf("unterminated raw string literal")
	}
	s.advance()
	if len(b) == 0 {
		s.toks = append(s.toks, token.Token{token.STRING, ``, "``"})
		return nil
	}
	s.toks = append(s.toks, token.Token{token.STRING, string(b), s.src[s.start:s.pos]})
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
