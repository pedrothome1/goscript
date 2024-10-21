package scanner

import (
	"fmt"
	"github.com/pedrothome1/goscript/internal/token"
	"strconv"
	"strings"
)

type Scanner struct {
	src   string
	start int
	pos   int
	line  int
	col   int
	toks  []token.Token
}

func (s *Scanner) Init(src []byte) {
	s.src = string(src)
	s.start = 0
	s.pos = 0
	s.line = 1
	s.col = 0
	s.toks = make([]token.Token, 0)
}

func (s *Scanner) Scan() ([]token.Token, error) {
	for !s.atEnd() {
		s.start = s.pos
		ch := s.advance()

		switch ch {
		case '+':
			if s.peek() == '+' {
				s.addToken(token.INC, nil)
				s.advance()
			} else {
				s.addToken(token.ADD, nil)
			}
		case '-':
			if s.peek() == '-' {
				s.addToken(token.DEC, nil)
				s.advance()
			} else {
				s.addToken(token.SUB, nil)
			}
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
		case '[':
			s.addToken(token.LBRACK, nil)
		case ']':
			s.addToken(token.RBRACK, nil)
		case '{':
			s.addToken(token.LBRACE, nil)
		case '}':
			s.addToken(token.RBRACE, nil)
		case ';':
			s.addToken(token.SEMICOLON, nil)
		case ':':
			if s.peek() == '=' {
				s.addToken(token.DEFINE, nil)
				s.advance()
			} else {
				s.addToken(token.COLON, nil)
			}
		case ',':
			s.addToken(token.COMMA, nil)
		case '.':
			if s.peek() == '.' && s.peekNext() == '.' {
				s.addToken(token.ELLIPSIS, nil)
				s.advance()
				s.advance()
			} else {
				s.addToken(token.PERIOD, nil)
			}
		case '^':
			s.addToken(token.XOR, nil)
		case '<':
			if s.peek() == '<' {
				s.addToken(token.SHL, nil)
				s.advance()
			} else if s.peek() == '=' {
				s.addToken(token.LEQ, nil)
				s.advance()
			} else if s.peek() == '-' {
				s.addToken(token.ARROW, nil)
				s.advance()
			} else {
				s.addToken(token.LSS, nil)
			}
		case '>':
			if s.peek() == '>' {
				s.addToken(token.SHR, nil)
				s.advance()
			} else if s.peek() == '=' {
				s.addToken(token.GEQ, nil)
				s.advance()
			} else {
				s.addToken(token.GTR, nil)
			}
		case '!':
			if s.peek() == '=' {
				s.addToken(token.NEQ, nil)
				s.advance()
			} else {
				s.addToken(token.NOT, nil)
			}
		case '=':
			if s.peek() == '=' {
				s.addToken(token.EQL, nil)
				s.advance()
			} else {
				s.addToken(token.ASSIGN, nil)
			}
		case '|':
			if s.peek() == '|' {
				s.addToken(token.LOR, nil)
				s.advance()
			} else {
				s.addToken(token.OR, nil)
			}
		case '&':
			if s.peek() == '&' {
				s.addToken(token.LAND, nil)
				s.advance()
			} else if s.peek() == '^' {
				s.addToken(token.AND_NOT, nil)
				s.advance()
			} else {
				s.addToken(token.AND, nil)
			}
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
		case '\n':
			s.line++
			s.col = 0
			s.addAutoSemi()
		case ' ', '\r', '\t':
			break
		default:
			if s.isDigit(ch) {
				if err := s.addNumber(); err != nil {
					return s.toks, err
				}
			} else if s.isAlpha(ch) {
				if err := s.addIdentifier(); err != nil {
					return s.toks, err
				}
			} else {
				s.addToken(token.ILLEGAL, nil)
				return s.toks, s.scanError(ch)
			}
		}
	}
	s.addAutoSemi()
	s.toks = append(s.toks, token.Token{token.EOF, nil, "", s.pos})
	return s.toks, nil
}

func (s *Scanner) addToken(kind token.Kind, lit any) {
	s.toks = append(s.toks, token.Token{kind, lit, s.src[s.start:s.pos], s.start})
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

func (s *Scanner) addIdentifier() error {
	for s.isAlphaNumeric(s.peek()) {
		s.advance()
	}
	if t, ok := token.Keyword(s.src[s.start:s.pos]); ok {
		s.addToken(t.Kind, t.Lit)
		return nil
	}
	s.addToken(token.IDENT, nil)
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
		s.addToken(token.CHAR, int(escaped))
	} else {
		s.addToken(token.CHAR, int(s.src[s.start+1]))
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
		s.addToken(token.STRING, "")
		return nil
	}
	s.addToken(token.STRING, string(b))
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
		s.addToken(token.STRING, ``)
		return nil
	}
	s.addToken(token.STRING, string(b))
	return nil
}

func (s *Scanner) addAutoSemi() {
	switch s.toks[len(s.toks)-1].Kind {
	case token.IDENT, token.NIL, token.INT, token.BOOL, token.FLOAT, token.CHAR, token.STRING, token.BREAK,
		token.CONTINUE, token.FALLTHROUGH, token.RETURN, token.INC, token.DEC, token.RPAREN, token.RBRACK, token.RBRACE:
		s.toks = append(s.toks, token.Token{token.SEMICOLON, nil, ";", -1})
	}
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
	s.col++
	return ch
}

func (s *Scanner) peek() byte {
	if s.atEnd() {
		return '\x00'
	}
	return s.src[s.pos]
}

func (s *Scanner) peekNext() byte {
	if s.pos+1 >= len(s.src) {
		return '\x00'
	}
	return s.src[s.pos+1]
}

func (s *Scanner) isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func (s *Scanner) isAlpha(c byte) bool {
	return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_'
}

func (s *Scanner) isAlphaNumeric(c byte) bool {
	return s.isAlpha(c) || s.isDigit(c)
}

type ScanError struct {
	message string
	details string
}

func (e *ScanError) Error() string {
	return e.message
}

func (e *ScanError) Details() string {
	return e.details
}

func (e *ScanError) Report() {
	fmt.Print(e.details)
}

func (s *Scanner) scanError(ch byte) *ScanError {
	ln := 1
	i := 0
	for ; ln < s.line; i++ {
		if s.src[i] == '\n' {
			ln++
		}
	}

	lineStr := s.src[i:s.pos]

	var sb strings.Builder

	msg := fmt.Sprintf("invalid character %q at line %d and col %d", ch, s.line, s.col)
	fmt.Fprintln(&sb, msg)
	fmt.Fprintf(&sb, "%4s%s\n", " ", lineStr)
	fmt.Fprintf(&sb, "%4s%"+strconv.Itoa(s.pos-i)+"s\n", " ", "^")

	return &ScanError{
		message: msg,
		details: sb.String(),
	}
}
