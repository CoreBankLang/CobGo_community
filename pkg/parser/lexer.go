package parser

import (
	"unicode"
)

// Lexer represents the lexical analyzer
type Lexer struct {
	input        string
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
	line         int  // current line number
	column       int  // current column number
}

// NewLexer creates a new lexer
func NewLexer(input string) *Lexer {
	l := &Lexer{
		input:  input,
		line:   1,
		column: 1,
	}
	l.readChar()
	return l
}

// readChar reads the next character and advances the position
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII NUL character, represents EOF
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++

	if l.ch == '\n' {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
}

// peekChar returns the next character without advancing the position
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// NextToken scans and returns the next token
func (l *Lexer) NextToken() Token {
	var tok Token

	l.skipWhitespace()

	tok.Line = l.line
	tok.Column = l.column

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: EQUAL, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(PLUS, l.ch)
	case '-':
		tok = newToken(MINUS, l.ch)
	case '*':
		tok = newToken(MULTIPLY, l.ch)
	case '/':
		if l.peekChar() == '/' {
			tok.Type = LINE_COMMENT
			tok.Literal = l.readLineComment()
		} else if l.peekChar() == '*' {
			tok.Type = BLOCK_COMMENT
			tok.Literal = l.readBlockComment()
		} else {
			tok = newToken(DIVIDE, l.ch)
		}
	case '%':
		tok = newToken(MODULO, l.ch)
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: NOT_EQUAL, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(NOT, l.ch)
		}
	case '<':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: LESS_EQUAL, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(LESS, l.ch)
		}
	case '>':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: GREATER_EQUAL, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(GREATER, l.ch)
		}
	case '&':
		if l.peekChar() == '&' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: AND, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(ILLEGAL, l.ch)
		}
	case '|':
		if l.peekChar() == '|' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: OR, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = newToken(ILLEGAL, l.ch)
		}
	case '(':
		tok = newToken(LEFT_PAREN, l.ch)
	case ')':
		tok = newToken(RIGHT_PAREN, l.ch)
	case '{':
		tok = newToken(LEFT_BRACE, l.ch)
	case '}':
		tok = newToken(RIGHT_BRACE, l.ch)
	case '[':
		tok = newToken(LEFT_BRACKET, l.ch)
	case ']':
		tok = newToken(RIGHT_BRACKET, l.ch)
	case ',':
		tok = newToken(COMMA, l.ch)
	case ';':
		tok = newToken(SEMICOLON, l.ch)
	case ':':
		tok = newToken(COLON, l.ch)
	case '.':
		tok = newToken(DOT, l.ch)
	case '"':
		tok.Type = STRING_LITERAL
		tok.Literal = l.readString()
	case 0:
		tok.Literal = ""
		tok.Type = EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = LookupIdentifier(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			tok.Type, tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

// newToken creates a new token
func newToken(tokenType TokenType, ch byte) Token {
	return Token{Type: tokenType, Literal: string(ch)}
}

// readIdentifier reads an identifier
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '_' {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readNumber reads a number (integer or decimal)
func (l *Lexer) readNumber() (TokenType, string) {
	position := l.position
	tokenType := NUMBER_LITERAL

	for isDigit(l.ch) {
		l.readChar()
	}

	// Check for decimal point
	if l.ch == '.' && isDigit(l.peekChar()) {
		tokenType = DECIMAL_LITERAL
		l.readChar() // consume the '.'
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	return tokenType, l.input[position:l.position]
}

// readString reads a string literal
func (l *Lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

// readLineComment reads a line comment
func (l *Lexer) readLineComment() string {
	position := l.position
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readBlockComment reads a block comment
func (l *Lexer) readBlockComment() string {
	position := l.position
	for {
		l.readChar()
		if l.ch == '*' && l.peekChar() == '/' {
			l.readChar() // consume the '*'
			l.readChar() // consume the '/'
			break
		}
		if l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

// skipWhitespace skips whitespace characters
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r' || l.ch == '\n' {
		l.readChar()
	}
}

// isLetter checks if the character is a letter
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// isDigit checks if the character is a digit
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// isLetterUnicode checks if the rune is a letter (for Unicode support)
func isLetterUnicode(r rune) bool {
	return unicode.IsLetter(r)
}

// isDigitUnicode checks if the rune is a digit (for Unicode support)
func isDigitUnicode(r rune) bool {
	return unicode.IsDigit(r)
}
