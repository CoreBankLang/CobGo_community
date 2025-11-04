package cobolparser

import (
	"fmt"
	"strings"
)

// Token represents a lexical token in COBOL
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

// TokenType represents the type of a COBOL token
type TokenType int

const (
	// Special tokens
	ILLEGAL TokenType = iota
	EOF
	COMMENT

	// Literals
	IDENT  // CUSTOMER-ID, WS-TOTAL
	NUMBER // 123, 123.45
	STRING // "hello world", 'hello world'

	// Keywords - Division headers
	IDENTIFICATION
	DIVISION
	ENVIRONMENT
	DATA
	PROCEDURE

	// Keywords - Section headers
	SECTION
	CONFIGURATION
	INPUT_OUTPUT
	FILE_CONTROL
	FILE_SECTION
	WORKING_STORAGE
	LOCAL_STORAGE
	LINKAGE

	// Keywords - Statements
	PROGRAM_ID
	AUTHOR
	DATE_WRITTEN
	SELECT
	ASSIGN
	ORGANIZATION
	ACCESS
	FILE_STATUS
	FD
	RECORD
	BLOCK
	CONTAINS
	LABEL
	STANDARD
	OMITTED
	RECORDS
	CHARACTERS
	PIC
	PICTURE
	VALUE
	OCCURS
	REDEFINES
	PERFORM
	UNTIL
	VARYING
	THRU
	THROUGH
	MOVE
	TO
	FROM
	COMPUTE
	ADD
	SUBTRACT
	MULTIPLY
	DIVIDE
	BY
	GIVING
	ROUNDED
	IF
	THEN
	ELSE
	END_IF
	EVALUATE
	WHEN
	END_EVALUATE
	DISPLAY
	ACCEPT
	OPEN
	CLOSE
	READ
	WRITE
	REWRITE
	DELETE
	START
	STOP
	RUN
	EXIT
	GO
	GOBACK
	CALL
	USING
	RETURNING
	INPUT
	OUTPUT
	IO
	EXTEND
	AT
	END
	NOT
	INVALID
	KEY
	INTO
	SEQUENTIAL
	INDEXED
	RELATIVE
	DYNAMIC
	RANDOM
	MODE
	IS
	ARE
	UPON

	// Operators and delimiters
	PERIOD     // .
	COMMA      // ,
	LPAREN     // (
	RPAREN     // )
	PLUS       // +
	MINUS      // -
	ASTERISK   // *
	SLASH      // /
	EQUAL      // =
	GREATER    // >
	LESS       // <
	NOT_EQUAL  // <>
	GREATER_EQ // >=
	LESS_EQ    // <=
	AND
	OR
)

var keywords = map[string]TokenType{
	"IDENTIFICATION":  IDENTIFICATION,
	"DIVISION":        DIVISION,
	"ENVIRONMENT":     ENVIRONMENT,
	"DATA":            DATA,
	"PROCEDURE":       PROCEDURE,
	"SECTION":         SECTION,
	"CONFIGURATION":   CONFIGURATION,
	"INPUT-OUTPUT":    INPUT_OUTPUT,
	"FILE-CONTROL":    FILE_CONTROL,
	"FILE":            FILE_SECTION,
	"WORKING-STORAGE": WORKING_STORAGE,
	"LOCAL-STORAGE":   LOCAL_STORAGE,
	"LINKAGE":         LINKAGE,
	"PROGRAM-ID":      PROGRAM_ID,
	"AUTHOR":          AUTHOR,
	"DATE-WRITTEN":    DATE_WRITTEN,
	"SELECT":          SELECT,
	"ASSIGN":          ASSIGN,
	"ORGANIZATION":    ORGANIZATION,
	"ACCESS":          ACCESS,
	"FILE-STATUS":     FILE_STATUS,
	"FD":              FD,
	"RECORD":          RECORD,
	"RECORDS":         RECORDS,
	"BLOCK":           BLOCK,
	"CONTAINS":        CONTAINS,
	"LABEL":           LABEL,
	"STANDARD":        STANDARD,
	"OMITTED":         OMITTED,
	"CHARACTERS":      CHARACTERS,
	"PIC":             PIC,
	"PICTURE":         PICTURE,
	"VALUE":           VALUE,
	"OCCURS":          OCCURS,
	"REDEFINES":       REDEFINES,
	"PERFORM":         PERFORM,
	"UNTIL":           UNTIL,
	"VARYING":         VARYING,
	"THRU":            THRU,
	"THROUGH":         THROUGH,
	"MOVE":            MOVE,
	"TO":              TO,
	"FROM":            FROM,
	"COMPUTE":         COMPUTE,
	"ADD":             ADD,
	"SUBTRACT":        SUBTRACT,
	"MULTIPLY":        MULTIPLY,
	"DIVIDE":          DIVIDE,
	"BY":              BY,
	"GIVING":          GIVING,
	"ROUNDED":         ROUNDED,
	"IF":              IF,
	"THEN":            THEN,
	"ELSE":            ELSE,
	"END-IF":          END_IF,
	"EVALUATE":        EVALUATE,
	"WHEN":            WHEN,
	"END-EVALUATE":    END_EVALUATE,
	"DISPLAY":         DISPLAY,
	"ACCEPT":          ACCEPT,
	"OPEN":            OPEN,
	"CLOSE":           CLOSE,
	"READ":            READ,
	"WRITE":           WRITE,
	"REWRITE":         REWRITE,
	"DELETE":          DELETE,
	"START":           START,
	"STOP":            STOP,
	"RUN":             RUN,
	"EXIT":            EXIT,
	"GO":              GO,
	"GOBACK":          GOBACK,
	"CALL":            CALL,
	"USING":           USING,
	"RETURNING":       RETURNING,
	"INPUT":           INPUT,
	"OUTPUT":          OUTPUT,
	"I-O":             IO,
	"EXTEND":          EXTEND,
	"AT":              AT,
	"END":             END,
	"NOT":             NOT,
	"INVALID":         INVALID,
	"KEY":             KEY,
	"INTO":            INTO,
	"SEQUENTIAL":      SEQUENTIAL,
	"INDEXED":         INDEXED,
	"RELATIVE":        RELATIVE,
	"DYNAMIC":         DYNAMIC,
	"RANDOM":          RANDOM,
	"MODE":            MODE,
	"IS":              IS,
	"ARE":             ARE,
	"UPON":            UPON,
	"AND":             AND,
	"OR":              OR,
}

// Lexer performs lexical analysis on COBOL source code
type Lexer struct {
	input          string
	position       int
	readPosition   int
	ch             byte
	line           int
	column         int
	fixedFormat    bool // true if file uses fixed format with sequence numbers
	detectedFormat bool // true if we've already detected the format
}

// NewLexer creates a new lexer for COBOL source code
func NewLexer(input string) *Lexer {
	l := &Lexer{
		input:          input,
		line:           1,
		column:         0,
		fixedFormat:    false,
		detectedFormat: false,
	}
	l.readChar()
	return l
}

// NextToken returns the next token from the input
func (l *Lexer) NextToken() Token {
	var tok Token

	l.skipWhitespace()

	tok.Line = l.line
	tok.Column = l.column

	switch l.ch {
	case 0:
		tok.Type = EOF
		tok.Literal = ""
	case '.':
		tok = Token{Type: PERIOD, Literal: string(l.ch)}
	case ',':
		tok = Token{Type: COMMA, Literal: string(l.ch)}
	case '(':
		tok = Token{Type: LPAREN, Literal: string(l.ch)}
	case ')':
		tok = Token{Type: RPAREN, Literal: string(l.ch)}
	case '+':
		tok = Token{Type: PLUS, Literal: string(l.ch)}
	case '-':
		// Check for continuation line (- in column 7)
		if l.column == 7 {
			// Skip continuation indicator and continue reading
			l.readChar()
			l.skipWhitespace()
			return l.NextToken() // Get next real token
		}
		tok = Token{Type: MINUS, Literal: string(l.ch)}
	case '*':
		// Check for comment (* in column 7)
		if l.column == 7 {
			return l.readComment()
		}
		tok = Token{Type: ASTERISK, Literal: string(l.ch)}
	case '/':
		tok = Token{Type: SLASH, Literal: string(l.ch)}
	case '=':
		tok = Token{Type: EQUAL, Literal: string(l.ch)}
	case '>':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: GREATER_EQ, Literal: string(ch) + string(l.ch)}
		} else {
			tok = Token{Type: GREATER, Literal: string(l.ch)}
		}
	case '<':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: LESS_EQ, Literal: string(ch) + string(l.ch)}
		} else if l.peekChar() == '>' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: NOT_EQUAL, Literal: string(ch) + string(l.ch)}
		} else {
			tok = Token{Type: LESS, Literal: string(l.ch)}
		}
	case '"', '\'':
		tok.Type = STRING
		tok.Literal = l.readString(l.ch)
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = lookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			tok.Type = NUMBER
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = Token{Type: ILLEGAL, Literal: string(l.ch)}
		}
	}

	l.readChar()
	return tok
}

// readChar reads the next character
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
		return
	}

	l.ch = l.input[l.readPosition]
	l.position = l.readPosition
	l.readPosition++
	l.column++

	// Handle newline
	if l.ch == '\n' {
		l.line++
		l.column = 0
		return
	}

	// Auto-detect fixed format on first line
	if !l.detectedFormat && l.line == 1 && l.column >= 1 && l.column <= 6 {
		// Check if columns 1-6 contain only digits (sequence numbers)
		if l.column == 1 {
			l.fixedFormat = isDigit(l.ch) || l.ch == ' '
		} else if l.fixedFormat {
			l.fixedFormat = isDigit(l.ch) || l.ch == ' '
		}
		if l.column == 6 {
			l.detectedFormat = true
		}
	}

	// COBOL fixed format: skip columns 1-6 (sequence area) at start of line
	if l.fixedFormat && l.column >= 1 && l.column <= 6 {
		// Continue reading until we reach column 7
		for l.column < 7 && l.ch != '\n' && l.readPosition < len(l.input) {
			l.ch = l.input[l.readPosition]
			l.position = l.readPosition
			l.readPosition++
			if l.ch != '\n' {
				l.column++
			} else {
				l.line++
				l.column = 0
				return
			}
		}
	}

	// COBOL fixed format: skip columns 73-80 (identification area)
	if l.fixedFormat && l.column > 72 && l.ch != '\n' {
		// Skip to next newline
		for l.ch != '\n' && l.readPosition < len(l.input) {
			l.ch = l.input[l.readPosition]
			l.position = l.readPosition
			l.readPosition++
		}
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
	}
}

// peekChar looks at the next character without advancing
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// readIdentifier reads an identifier or keyword
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '-' || l.ch == '_' {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readNumber reads a numeric literal
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}

	// Check for decimal point
	if l.ch == '.' && isDigit(l.peekChar()) {
		l.readChar() // consume '.'
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	return l.input[position:l.position]
}

// readString reads a string literal
func (l *Lexer) readString(quote byte) string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == quote || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

// readComment reads a comment line
func (l *Lexer) readComment() Token {
	position := l.position
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	return Token{
		Type:    COMMENT,
		Literal: l.input[position:l.position],
		Line:    l.line,
		Column:  l.column,
	}
}

// skipWhitespace skips whitespace characters
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// isLetter checks if a character is a letter
func isLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
}

// isDigit checks if a character is a digit
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// lookupIdent checks if an identifier is a keyword
func lookupIdent(ident string) TokenType {
	// COBOL is case-insensitive
	ident = strings.ToUpper(ident)
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

// TokenTypeString returns a string representation of a token type
func (tt TokenType) String() string {
	names := map[TokenType]string{
		ILLEGAL:        "ILLEGAL",
		EOF:            "EOF",
		COMMENT:        "COMMENT",
		IDENT:          "IDENT",
		NUMBER:         "NUMBER",
		STRING:         "STRING",
		IDENTIFICATION: "IDENTIFICATION",
		DIVISION:       "DIVISION",
		ENVIRONMENT:    "ENVIRONMENT",
		DATA:           "DATA",
		PROCEDURE:      "PROCEDURE",
		SECTION:        "SECTION",
		PROGRAM_ID:     "PROGRAM-ID",
		AUTHOR:         "AUTHOR",
		SELECT:         "SELECT",
		FD:             "FD",
		RECORD:         "RECORD",
		BLOCK:          "BLOCK",
		CONTAINS:       "CONTAINS",
		LABEL:          "LABEL",
		STANDARD:       "STANDARD",
		OMITTED:        "OMITTED",
		PERFORM:        "PERFORM",
		MOVE:           "MOVE",
		DISPLAY:        "DISPLAY",
		PERIOD:         "PERIOD",
		COMMA:          "COMMA",
		LPAREN:         "LPAREN",
		RPAREN:         "RPAREN",
	}
	if name, ok := names[tt]; ok {
		return name
	}
	return fmt.Sprintf("TokenType(%d)", tt)
}

// Format formats a token for display
func (t Token) String() string {
	return fmt.Sprintf("Token{%s, %q, Line:%d, Col:%d}", t.Type.String(), t.Literal, t.Line, t.Column)
}
