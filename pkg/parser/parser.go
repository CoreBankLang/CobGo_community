package parser

import (
	"fmt"
	"io"
	"io/ioutil"
)

// Parser represents a CobGO DSL parser
type Parser struct {
	lexer          *Lexer
	curToken       Token
	peekToken      Token
	errors         []string
	prefixParseFns map[TokenType]func() Expression
	infixParseFns  map[TokenType]func(Expression) Expression
}

// New creates a new CobGO parser
func New() *Parser {
	p := &Parser{
		errors: []string{},
	}

	// Initialize prefix parse functions
	p.prefixParseFns = make(map[TokenType]func() Expression)
	p.prefixParseFns[IDENTIFIER] = p.parseIdentifier
	p.prefixParseFns[NUMBER_LITERAL] = p.parseIntegerLiteral
	p.prefixParseFns[DECIMAL_LITERAL] = p.parseDecimalLiteral
	p.prefixParseFns[STRING_LITERAL] = p.parseStringLiteral
	p.prefixParseFns[BOOLEAN_LITERAL] = p.parseBooleanLiteral
	p.prefixParseFns[LEFT_PAREN] = p.parseGroupedExpression
	p.prefixParseFns[LEFT_BRACKET] = p.parseArrayLiteral
	p.prefixParseFns[MINUS] = p.parsePrefixExpression
	p.prefixParseFns[NOT] = p.parsePrefixExpression
	p.prefixParseFns[INT32] = p.parseIdentifier
	p.prefixParseFns[INT64] = p.parseIdentifier
	p.prefixParseFns[STRING] = p.parseIdentifier
	p.prefixParseFns[DECIMAL] = p.parseIdentifier
	p.prefixParseFns[DATE] = p.parseIdentifier
	p.prefixParseFns[BOOL] = p.parseIdentifier

	// Initialize infix parse functions
	p.infixParseFns = make(map[TokenType]func(Expression) Expression)
	p.infixParseFns[PLUS] = p.parseInfixExpression
	p.infixParseFns[MINUS] = p.parseInfixExpression
	p.infixParseFns[MULTIPLY] = p.parseInfixExpression
	p.infixParseFns[DIVIDE] = p.parseInfixExpression
	p.infixParseFns[MODULO] = p.parseInfixExpression
	p.infixParseFns[EQUAL] = p.parseInfixExpression
	p.infixParseFns[NOT_EQUAL] = p.parseInfixExpression
	p.infixParseFns[LESS] = p.parseInfixExpression
	p.infixParseFns[GREATER] = p.parseInfixExpression
	p.infixParseFns[LESS_EQUAL] = p.parseInfixExpression
	p.infixParseFns[GREATER_EQUAL] = p.parseInfixExpression
	p.infixParseFns[AND] = p.parseInfixExpression
	p.infixParseFns[OR] = p.parseInfixExpression
	p.infixParseFns[ASSIGN] = p.parseInfixExpression
	p.infixParseFns[LEFT_PAREN] = p.parseCallExpression
	p.infixParseFns[LEFT_BRACKET] = p.parseIndexExpression

	return p
}

// Parse parses CobGO source code and returns an AST
func (p *Parser) Parse(reader io.Reader) (*Program, error) {
	// Read the input
	input, err := ioutil.ReadAll(reader)
	if err != nil {
		return nil, fmt.Errorf("failed to read input: %w", err)
	}

	// Create lexer and parse
	p.lexer = NewLexer(string(input))
	p.nextToken()
	p.nextToken()

	program := p.parseProgram()

	if len(p.errors) > 0 {
		return program, fmt.Errorf("parsing errors: %v", p.errors)
	}

	return program, nil
}
