package parser

import (
	"fmt"
	"strconv"
)

// Precedence constants
const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // !X or -X
	CALL        // myFunction(X)
	INDEX       // array[index]
)

// Precedence map
var precedences = map[TokenType]int{
	EQUAL:         EQUALS,
	NOT_EQUAL:     EQUALS,
	LESS:          LESSGREATER,
	GREATER:       LESSGREATER,
	LESS_EQUAL:    LESSGREATER,
	GREATER_EQUAL: LESSGREATER,
	PLUS:          SUM,
	MINUS:         SUM,
	MULTIPLY:      PRODUCT,
	DIVIDE:        PRODUCT,
	MODULO:        PRODUCT,
	ASSIGN:        LOWEST,
	LEFT_PAREN:    CALL,
	LEFT_BRACKET:  INDEX,
}

// parseExpression parses an expression with the given precedence
func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for p.peekToken.Type != SEMICOLON && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

// Helper functions for parsing expressions
func (p *Parser) parseIdentifier() Expression {
	return &Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteral{Token: p.curToken}
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseDecimalLiteral() Expression {
	lit := &DecimalLiteral{Token: p.curToken}
	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as decimal", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBooleanLiteral() Expression {
	return &BooleanLiteral{Token: p.curToken, Value: p.curToken.Literal == "true"}
}

func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseArrayLiteral() Expression {
	array := &ArrayLiteral{Token: p.curToken}
	array.Elements = p.parseExpressionList(RIGHT_BRACKET)
	return array
}

func (p *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

func (p *Parser) parseInfixExpression(left Expression) Expression {
	expression := &InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parseCallExpression(function Expression) Expression {
	exp := &CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(RIGHT_PAREN)
	return exp
}

func (p *Parser) parseIndexExpression(left Expression) Expression {
	exp := &IndexExpression{Token: p.curToken, Left: left}
	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)
	if !p.expectPeek(RIGHT_BRACKET) {
		return nil
	}
	return exp
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) parseExpressionList(end TokenType) []Expression {
	list := []Expression{}

	if p.peekToken.Type == end {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekToken.Type == COMMA {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseParameterList() []*Identifier {
	identifiers := []*Identifier{}

	if p.peekToken.Type == RIGHT_PAREN {
		p.nextToken()
		return identifiers
	}

	p.nextToken()
	ident := &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekToken.Type == COMMA {
		p.nextToken()
		p.nextToken()
		ident := &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}

	return identifiers
}

func (p *Parser) parseFieldList() []*FieldDeclaration {
	fields := []*FieldDeclaration{}

	for p.curToken.Type != RIGHT_BRACE && p.curToken.Type != EOF {
		field := &FieldDeclaration{}

		if p.curToken.Type == IDENTIFIER {
			field.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		} else {
			p.nextToken()
			continue
		}

		p.nextToken()
		if p.curToken.Type == IDENTIFIER || p.curToken.Type == STRING || p.curToken.Type == INT32 || p.curToken.Type == INT64 || p.curToken.Type == DECIMAL || p.curToken.Type == DATE || p.curToken.Type == BOOL {
			field.Type = &TypeAnnotation{
				Type:     p.curToken.Type,
				TypeName: p.curToken.Literal,
			}
		}

		fields = append(fields, field)
		p.nextToken()
	}

	return fields
}
