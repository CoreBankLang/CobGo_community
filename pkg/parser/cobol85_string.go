package parser

import (
	"fmt"
)

// parseStringStatement parses COBOL STRING statement
// STRING source1 DELIMITED BY delimiter1
//
//	source2 DELIMITED BY delimiter2
//	INTO destination
//	WITH POINTER position
//	ON OVERFLOW imperative-statement
func (p *Parser) parseStringStatement() (Statement, error) {
	stringToken := p.curToken // Save the STRING token

	stmt := newStringAST(stringToken)

	// Parse source fields and delimiters
	for {
		// Parse source identifier or literal
		if !p.expectPeek(IDENTIFIER) && !p.expectPeek(STRING_LITERAL) {
			return nil, fmt.Errorf("expected source identifier or literal in STRING statement")
		}

		var source Expression
		if p.curToken.Type == STRING_LITERAL {
			source = &StringLiteral{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		} else {
			source = &Identifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		}

		// Expect DELIMITED
		if !p.expectPeek(DELIMITED) {
			return nil, fmt.Errorf("expected DELIMITED after source in STRING statement")
		}

		// Expect BY
		if !p.expectPeek(BY) {
			return nil, fmt.Errorf("expected BY after DELIMITED")
		}

		// Parse delimiter (SIZE or literal)
		if !p.expectPeek(SIZE) && !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected SIZE or delimiter after DELIMITED BY")
		}
		delimiter := p.curToken.Literal

		stmt.Sources = append(stmt.Sources, &StringSourceAST{
			Value:     source,
			Delimiter: delimiter,
		})

		// Check if there's another source or if we've reached INTO
		if p.peekToken.Type == INTO {
			break
		}

		// Check for end of sources
		if p.peekToken.Type != IDENTIFIER && p.peekToken.Type != STRING_LITERAL {
			break
		}
	}

	// Expect INTO
	if !p.expectPeek(INTO) {
		return nil, fmt.Errorf("expected INTO in STRING statement")
	}

	// Parse destination
	if !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected destination identifier after INTO")
	}
	stmt.Destination = &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Optional WITH POINTER clause
	if p.peekToken.Type == WITH {
		p.nextToken() // consume WITH
		if !p.expectPeek(POINTER) {
			return nil, fmt.Errorf("expected POINTER after WITH")
		}
		if !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected pointer variable after POINTER")
		}
		stmt.Pointer = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Optional ON OVERFLOW clause
	if p.peekToken.Type == ON {
		p.nextToken() // consume ON
		if !p.expectPeek(OVERFLOW) {
			return nil, fmt.Errorf("expected OVERFLOW after ON")
		}

		// Parse overflow handler - for now, create an empty block
		// In a complete implementation, we'd parse the statements
		stmt.OnOverflow = &BlockStatement{
			Token:      p.curToken,
			Statements: []Statement{},
		}
	}

	return stmt, nil
}
