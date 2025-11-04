package parser

import (
	"fmt"
)

// parseUnstringStatement parses COBOL UNSTRING statement
// UNSTRING source
//
//	DELIMITED BY delimiter1 OR delimiter2
//	INTO target1, target2, target3
//	WITH POINTER position
//	TALLYING counter
//	ON OVERFLOW imperative-statement
func (p *Parser) parseUnstringStatement() (Statement, error) {
	unstringToken := p.curToken // Save the UNSTRING token

	// Parse source identifier
	if !p.expectPeek(IDENTIFIER) && !p.expectPeek(STRING_LITERAL) {
		return nil, fmt.Errorf("expected source identifier in UNSTRING statement")
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

	stmt := newUnstringAST(unstringToken, source)

	// Expect DELIMITED
	if !p.expectPeek(DELIMITED) {
		return nil, fmt.Errorf("expected DELIMITED in UNSTRING statement")
	}

	// Expect BY
	if !p.expectPeek(BY) {
		return nil, fmt.Errorf("expected BY after DELIMITED")
	}

	// Parse delimiters (can be multiple with OR)
	for {
		if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected delimiter in UNSTRING statement")
		}
		stmt.Delimiters = append(stmt.Delimiters, p.curToken.Literal)

		// Check for OR (additional delimiter)
		if p.peekToken.Type != OR {
			break
		}
		p.nextToken() // consume OR
	}

	// Expect INTO
	if !p.expectPeek(INTO) {
		return nil, fmt.Errorf("expected INTO in UNSTRING statement")
	}

	// Parse target fields (can be multiple, comma-separated)
	for {
		if !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected target identifier in UNSTRING statement")
		}
		stmt.Targets = append(stmt.Targets, &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		})

		// Check for comma (more targets)
		if p.peekToken.Type != COMMA {
			break
		}
		p.nextToken() // consume COMMA
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

	// Optional TALLYING clause
	if p.peekToken.Type == TALLYING {
		p.nextToken() // consume TALLYING
		if !p.expectPeek(IN) || !p.expectPeek(IDENTIFIER) {
			if !p.expectPeek(IDENTIFIER) {
				return nil, fmt.Errorf("expected counter variable after TALLYING")
			}
		}
		stmt.Tallying = &Identifier{
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
		stmt.OnOverflow = &BlockStatement{
			Token:      p.curToken,
			Statements: []Statement{},
		}
	}

	return stmt, nil
}
