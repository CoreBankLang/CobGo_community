package parser

import (
	"fmt"
)

// parseSearchStatement parses COBOL SEARCH statement
// SEARCH identifier
//
//	VARYING index-name
//	AT END imperative-statement
//	WHEN condition imperative-statement
//	...
//
// SEARCH ALL identifier
//
//	AT END imperative-statement
//	WHEN key-condition imperative-statement
func (p *Parser) parseSearchStatement() (Statement, error) {
	searchToken := p.curToken // Save the SEARCH token

	// Check for SEARCH ALL (binary search)
	isSearchAll := false
	if p.peekToken.Type == ALL {
		p.nextToken() // consume ALL
		isSearchAll = true
	}

	// Parse table identifier
	if !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected table identifier after SEARCH")
	}
	tableName := &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	stmt := newSearchAST(searchToken, tableName, isSearchAll)

	// Optional VARYING clause (for linear SEARCH only)
	if !isSearchAll && p.peekToken.Type == VARYING {
		p.nextToken() // consume VARYING
		if !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected index variable after VARYING")
		}
		stmt.Varying = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Optional AT END clause
	if p.peekToken.Type == AT {
		p.nextToken() // consume AT
		if !p.expectPeek(END) {
			return nil, fmt.Errorf("expected END after AT")
		}

		// Parse AT END handler - for now, create an empty block
		stmt.AtEnd = &BlockStatement{
			Token:      p.curToken,
			Statements: []Statement{},
		}

		// Skip to next WHEN or end of statement
		for p.peekToken.Type != WHEN && p.peekToken.Type != EOF && p.peekToken.Type != SEMICOLON {
			p.nextToken()
		}
	}

	// Parse WHEN clauses (at least one required)
	if p.peekToken.Type != WHEN {
		return nil, fmt.Errorf("expected at least one WHEN clause in SEARCH statement")
	}

	for p.peekToken.Type == WHEN {
		p.nextToken() // consume WHEN

		// Parse condition - for now, we'll create a simple placeholder
		// A full implementation would parse the condition expression
		conditionToken := p.curToken

		// Skip condition tokens until we find the body or another WHEN
		for p.peekToken.Type != WHEN && p.peekToken.Type != EOF && p.peekToken.Type != SEMICOLON && p.peekToken.Type != END {
			p.nextToken()
		}

		whenClause := &SearchWhenAST{
			Condition: &Identifier{
				Token: conditionToken,
				Value: conditionToken.Literal,
			},
			Body: &BlockStatement{
				Token:      conditionToken,
				Statements: []Statement{},
			},
		}

		stmt.WhenClauses = append(stmt.WhenClauses, whenClause)

		// Check for another WHEN clause
		if p.peekToken.Type != WHEN {
			break
		}
	}

	return stmt, nil
}
