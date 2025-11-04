package parser

import (
	"fmt"
)

// parseInspectStatement parses COBOL INSPECT statement
// INSPECT identifier
//
//	TALLYING counter FOR ALL|LEADING|CHARACTERS pattern [BEFORE|AFTER INITIAL delimiter]
//	REPLACING ALL|LEADING|FIRST pattern BY replacement [BEFORE|AFTER INITIAL delimiter]
//	CONVERTING pattern TO replacement [BEFORE|AFTER INITIAL delimiter]
func (p *Parser) parseInspectStatement() (Statement, error) {
	inspectToken := p.curToken // Save the INSPECT token

	// Parse the identifier to inspect
	if !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected identifier after INSPECT")
	}
	target := &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Determine the operation type
	if p.peekToken.Type != TALLYING && p.peekToken.Type != REPLACING && p.peekToken.Type != CONVERTING {
		return nil, fmt.Errorf("expected TALLYING, REPLACING, or CONVERTING after INSPECT identifier")
	}

	p.nextToken()
	operation := p.curToken.Type

	switch operation {
	case TALLYING:
		return p.parseInspectTallying(inspectToken, target)
	case REPLACING:
		return p.parseInspectReplacing(inspectToken, target)
	case CONVERTING:
		return p.parseInspectConverting(inspectToken, target)
	default:
		return nil, fmt.Errorf("unexpected INSPECT operation: %s", operation)
	}
}

// parseInspectTallying parses INSPECT...TALLYING
func (p *Parser) parseInspectTallying(inspectToken Token, target *Identifier) (Statement, error) {
	// TALLYING already consumed

	// Parse counter identifier
	if !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected counter identifier after TALLYING")
	}
	counter := &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Expect FOR
	if !p.expectPeek(FOR) {
		return nil, fmt.Errorf("expected FOR after TALLYING counter")
	}

	// Parse mode: ALL, LEADING, or CHARACTERS
	if !p.expectPeek(ALL) && !p.expectPeek(LEADING) && !p.expectPeek(CHARACTERS) {
		return nil, fmt.Errorf("expected ALL, LEADING, or CHARACTERS after FOR")
	}
	mode := string(p.curToken.Type)

	// Parse pattern (if not CHARACTERS)
	var pattern Expression
	if p.curToken.Type != CHARACTERS {
		if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected pattern after %s", mode)
		}
		if p.curToken.Type == STRING_LITERAL {
			pattern = &StringLiteral{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		} else {
			pattern = &Identifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		}
	}

	// Parse optional BEFORE/AFTER INITIAL clause
	var beforeAfter *InspectBeforeAfterAST
	if p.peekToken.Type == BEFORE || p.peekToken.Type == AFTER {
		p.nextToken()
		baType := p.curToken.Literal

		if !p.expectPeek(INITIAL) {
			return nil, fmt.Errorf("expected INITIAL after %s", baType)
		}

		if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected delimiter after INITIAL")
		}

		var delimiter Expression
		if p.curToken.Type == STRING_LITERAL {
			delimiter = &StringLiteral{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		} else {
			delimiter = &Identifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		}

		beforeAfter = &InspectBeforeAfterAST{
			Type:      baType,
			Delimiter: delimiter,
		}
	}

	return &InspectStatementAST{
		Token:       inspectToken,
		Target:      target,
		Operation:   "TALLYING",
		Mode:        mode,
		Pattern:     pattern,
		Counter:     counter,
		BeforeAfter: beforeAfter,
		Line:        inspectToken.Line,
		Column:      inspectToken.Column,
	}, nil
}

// parseInspectReplacing parses INSPECT...REPLACING
func (p *Parser) parseInspectReplacing(inspectToken Token, target *Identifier) (Statement, error) {
	// REPLACING already consumed

	// Parse mode: ALL, LEADING, or FIRST
	if !p.expectPeek(ALL) && !p.expectPeek(LEADING) && !p.expectPeek(FIRST) {
		return nil, fmt.Errorf("expected ALL, LEADING, or FIRST after REPLACING")
	}
	mode := string(p.curToken.Type)

	// Parse pattern
	if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected pattern after %s", mode)
	}

	var pattern Expression
	if p.curToken.Type == STRING_LITERAL {
		pattern = &StringLiteral{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	} else {
		pattern = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Expect BY
	if !p.expectPeek(BY) {
		return nil, fmt.Errorf("expected BY after pattern")
	}

	// Parse replacement
	if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected replacement after BY")
	}

	var replacement Expression
	if p.curToken.Type == STRING_LITERAL {
		replacement = &StringLiteral{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	} else {
		replacement = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Parse optional BEFORE/AFTER INITIAL clause
	var beforeAfter *InspectBeforeAfterAST
	if p.peekToken.Type == BEFORE || p.peekToken.Type == AFTER {
		p.nextToken()
		baType := p.curToken.Literal

		if !p.expectPeek(INITIAL) {
			return nil, fmt.Errorf("expected INITIAL after %s", baType)
		}

		if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected delimiter after INITIAL")
		}

		var delimiter Expression
		if p.curToken.Type == STRING_LITERAL {
			delimiter = &StringLiteral{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		} else {
			delimiter = &Identifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		}

		beforeAfter = &InspectBeforeAfterAST{
			Type:      baType,
			Delimiter: delimiter,
		}
	}

	return &InspectStatementAST{
		Token:       inspectToken,
		Target:      target,
		Operation:   "REPLACING",
		Mode:        mode,
		Pattern:     pattern,
		Replacement: replacement,
		BeforeAfter: beforeAfter,
		Line:        inspectToken.Line,
		Column:      inspectToken.Column,
	}, nil
}

// parseInspectConverting parses INSPECT...CONVERTING
func (p *Parser) parseInspectConverting(inspectToken Token, target *Identifier) (Statement, error) {
	// CONVERTING already consumed

	// Parse source pattern
	if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected source pattern after CONVERTING")
	}

	var pattern Expression
	if p.curToken.Type == STRING_LITERAL {
		pattern = &StringLiteral{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	} else {
		pattern = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Expect TO
	if !p.expectPeek(TO) {
		return nil, fmt.Errorf("expected TO after source pattern")
	}

	// Parse target pattern
	if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
		return nil, fmt.Errorf("expected target pattern after TO")
	}

	var replacement Expression
	if p.curToken.Type == STRING_LITERAL {
		replacement = &StringLiteral{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	} else {
		replacement = &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
	}

	// Parse optional BEFORE/AFTER INITIAL clause
	var beforeAfter *InspectBeforeAfterAST
	if p.peekToken.Type == BEFORE || p.peekToken.Type == AFTER {
		p.nextToken()
		baType := p.curToken.Literal

		if !p.expectPeek(INITIAL) {
			return nil, fmt.Errorf("expected INITIAL after %s", baType)
		}

		if !p.expectPeek(STRING_LITERAL) && !p.expectPeek(IDENTIFIER) {
			return nil, fmt.Errorf("expected delimiter after INITIAL")
		}

		var delimiter Expression
		if p.curToken.Type == STRING_LITERAL {
			delimiter = &StringLiteral{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		} else {
			delimiter = &Identifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			}
		}

		beforeAfter = &InspectBeforeAfterAST{
			Type:      baType,
			Delimiter: delimiter,
		}
	}

	return &InspectStatementAST{
		Token:       inspectToken,
		Target:      target,
		Operation:   "CONVERTING",
		Pattern:     pattern,
		Replacement: replacement,
		BeforeAfter: beforeAfter,
		Line:        inspectToken.Line,
		Column:      inspectToken.Column,
	}, nil
}
