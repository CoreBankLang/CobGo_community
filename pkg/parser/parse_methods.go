package parser

import "fmt"

// nextToken advances both curToken and peekToken
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

// parseProgram parses the entire program
func (p *Parser) parseProgram() *Program {
	program := &Program{
		Jobs:    []*JobStatement{},
		Records: []*RecordStatement{},
	}

	for p.curToken.Type != EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			if jobStmt, ok := stmt.(*JobStatement); ok {
				program.Jobs = append(program.Jobs, jobStmt)
			} else if recordStmt, ok := stmt.(*RecordStatement); ok {
				program.Records = append(program.Records, recordStmt)
			}
		}
		p.nextToken()
	}

	return program
}

// parseStatement parses a statement
func (p *Parser) parseStatement() Statement {
	switch p.curToken.Type {
	case JOB:
		return p.parseJobStatement()
	case STEP:
		return p.parseStepStatement()
	case RECORD:
		return p.parseRecordStatement()
	case VAR:
		return p.parseVarStatement()
	case IF:
		return p.parseIfStatement()
	case FOR:
		return p.parseForStatement()
	case WHILE:
		return p.parseWhileStatement()
	case RETURN:
		return p.parseReturnStatement()
	case DISPLAY:
		return p.parseDisplayStatement()
	case ACCEPT:
		return p.parseAcceptStatement()
	case PERFORM:
		return p.parsePerformStatement()
	case EVALUATE:
		return p.parseEvaluateStatement()
	case MOVE:
		return p.parseMoveStatement()
	case COMPUTE:
		return p.parseComputeStatement()
	case READ, WRITE, OPEN, CLOSE, REWRITE, DELETE:
		return p.parseFileOperationStatement()
	case COPY:
		return p.parseCopyStatement()
	case INSPECT:
		stmt, err := p.parseInspectStatement()
		if err != nil {
			p.errors = append(p.errors, err.Error())
			return nil
		}
		return stmt
	case STRING_STMT:
		stmt, err := p.parseStringStatement()
		if err != nil {
			p.errors = append(p.errors, err.Error())
			return nil
		}
		return stmt
	case UNSTRING:
		stmt, err := p.parseUnstringStatement()
		if err != nil {
			p.errors = append(p.errors, err.Error())
			return nil
		}
		return stmt
	case SEARCH:
		stmt, err := p.parseSearchStatement()
		if err != nil {
			p.errors = append(p.errors, err.Error())
			return nil
		}
		return stmt
	case LEFT_BRACE:
		return p.parseBlockStatement()
	case LINE_COMMENT, BLOCK_COMMENT:
		// Skip comments
		return nil
	default:
		// Check if this is an assignment statement
		if p.curToken.Type == IDENTIFIER && p.peekToken.Type == ASSIGN {
			return p.parseAssignmentStatement()
		}
		return p.parseExpressionStatement()
	}
}

// parseJobStatement parses a job statement
func (p *Parser) parseJobStatement() *JobStatement {
	stmt := &JobStatement{Token: p.curToken}

	if !p.expectPeek(IDENTIFIER) {
		return nil
	}

	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseStepStatement parses a step statement
func (p *Parser) parseStepStatement() *StepStatement {
	stmt := &StepStatement{Token: p.curToken}

	if !p.expectPeek(IDENTIFIER) {
		return nil
	}

	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if p.peekToken.Type == LEFT_PAREN {
		p.nextToken() // consume the identifier
		p.nextToken() // consume the '('
		stmt.Parameters = p.parseParameterList()
	}

	if p.peekToken.Type == IDENTIFIER {
		p.nextToken()
		stmt.ReturnType = &TypeAnnotation{
			Type:     p.curToken.Type,
			TypeName: p.curToken.Literal,
		}
	}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseRecordStatement parses a record statement
func (p *Parser) parseRecordStatement() *RecordStatement {
	stmt := &RecordStatement{Token: p.curToken}

	if !p.expectPeek(IDENTIFIER) {
		return nil
	}

	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Fields = p.parseFieldList()

	return stmt
}

// parseVarStatement parses a variable declaration
func (p *Parser) parseVarStatement() *VarStatement {
	stmt := &VarStatement{Token: p.curToken}

	if !p.expectPeek(IDENTIFIER) {
		return nil
	}

	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse type annotation
	if p.peekToken.Type == IDENTIFIER || p.peekToken.Type == STRING || p.peekToken.Type == INT32 || p.peekToken.Type == INT64 || p.peekToken.Type == DECIMAL || p.peekToken.Type == DATE || p.peekToken.Type == BOOL {
		p.nextToken()
		stmt.Type = &TypeAnnotation{
			Type:     p.curToken.Type,
			TypeName: p.curToken.Literal,
		}
	}

	// Parse optional assignment
	if p.peekToken.Type == ASSIGN {
		p.nextToken() // consume the '='
		p.nextToken() // move to the value
		stmt.Value = p.parseExpression(LOWEST)
	}

	return stmt
}

// parseIfStatement parses an if statement
func (p *Parser) parseIfStatement() *IfStatement {
	stmt := &IfStatement{Token: p.curToken}

	if !p.expectPeek(LEFT_PAREN) {
		return nil
	}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Consequence = p.parseBlockStatement()

	if p.peekToken.Type == ELSE {
		p.nextToken()
		stmt.Alternative = p.parseStatement()
	}

	return stmt
}

// parseForStatement parses a for statement
func (p *Parser) parseForStatement() *ForStatement {
	stmt := &ForStatement{Token: p.curToken}

	if !p.expectPeek(LEFT_PAREN) {
		return nil
	}

	p.nextToken()
	stmt.Init = p.parseStatement()

	if !p.expectPeek(SEMICOLON) {
		return nil
	}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(SEMICOLON) {
		return nil
	}

	p.nextToken()
	stmt.Update = p.parseStatement()

	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseWhileStatement parses a while statement
func (p *Parser) parseWhileStatement() *WhileStatement {
	stmt := &WhileStatement{Token: p.curToken}

	if !p.expectPeek(LEFT_PAREN) {
		return nil
	}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}

	if !p.expectPeek(LEFT_BRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseReturnStatement parses a return statement
func (p *Parser) parseReturnStatement() *ReturnStatement {
	stmt := &ReturnStatement{Token: p.curToken}

	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)

	return stmt
}

// parseDisplayStatement parses a display statement
func (p *Parser) parseDisplayStatement() *DisplayStatement {
	stmt := &DisplayStatement{Token: p.curToken}

	if !p.expectPeek(LEFT_PAREN) {
		return nil
	}

	// Don't call nextToken() here - expectPeek already advanced the token
	args := p.parseExpressionList(RIGHT_PAREN)
	if args == nil {
		return nil
	}
	stmt.Args = args

	return stmt
}

// parseAcceptStatement parses an accept statement
func (p *Parser) parseAcceptStatement() *AcceptStatement {
	stmt := &AcceptStatement{Token: p.curToken}

	if !p.expectPeek(LEFT_PAREN) {
		return nil
	}

	p.nextToken()
	stmt.Prompt = p.parseExpression(LOWEST)

	if !p.expectPeek(COMMA) {
		return nil
	}

	p.nextToken()
	p.nextToken()
	stmt.Target = &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(RIGHT_PAREN) {
		return nil
	}

	return stmt
}

// parseBlockStatement parses a block statement
func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{Token: p.curToken}
	block.Statements = []Statement{}

	p.nextToken()

	for p.curToken.Type != RIGHT_BRACE && p.curToken.Type != EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

// parseAssignmentStatement parses an assignment statement
func (p *Parser) parseAssignmentStatement() *ExpressionStatement {
	stmt := &ExpressionStatement{Token: p.curToken}

	// Parse the left side (identifier)
	left := &Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Consume the ASSIGN token
	p.nextToken()

	// Parse the right side (expression)
	p.nextToken()
	right := p.parseExpression(LOWEST)

	// Create an infix expression for the assignment
	stmt.Expression = &InfixExpression{
		Token:    Token{Type: ASSIGN, Literal: "="},
		Left:     left,
		Operator: "=",
		Right:    right,
	}

	return stmt
}

// parseExpressionStatement parses an expression statement
func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	stmt := &ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekToken.Type == SEMICOLON {
		p.nextToken()
	}

	return stmt
}

// Helper functions
func (p *Parser) expectPeek(t TokenType) bool {
	if p.peekToken.Type == t {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) peekError(t TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) noPrefixParseFnError(t TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// parsePerformStatement parses a PERFORM statement
func (p *Parser) parsePerformStatement() *PerformStatement {
	stmt := &PerformStatement{Token: p.curToken}

	p.nextToken()

	// Parse target (step name)
	if p.curToken.Type == IDENTIFIER {
		stmt.Target = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		p.nextToken()
	}

	// Parse UNTIL condition
	if p.curToken.Type == UNTIL {
		p.nextToken()
		stmt.Condition = p.parseExpression(LOWEST)
	}

	// Parse VARYING clause
	if p.curToken.Type == VARYING {
		p.nextToken()
		if p.curToken.Type == IDENTIFIER {
			stmt.Varying = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
			p.nextToken()
		}

		// Parse FROM
		if p.curToken.Type == FROM {
			p.nextToken()
			stmt.From = p.parseExpression(LOWEST)
		}

		// Parse TO
		if p.curToken.Type == TO {
			p.nextToken()
			stmt.To = p.parseExpression(LOWEST)
		}

		// Parse BY
		if p.curToken.Type == BY {
			p.nextToken()
			stmt.By = p.parseExpression(LOWEST)
		}
	}

	// Parse TIMES
	if p.curToken.Type == TIMES {
		p.nextToken()
		stmt.Times = p.parseExpression(LOWEST)
	}

	// Parse body if present
	if p.curToken.Type == LEFT_BRACE {
		stmt.Body = p.parseBlockStatement()
	}

	return stmt
}

// parseEvaluateStatement parses an EVALUATE statement
func (p *Parser) parseEvaluateStatement() *EvaluateStatement {
	stmt := &EvaluateStatement{Token: p.curToken}

	p.nextToken()
	stmt.Expression = p.parseExpression(LOWEST)

	// Parse WHEN clauses
	for p.curToken.Type == WHEN {
		whenClause := &WhenClause{Token: p.curToken}
		p.nextToken()
		whenClause.Value = p.parseExpression(LOWEST)

		if p.curToken.Type == LEFT_BRACE {
			whenClause.Body = p.parseBlockStatement()
		}

		stmt.Cases = append(stmt.Cases, whenClause)
	}

	// Parse DEFAULT clause
	if p.curToken.Type == DEFAULT {
		p.nextToken()
		if p.curToken.Type == LEFT_BRACE {
			stmt.Default = p.parseBlockStatement()
		}
	}

	return stmt
}

// parseMoveStatement parses a MOVE statement
func (p *Parser) parseMoveStatement() *MoveStatement {
	stmt := &MoveStatement{Token: p.curToken}

	p.nextToken()
	stmt.From = p.parseExpression(LOWEST)

	if !p.expectPeek(TO) {
		return nil
	}

	p.nextToken()
	stmt.To = p.parseExpression(LOWEST)

	return stmt
}

// parseComputeStatement parses a COMPUTE statement
func (p *Parser) parseComputeStatement() *ComputeStatement {
	stmt := &ComputeStatement{Token: p.curToken}

	p.nextToken()
	stmt.Target = p.parseExpression(LOWEST)

	if !p.expectPeek(ASSIGN) {
		return nil
	}

	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)

	return stmt
}

// parseFileOperationStatement parses file operation statements
func (p *Parser) parseFileOperationStatement() *FileOperationStatement {
	stmt := &FileOperationStatement{
		Token:     p.curToken,
		Operation: p.curToken.Literal,
	}

	p.nextToken()

	// Parse file name
	if p.curToken.Type == IDENTIFIER {
		stmt.File = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		p.nextToken()
	}

	// Parse record name
	if p.curToken.Type == IDENTIFIER {
		stmt.Record = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		p.nextToken()
	}

	// Parse INTO clause
	if p.curToken.Type == INTO {
		p.nextToken()
		if p.curToken.Type == IDENTIFIER {
			stmt.Into = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
			p.nextToken()
		}
	}

	// Parse FROM clause
	if p.curToken.Type == FROM {
		p.nextToken()
		if p.curToken.Type == IDENTIFIER {
			stmt.From = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
			p.nextToken()
		}
	}

	// Parse AT clause
	if p.curToken.Type == AT {
		p.nextToken()
		stmt.At = p.parseExpression(LOWEST)
	}

	// Parse INVALID clause
	if p.curToken.Type == INVALID {
		p.nextToken()
		if p.curToken.Type == LEFT_BRACE {
			stmt.Invalid = p.parseBlockStatement()
		}
	}

	// Parse END clause
	if p.curToken.Type == END {
		p.nextToken()
		if p.curToken.Type == LEFT_BRACE {
			stmt.End = p.parseBlockStatement()
		}
	}

	return stmt
}

// parseCopyStatement parses a COPY statement
func (p *Parser) parseCopyStatement() *CopyStatement {
	stmt := &CopyStatement{Token: p.curToken}

	p.nextToken()
	if p.curToken.Type == STRING_LITERAL {
		stmt.File = &StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
	}

	return stmt
}
