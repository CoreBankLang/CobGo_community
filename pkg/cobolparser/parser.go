package cobolparser

import (
	"fmt"
	"strconv"
	"strings"
)

// Parser parses COBOL source code into an AST
type Parser struct {
	lexer     *Lexer
	curToken  Token
	peekToken Token
	errors    []string
}

// NewParser creates a new COBOL parser
func NewParser(input string) *Parser {
	p := &Parser{
		lexer:  NewLexer(input),
		errors: []string{},
	}

	// Read two tokens to initialize curToken and peekToken
	p.nextToken()
	p.nextToken()

	return p
}

// Parse parses the COBOL source and returns an AST
func (p *Parser) Parse() (*Program, error) {
	program := &Program{}

	// Parse each division
	for p.curToken.Type != EOF {
		switch p.curToken.Type {
		case IDENTIFICATION:
			program.Identification = p.parseIdentificationDivision()
		case ENVIRONMENT:
			program.Environment = p.parseEnvironmentDivision()
		case DATA:
			program.Data = p.parseDataDivision()
		case PROCEDURE:
			program.Procedure = p.parseProcedureDivision()
		case COMMENT:
			// Skip comments
			p.nextToken()
		default:
			p.nextToken()
		}
	}

	if len(p.errors) > 0 {
		return program, fmt.Errorf("parsing errors: %v", p.errors)
	}

	return program, nil
}

// parseIdentificationDivision parses the IDENTIFICATION DIVISION
func (p *Parser) parseIdentificationDivision() *IdentificationDivision {
	div := &IdentificationDivision{}

	p.expectToken(IDENTIFICATION)
	p.expectToken(DIVISION)
	p.expectToken(PERIOD)

	// Parse PROGRAM-ID
	if p.curTokenIs(PROGRAM_ID) {
		p.nextToken()
		if p.curTokenIs(PERIOD) {
			p.nextToken()
		}
		if p.curTokenIs(IDENT) {
			div.ProgramID = p.curToken.Literal
			p.nextToken()
		}
		p.expectToken(PERIOD)
	}

	// Parse optional AUTHOR, DATE-WRITTEN, etc.
	for !p.curTokenIs(ENVIRONMENT) && !p.curTokenIs(DATA) && !p.curTokenIs(PROCEDURE) && !p.curTokenIs(EOF) {
		if p.curTokenIs(AUTHOR) {
			p.nextToken()
			if p.curTokenIs(PERIOD) {
				p.nextToken()
			}
			if p.curTokenIs(IDENT) || p.curTokenIs(STRING) {
				div.Author = p.curToken.Literal
				p.nextToken()
			}
			if p.curTokenIs(PERIOD) {
				p.nextToken()
			}
		} else if p.curTokenIs(DATE_WRITTEN) {
			p.nextToken()
			if p.curTokenIs(PERIOD) {
				p.nextToken()
			}
			// Skip date value
			p.nextToken()
			if p.curTokenIs(PERIOD) {
				p.nextToken()
			}
		} else {
			p.nextToken()
		}
	}

	return div
}

// parseEnvironmentDivision parses the ENVIRONMENT DIVISION
func (p *Parser) parseEnvironmentDivision() *EnvironmentDivision {
	div := &EnvironmentDivision{}

	p.expectToken(ENVIRONMENT)
	p.expectToken(DIVISION)
	p.expectToken(PERIOD)

	// Parse sections
	for !p.curTokenIs(DATA) && !p.curTokenIs(PROCEDURE) && !p.curTokenIs(EOF) {
		if p.curTokenIs(INPUT_OUTPUT) {
			div.InputOutput = p.parseInputOutputSection()
		} else {
			p.nextToken()
		}
	}

	return div
}

// parseInputOutputSection parses INPUT-OUTPUT SECTION
func (p *Parser) parseInputOutputSection() *InputOutputSection {
	section := &InputOutputSection{
		FileControl: []*FileControlEntry{},
	}

	p.expectToken(INPUT_OUTPUT)
	p.expectToken(SECTION)
	p.expectToken(PERIOD)

	// Parse FILE-CONTROL
	if p.curTokenIs(FILE_CONTROL) {
		p.nextToken()
		if p.curTokenIs(PERIOD) {
			p.nextToken()
		}

		// Parse SELECT statements
		for p.curTokenIs(SELECT) {
			entry := p.parseFileControlEntry()
			section.FileControl = append(section.FileControl, entry)
		}
	}

	return section
}

// parseFileControlEntry parses a SELECT statement
func (p *Parser) parseFileControlEntry() *FileControlEntry {
	entry := &FileControlEntry{}

	p.expectToken(SELECT)
	if p.curTokenIs(IDENT) {
		entry.FileName = p.curToken.Literal
		p.nextToken()
	}

	// Parse clauses in any order until we hit a PERIOD
	for !p.curTokenIs(PERIOD) && !p.curTokenIs(EOF) && !p.curTokenIs(SELECT) {
		switch p.curToken.Type {
		case ASSIGN:
			p.nextToken()
			if p.curTokenIs(TO) {
				p.nextToken()
			}
			if p.curTokenIs(STRING) || p.curTokenIs(IDENT) {
				entry.AssignTo = p.curToken.Literal
				p.nextToken()
			}

		case ORGANIZATION:
			p.nextToken()
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			if p.curTokenIs(SEQUENTIAL) || p.curTokenIs(INDEXED) || p.curTokenIs(RELATIVE) {
				entry.Organization = p.curToken.Literal
				p.nextToken()
			}

		case SEQUENTIAL, INDEXED, RELATIVE:
			// Handle standalone organization keyword
			if entry.Organization == "" {
				entry.Organization = p.curToken.Literal
			}
			p.nextToken()

		case ACCESS:
			p.nextToken()
			if p.curTokenIs(MODE) {
				p.nextToken()
			}
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			if p.curTokenIs(SEQUENTIAL) || p.curTokenIs(RANDOM) || p.curTokenIs(DYNAMIC) {
				entry.AccessMode = p.curToken.Literal
				p.nextToken()
			}

		case FILE_STATUS:
			p.nextToken()
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			if p.curTokenIs(IDENT) {
				entry.FileStatus = p.curToken.Literal
				p.nextToken()
			}

		default:
			// Skip unknown tokens
			p.nextToken()
		}
	}

	// Consume the period
	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return entry
}

// parseDataDivision parses the DATA DIVISION
func (p *Parser) parseDataDivision() *DataDivision {
	div := &DataDivision{
		FileSection:    []*FileDescription{},
		WorkingStorage: []*DataItem{},
	}

	p.expectToken(DATA)
	p.expectToken(DIVISION)
	p.expectToken(PERIOD)

	// Parse sections
	for !p.curTokenIs(EOF) {
		// Check if we're at PROCEDURE DIVISION (not just PROCEDURE as variable name)
		if p.curTokenIs(PROCEDURE) {
			// Look ahead to see if next token is DIVISION
			if p.peekTokenIs(DIVISION) {
				break // Exit loop, we've reached PROCEDURE DIVISION
			}
			// Otherwise, it's just a variable named PROCEDURE, keep parsing
		}

		if p.curTokenIs(FILE_SECTION) {
			p.nextToken()
			p.expectToken(SECTION)
			p.expectToken(PERIOD)
			// Parse FD entries
			for p.curTokenIs(FD) {
				fd := p.parseFileDescription()
				div.FileSection = append(div.FileSection, fd)
			}
		} else if p.curTokenIs(WORKING_STORAGE) {
			p.nextToken()
			p.expectToken(SECTION)
			p.expectToken(PERIOD)
			// Parse data items (77, 01, 05, etc.)
			for p.curTokenIs(NUMBER) {
				item := p.parseDataItem()
				div.WorkingStorage = append(div.WorkingStorage, item)
			}
		} else {
			p.nextToken()
		}
	}

	return div
}

// parseFileDescription parses an FD entry
func (p *Parser) parseFileDescription() *FileDescription {
	fd := &FileDescription{}

	p.expectToken(FD)
	if p.curTokenIs(IDENT) {
		fd.FileName = p.curToken.Literal
		p.nextToken()
	}

	// Parse FD clauses until we hit a PERIOD
	for !p.curTokenIs(PERIOD) && !p.curTokenIs(EOF) && !p.curTokenIs(NUMBER) && !p.curTokenIs(FD) {
		switch p.curToken.Type {
		case BLOCK:
			// BLOCK CONTAINS n CHARACTERS/RECORDS
			p.nextToken()
			if p.curTokenIs(CONTAINS) {
				p.nextToken()
			}
			// Skip the number and unit (CHARACTERS/RECORDS)
			if p.curTokenIs(NUMBER) {
				p.nextToken()
			}
			// Skip CHARACTERS or RECORDS keyword
			p.nextToken()

		case RECORD:
			// RECORD CONTAINS n CHARACTERS
			p.nextToken()
			if p.curTokenIs(CONTAINS) {
				p.nextToken()
			}
			// Skip the number
			if p.curTokenIs(NUMBER) {
				p.nextToken()
			}
			// Skip CHARACTERS keyword if present
			if p.curTokenIs(IDENT) {
				p.nextToken()
			}

		case LABEL:
			// LABEL RECORD/RECORDS IS/ARE STANDARD/OMITTED
			p.nextToken()
			// Skip RECORD/RECORDS
			if p.curTokenIs(RECORD) || p.curTokenIs(IDENT) {
				p.nextToken()
			}
			// Skip IS/ARE
			if p.curTokenIs(IS) || p.curTokenIs(IDENT) {
				p.nextToken()
			}
			// Skip STANDARD/OMITTED
			if p.curTokenIs(IDENT) {
				p.nextToken()
			}

		case DATA:
			// DATA RECORD IS record-name
			p.nextToken()
			if p.curTokenIs(RECORD) {
				p.nextToken()
			}
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			// Skip record name
			if p.curTokenIs(IDENT) {
				p.nextToken()
			}

		default:
			// Skip unknown tokens
			p.nextToken()
		}
	}

	// Consume the period
	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	// Parse record description (01 level)
	if p.curTokenIs(NUMBER) {
		item := p.parseDataItem()
		fd.RecordName = item.Name
		fd.RecordItems = item.Children
	}

	return fd
}

// parseDataItem parses a data item (01, 05, etc.)
func (p *Parser) parseDataItem() *DataItem {
	item := &DataItem{}

	// Parse level number
	if p.curTokenIs(NUMBER) {
		level, _ := strconv.Atoi(p.curToken.Literal)
		item.Level = level
		p.nextToken()
	}

	// Parse name
	if p.curTokenIs(IDENT) {
		item.Name = p.curToken.Literal
		p.nextToken()
	}

	// Parse clauses
	for !p.curTokenIs(PERIOD) && !p.curTokenIs(NUMBER) && !p.curTokenIs(EOF) {
		switch p.curToken.Type {
		case PIC, PICTURE:
			p.nextToken()
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			// Parse PICTURE clause - can be complex like 9(5), X(20), S9(10)V9(2), etc.
			picBuilder := strings.Builder{}
			for !p.curTokenIs(PERIOD) && !p.curTokenIs(VALUE) && !p.curTokenIs(OCCURS) &&
				!p.curTokenIs(REDEFINES) && !p.curTokenIs(NUMBER) && !p.curTokenIs(EOF) {
				if p.curToken.Type == IDENT || p.curToken.Type == STRING || p.curToken.Type == NUMBER {
					picBuilder.WriteString(p.curToken.Literal)
				} else if p.curToken.Type == LPAREN {
					picBuilder.WriteString("(")
				} else if p.curToken.Type == RPAREN {
					picBuilder.WriteString(")")
				} else {
					break
				}
				p.nextToken()
			}
			item.Picture = picBuilder.String()
		case VALUE:
			p.nextToken()
			if p.curTokenIs(IS) {
				p.nextToken()
			}
			if p.curTokenIs(STRING) || p.curTokenIs(NUMBER) {
				item.Value = p.curToken.Literal
				p.nextToken()
			}
		case OCCURS:
			p.nextToken()
			if p.curTokenIs(NUMBER) {
				item.Occurs, _ = strconv.Atoi(p.curToken.Literal)
				p.nextToken()
			}
		case REDEFINES:
			p.nextToken()
			if p.curTokenIs(IDENT) {
				item.Redefines = p.curToken.Literal
				p.nextToken()
			}
		default:
			p.nextToken()
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	// Parse children (subordinate items)
	for p.curTokenIs(NUMBER) {
		nextLevel, _ := strconv.Atoi(p.curToken.Literal)
		if nextLevel > item.Level {
			child := p.parseDataItem()
			item.Children = append(item.Children, child)
		} else {
			break
		}
	}

	return item
}

// parseProcedureDivision parses the PROCEDURE DIVISION
func (p *Parser) parseProcedureDivision() *ProcedureDivision {
	div := &ProcedureDivision{
		Sections:   []*Section{},
		Statements: []Statement{},
	}

	p.expectToken(PROCEDURE)
	p.expectToken(DIVISION)
	p.expectToken(PERIOD)

	// Parse sections and paragraphs
	for !p.curTokenIs(EOF) {
		if p.curTokenIs(IDENT) && p.peekTokenIs(SECTION) {
			section := p.parseSection()
			div.Sections = append(div.Sections, section)
		} else if p.curTokenIs(IDENT) && p.peekTokenIs(PERIOD) {
			// Standalone paragraph
			para := p.parseParagraph()
			// Add to a default section
			if len(div.Sections) == 0 {
				div.Sections = append(div.Sections, &Section{Name: "MAIN"})
			}
			div.Sections[len(div.Sections)-1].Paragraphs = append(div.Sections[len(div.Sections)-1].Paragraphs, para)
		} else {
			stmt := p.parseStatement()
			if stmt != nil {
				div.Statements = append(div.Statements, stmt)
			} else {
				p.nextToken()
			}
		}
	}

	return div
}

// parseSection parses a SECTION
func (p *Parser) parseSection() *Section {
	section := &Section{
		Paragraphs: []*Paragraph{},
	}

	if p.curTokenIs(IDENT) {
		section.Name = p.curToken.Literal
		p.nextToken()
	}

	p.expectToken(SECTION)
	p.expectToken(PERIOD)

	// Parse paragraphs
	for !p.curTokenIs(EOF) {
		// Check if next token is a SECTION (end of current section)
		if p.peekTokenIs(SECTION) {
			break
		}
		// Check if this is a paragraph name
		if p.curTokenIs(IDENT) && p.peekTokenIs(PERIOD) {
			para := p.parseParagraph()
			section.Paragraphs = append(section.Paragraphs, para)
		} else {
			// Skip to next token if we're not at a paragraph
			p.nextToken()
		}
	}

	return section
}

// parseParagraph parses a PARAGRAPH
func (p *Parser) parseParagraph() *Paragraph {
	para := &Paragraph{
		Statements: []Statement{},
	}

	if p.curTokenIs(IDENT) {
		para.Name = p.curToken.Literal
		p.nextToken()
	}

	p.expectToken(PERIOD)

	// Parse statements until next paragraph or section
	for !p.curTokenIs(EOF) {
		if p.curTokenIs(IDENT) && (p.peekTokenIs(PERIOD) || p.peekTokenIs(SECTION)) {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			para.Statements = append(para.Statements, stmt)
		} else {
			break
		}
	}

	return para
}

// parseStatement parses a single COBOL statement
func (p *Parser) parseStatement() Statement {
	switch p.curToken.Type {
	case PERFORM:
		return p.parsePerformStatement()
	case MOVE:
		return p.parseMoveStatement()
	case DISPLAY:
		return p.parseDisplayStatement()
	case OPEN:
		return p.parseOpenStatement()
	case CLOSE:
		return p.parseCloseStatement()
	case READ:
		return p.parseReadStatement()
	case WRITE:
		return p.parseWriteStatement()
	case STOP:
		return p.parseStopStatement()
	case EXIT:
		return p.parseExitStatement()
	default:
		return nil
	}
}

// parsePerformStatement parses a PERFORM statement
func (p *Parser) parsePerformStatement() *PerformStatement {
	stmt := &PerformStatement{}

	p.expectToken(PERFORM)

	if p.curTokenIs(IDENT) {
		stmt.Target = p.curToken.Literal
		p.nextToken()

		if p.curTokenIs(THRU) || p.curTokenIs(THROUGH) {
			p.nextToken()
			if p.curTokenIs(IDENT) {
				stmt.Through = p.curToken.Literal
				p.nextToken()
			}
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseMoveStatement parses a MOVE statement
func (p *Parser) parseMoveStatement() *MoveStatement {
	stmt := &MoveStatement{
		Destination: []string{},
	}

	p.expectToken(MOVE)

	// Parse source
	stmt.Source = p.parseExpression()

	if p.curTokenIs(TO) {
		p.nextToken()
		// Parse destinations
		for p.curTokenIs(IDENT) {
			stmt.Destination = append(stmt.Destination, p.curToken.Literal)
			p.nextToken()
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseDisplayStatement parses a DISPLAY statement
func (p *Parser) parseDisplayStatement() *DisplayStatement {
	stmt := &DisplayStatement{
		Items: []Expression{},
	}

	p.expectToken(DISPLAY)

	// Parse items to display
	for !p.curTokenIs(PERIOD) && !p.curTokenIs(UPON) && !p.curTokenIs(EOF) {
		if p.curTokenIs(IDENT) || p.curTokenIs(STRING) || p.curTokenIs(NUMBER) {
			stmt.Items = append(stmt.Items, p.parseExpression())
		} else {
			break
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseOpenStatement parses an OPEN statement
func (p *Parser) parseOpenStatement() *OpenStatement {
	stmt := &OpenStatement{
		Files: []string{},
	}

	p.expectToken(OPEN)

	// Parse mode
	if p.curTokenIs(INPUT) || p.curTokenIs(OUTPUT) || p.curTokenIs(IO) || p.curTokenIs(EXTEND) {
		stmt.Mode = p.curToken.Literal
		p.nextToken()
	}

	// Parse file names
	for p.curTokenIs(IDENT) {
		stmt.Files = append(stmt.Files, p.curToken.Literal)
		p.nextToken()
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseCloseStatement parses a CLOSE statement
func (p *Parser) parseCloseStatement() *CloseStatement {
	stmt := &CloseStatement{
		Files: []string{},
	}

	p.expectToken(CLOSE)

	// Parse file names
	for p.curTokenIs(IDENT) {
		stmt.Files = append(stmt.Files, p.curToken.Literal)
		p.nextToken()
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseReadStatement parses a READ statement
func (p *Parser) parseReadStatement() *ReadStatement {
	stmt := &ReadStatement{}

	p.expectToken(READ)

	if p.curTokenIs(IDENT) {
		stmt.FileName = p.curToken.Literal
		p.nextToken()
	}

	if p.curTokenIs(INTO) {
		p.nextToken()
		if p.curTokenIs(IDENT) {
			stmt.Into = p.curToken.Literal
			p.nextToken()
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseWriteStatement parses a WRITE statement
func (p *Parser) parseWriteStatement() *WriteStatement {
	stmt := &WriteStatement{}

	p.expectToken(WRITE)

	if p.curTokenIs(IDENT) {
		stmt.RecordName = p.curToken.Literal
		p.nextToken()
	}

	if p.curTokenIs(FROM) {
		p.nextToken()
		if p.curTokenIs(IDENT) {
			stmt.From = p.curToken.Literal
			p.nextToken()
		}
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseStopStatement parses a STOP RUN statement
func (p *Parser) parseStopStatement() *StopStatement {
	stmt := &StopStatement{}

	p.expectToken(STOP)

	if p.curTokenIs(RUN) {
		p.nextToken()
	}

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseExitStatement parses an EXIT statement
func (p *Parser) parseExitStatement() *ExitStatement {
	stmt := &ExitStatement{}

	p.expectToken(EXIT)

	if p.curTokenIs(PERIOD) {
		p.nextToken()
	}

	return stmt
}

// parseExpression parses an expression
func (p *Parser) parseExpression() Expression {
	if p.curTokenIs(IDENT) {
		expr := &Identifier{Name: p.curToken.Literal}
		p.nextToken()
		return expr
	} else if p.curTokenIs(NUMBER) {
		expr := &NumberLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return expr
	} else if p.curTokenIs(STRING) {
		expr := &StringLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return expr
	}
	return nil
}

// Helper functions

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.NextToken()

	// Skip comments
	for p.peekToken.Type == COMMENT {
		p.peekToken = p.lexer.NextToken()
	}
}

func (p *Parser) curTokenIs(t TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectToken(t TokenType) {
	if !p.curTokenIs(t) {
		p.errors = append(p.errors, fmt.Sprintf("expected %v, got %v at line %d",
			t, p.curToken.Type, p.curToken.Line))
	}
	p.nextToken()
}

// Errors returns parsing errors
func (p *Parser) Errors() []string {
	return p.errors
}
