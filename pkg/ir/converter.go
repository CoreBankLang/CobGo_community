package ir

import (
	"fmt"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

// ASTToIRConverter converts AST to IR
type ASTToIRConverter struct {
	Program *Program
	Errors  []error
}

// NewASTToIRConverter creates a new AST to IR converter
func NewASTToIRConverter() *ASTToIRConverter {
	return &ASTToIRConverter{
		Program: NewProgram("main"),
		Errors:  []error{},
	}
}

// Convert converts an AST program to IR
func (c *ASTToIRConverter) Convert(astProgram *parser.Program) (*Program, error) {
	c.Program = NewProgram("main")
	c.Errors = []error{}

	// Convert records
	for _, recordAST := range astProgram.Records {
		record := c.convertRecord(recordAST)
		if record != nil {
			c.Program.AddRecord(record)
		}
	}

	// Convert jobs
	for _, jobAST := range astProgram.Jobs {
		job := c.convertJob(jobAST)
		if job != nil {
			c.Program.AddJob(job)
		}
	}

	if len(c.Errors) > 0 {
		return c.Program, fmt.Errorf("conversion failed with %d errors", len(c.Errors))
	}

	return c.Program, nil
}

// convertJob converts a job AST to IR
func (c *ASTToIRConverter) convertJob(jobAST *parser.JobStatement) *Job {
	job := &Job{
		Name:      jobAST.Name.Value,
		Steps:     []*Step{},
		Variables: []*Variable{},
		Line:      jobAST.Token.Line,
		Column:    jobAST.Token.Column,
	}

	// Convert job body
	if jobAST.Body != nil {
		job.Body = c.convertBlock(jobAST.Body)

		// Extract steps and variables from the body
		c.extractStepsAndVariables(job, jobAST.Body)
	}

	return job
}

// convertStep converts a step AST to IR
func (c *ASTToIRConverter) convertStep(stepAST *parser.StepStatement) *Step {
	step := &Step{
		Name:       stepAST.Name.Value,
		Parameters: []*Parameter{},
		Variables:  []*Variable{},
		Line:       stepAST.Token.Line,
		Column:     stepAST.Token.Column,
	}

	// Convert parameters
	for _, paramAST := range stepAST.Parameters {
		param := &Parameter{
			Name:   paramAST.Value,
			Type:   &TypeInfo{Type: "unknown"}, // Will be filled in during semantic analysis
			Line:   paramAST.Token.Line,
			Column: paramAST.Token.Column,
		}
		step.Parameters = append(step.Parameters, param)
	}

	// Convert return type
	if stepAST.ReturnType != nil {
		step.ReturnType = c.convertTypeAnnotation(stepAST.ReturnType)
	}

	// Convert step body
	if stepAST.Body != nil {
		step.Body = c.convertBlock(stepAST.Body)

		// Extract variables from the body
		c.extractVariablesFromBlock(step, stepAST.Body)
	}

	return step
}

// convertRecord converts a record AST to IR
func (c *ASTToIRConverter) convertRecord(recordAST *parser.RecordStatement) *Record {
	record := &Record{
		Name:   recordAST.Name.Value,
		Fields: []*Field{},
		Line:   recordAST.Token.Line,
		Column: recordAST.Token.Column,
	}

	// Convert fields
	for _, fieldAST := range recordAST.Fields {
		field := &Field{
			Name:   fieldAST.Name.Value,
			Type:   c.convertTypeAnnotation(fieldAST.Type),
			Line:   fieldAST.Name.Token.Line,
			Column: fieldAST.Name.Token.Column,
		}
		record.Fields = append(record.Fields, field)
	}

	return record
}

// convertBlock converts a block AST to IR
func (c *ASTToIRConverter) convertBlock(blockAST *parser.BlockStatement) *Block {
	block := &Block{
		Statements: []Statement{},
		Line:       blockAST.Token.Line,
		Column:     blockAST.Token.Column,
	}

	// Convert statements
	for _, stmtAST := range blockAST.Statements {
		stmt := c.convertStatement(stmtAST)
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
	}

	return block
}

// convertStatement converts a statement AST to IR
func (c *ASTToIRConverter) convertStatement(stmtAST parser.Statement) Statement {
	switch stmt := stmtAST.(type) {
	case *parser.VarStatement:
		return c.convertVarStatement(stmt)
	case *parser.ExpressionStatement:
		return c.convertExpressionStatement(stmt)
	case *parser.IfStatement:
		return c.convertIfStatement(stmt)
	case *parser.ForStatement:
		return c.convertForStatement(stmt)
	case *parser.WhileStatement:
		return c.convertWhileStatement(stmt)
	case *parser.ReturnStatement:
		return c.convertReturnStatement(stmt)
	case *parser.DisplayStatement:
		return c.convertDisplayStatement(stmt)
	case *parser.AcceptStatement:
		return c.convertAcceptStatement(stmt)
	case *parser.PerformStatement:
		return c.convertPerformStatement(stmt)
	case *parser.EvaluateStatement:
		return c.convertEvaluateStatement(stmt)
	case *parser.MoveStatement:
		return c.convertMoveStatement(stmt)
	case *parser.ComputeStatement:
		return c.convertComputeStatement(stmt)
	case *parser.FileOperationStatement:
		return c.convertFileOperationStatement(stmt)
	case *parser.CopyStatement:
		return c.convertCopyStatement(stmt)
	case *parser.InspectStatementAST:
		return c.convertInspectStatement(stmt)
	case *parser.StringStatementAST:
		return c.convertStringStatement(stmt)
	case *parser.UnstringStatementAST:
		return c.convertUnstringStatement(stmt)
	case *parser.SearchStatementAST:
		return c.convertSearchStatement(stmt)
	case *parser.StepStatement:
		// Steps are handled separately in extractStepsAndVariables
		return nil
	default:
		return nil
	}
}

// convertVarStatement converts a variable statement AST to IR
func (c *ASTToIRConverter) convertVarStatement(stmtAST *parser.VarStatement) *VarStatement {
	// Convert type information
	var typeInfo *TypeInfo
	if stmtAST.Type != nil {
		var size *int
		if stmtAST.Type.Size != nil {
			sizeVal := int(stmtAST.Type.Size.Value)
			size = &sizeVal
		}
		typeInfo = &TypeInfo{
			Type: stmtAST.Type.TypeName,
			Size: size,
		}
	}

	// Convert value if present
	var value interface{}
	if stmtAST.Value != nil {
		// Convert the expression to get the actual value
		expr := c.convertExpression(stmtAST.Value)
		if expr != nil {
			// For literal expressions, extract the actual value
			if litExpr, ok := expr.(*LiteralExpression); ok {
				value = litExpr.Value
			} else {
				// For other expressions, store the original string for now
				value = stmtAST.Value.String()
			}
		}
	}

	return &VarStatement{
		Name:    stmtAST.Name.Value,
		Type:    typeInfo,
		Value:   value,
		IsConst: false, // Default to false for now
		Line:    stmtAST.Token.Line,
		Column:  stmtAST.Token.Column,
	}
}

// convertExpressionStatement converts an expression statement AST to IR
func (c *ASTToIRConverter) convertExpressionStatement(stmtAST *parser.ExpressionStatement) *ExpressionStatement {
	return &ExpressionStatement{
		Expression: c.convertExpression(stmtAST.Expression),
		Line:       stmtAST.Token.Line,
		Column:     stmtAST.Token.Column,
	}
}

// convertIfStatement converts an if statement AST to IR
func (c *ASTToIRConverter) convertIfStatement(stmtAST *parser.IfStatement) *IfStatement {
	ifStmt := &IfStatement{
		Condition: c.convertExpression(stmtAST.Condition),
		ThenBlock: c.convertBlock(stmtAST.Consequence),
		Line:      stmtAST.Token.Line,
		Column:    stmtAST.Token.Column,
	}

	// Convert else block if present
	if stmtAST.Alternative != nil {
		ifStmt.ElseBlock = c.convertBlock(stmtAST.Alternative.(*parser.BlockStatement))
	}

	return ifStmt
}

// convertForStatement converts a for statement AST to IR
func (c *ASTToIRConverter) convertForStatement(stmtAST *parser.ForStatement) *ForStatement {
	forStmt := &ForStatement{
		Condition: c.convertExpression(stmtAST.Condition),
		Body:      c.convertBlock(stmtAST.Body),
		Line:      stmtAST.Token.Line,
		Column:    stmtAST.Token.Column,
	}

	// Convert init and update statements
	if stmtAST.Init != nil {
		forStmt.Init = c.convertStatement(stmtAST.Init)
	}
	if stmtAST.Update != nil {
		forStmt.Update = c.convertStatement(stmtAST.Update)
	}

	return forStmt
}

// convertWhileStatement converts a while statement AST to IR
func (c *ASTToIRConverter) convertWhileStatement(stmtAST *parser.WhileStatement) *WhileStatement {
	return &WhileStatement{
		Condition: c.convertExpression(stmtAST.Condition),
		Body:      c.convertBlock(stmtAST.Body),
		Line:      stmtAST.Token.Line,
		Column:    stmtAST.Token.Column,
	}
}

// convertReturnStatement converts a return statement AST to IR
func (c *ASTToIRConverter) convertReturnStatement(stmtAST *parser.ReturnStatement) *ReturnStatement {
	returnStmt := &ReturnStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	// Convert return value if present
	if stmtAST.ReturnValue != nil {
		returnStmt.Value = c.convertExpression(stmtAST.ReturnValue)
	}

	return returnStmt
}

// convertDisplayStatement converts a display statement AST to IR
func (c *ASTToIRConverter) convertDisplayStatement(stmtAST *parser.DisplayStatement) *DisplayStatement {
	var args []Expression
	for _, arg := range stmtAST.Args {
		convertedArg := c.convertExpression(arg)
		if convertedArg != nil {
			args = append(args, convertedArg)
		}
	}

	displayStmt := &DisplayStatement{
		Args:   args,
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	return displayStmt
}

// convertAcceptStatement converts an accept statement AST to IR
func (c *ASTToIRConverter) convertAcceptStatement(stmtAST *parser.AcceptStatement) *AcceptStatement {
	return &AcceptStatement{
		Prompt: c.convertExpression(stmtAST.Prompt),
		Target: stmtAST.Target.Value,
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}
}

// convertExpression converts an expression AST to IR
func (c *ASTToIRConverter) convertExpression(exprAST parser.Expression) Expression {
	switch expr := exprAST.(type) {
	case *parser.Identifier:
		return c.convertIdentifier(expr)
	case *parser.IntegerLiteral:
		return c.convertIntegerLiteral(expr)
	case *parser.DecimalLiteral:
		return c.convertDecimalLiteral(expr)
	case *parser.StringLiteral:
		return c.convertStringLiteral(expr)
	case *parser.BooleanLiteral:
		return c.convertBooleanLiteral(expr)
	case *parser.PrefixExpression:
		return c.convertPrefixExpression(expr)
	case *parser.InfixExpression:
		return c.convertInfixExpression(expr)
	case *parser.CallExpression:
		return c.convertCallExpression(expr)
	case *parser.IndexExpression:
		return c.convertIndexExpression(expr)
	default:
		return nil
	}
}

// convertIdentifier converts an identifier AST to IR
func (c *ASTToIRConverter) convertIdentifier(exprAST *parser.Identifier) *IdentifierExpression {
	return &IdentifierExpression{
		Name:   exprAST.Value,
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertIntegerLiteral converts an integer literal AST to IR
func (c *ASTToIRConverter) convertIntegerLiteral(exprAST *parser.IntegerLiteral) *LiteralExpression {
	return &LiteralExpression{
		Value:  exprAST.Value,
		Type:   "int64",
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertDecimalLiteral converts a decimal literal AST to IR
func (c *ASTToIRConverter) convertDecimalLiteral(exprAST *parser.DecimalLiteral) *LiteralExpression {
	return &LiteralExpression{
		Value:  exprAST.Token.Literal, // Use original string representation instead of float64
		Type:   "decimal",
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertStringLiteral converts a string literal AST to IR
func (c *ASTToIRConverter) convertStringLiteral(exprAST *parser.StringLiteral) *LiteralExpression {
	return &LiteralExpression{
		Value:  exprAST.Value,
		Type:   "string",
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertBooleanLiteral converts a boolean literal AST to IR
func (c *ASTToIRConverter) convertBooleanLiteral(exprAST *parser.BooleanLiteral) *LiteralExpression {
	return &LiteralExpression{
		Value:  exprAST.Value,
		Type:   "boolean",
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertPrefixExpression converts a prefix expression AST to IR
func (c *ASTToIRConverter) convertPrefixExpression(exprAST *parser.PrefixExpression) *UnaryExpression {
	return &UnaryExpression{
		Operator: exprAST.Operator,
		Operand:  c.convertExpression(exprAST.Right),
		Line:     exprAST.Token.Line,
		Column:   exprAST.Token.Column,
	}
}

// convertInfixExpression converts an infix expression AST to IR
func (c *ASTToIRConverter) convertInfixExpression(exprAST *parser.InfixExpression) *BinaryExpression {
	return &BinaryExpression{
		Left:     c.convertExpression(exprAST.Left),
		Operator: exprAST.Operator,
		Right:    c.convertExpression(exprAST.Right),
		Line:     exprAST.Token.Line,
		Column:   exprAST.Token.Column,
	}
}

// convertCallExpression converts a call expression AST to IR
func (c *ASTToIRConverter) convertCallExpression(exprAST *parser.CallExpression) *CallExpression {
	callExpr := &CallExpression{
		Function:  c.convertExpression(exprAST.Function),
		Arguments: []Expression{},
		Line:      exprAST.Token.Line,
		Column:    exprAST.Token.Column,
	}

	// Convert arguments
	for _, argAST := range exprAST.Arguments {
		callExpr.Arguments = append(callExpr.Arguments, c.convertExpression(argAST))
	}

	return callExpr
}

// convertIndexExpression converts an index expression AST to IR
func (c *ASTToIRConverter) convertIndexExpression(exprAST *parser.IndexExpression) *ArrayAccess {
	return &ArrayAccess{
		Array:  c.convertExpression(exprAST.Left),
		Index:  c.convertExpression(exprAST.Index),
		Line:   exprAST.Token.Line,
		Column: exprAST.Token.Column,
	}
}

// convertTypeAnnotation converts a type annotation AST to IR
func (c *ASTToIRConverter) convertTypeAnnotation(typeAST *parser.TypeAnnotation) *TypeInfo {
	typeInfo := &TypeInfo{
		Type:   typeAST.TypeName,
		GoType: c.getGoType(typeAST.TypeName),
		Line:   0, // Type annotations don't have line info in AST
		Column: 0, // Type annotations don't have column info in AST
	}

	// Set size if present
	if typeAST.Size != nil {
		size := int(typeAST.Size.Value)
		typeInfo.Size = &size
	}

	// Set precision for decimal types (scale is not available in AST yet)
	if typeAST.Precision != nil {
		precision := int(typeAST.Precision.Value)
		typeInfo.Precision = &precision
		// For now, assume scale is 0 if not specified
		scale := 0
		typeInfo.Scale = &scale
	}

	return typeInfo
}

// getGoType returns the Go type for a CobGO type
func (c *ASTToIRConverter) getGoType(cobgoType string) string {
	switch cobgoType {
	case "int32":
		return "int32"
	case "int64":
		return "int64"
	case "string":
		return "string"
	case "decimal":
		return "decimal.Decimal"
	case "date":
		return "time.Time"
	case "bool":
		return "bool"
	default:
		return "interface{}"
	}
}

// extractStepsAndVariables extracts steps and variables from a block
func (c *ASTToIRConverter) extractStepsAndVariables(job *Job, block *parser.BlockStatement) {
	for _, stmt := range block.Statements {
		switch s := stmt.(type) {
		case *parser.StepStatement:
			step := c.convertStep(s)
			job.Steps = append(job.Steps, step)
		case *parser.VarStatement:
			// Convert value if present
			var value interface{}
			if s.Value != nil {
				// Convert the expression to get the actual value
				expr := c.convertExpression(s.Value)
				if expr != nil {
					// For literal expressions, extract the actual value
					if litExpr, ok := expr.(*LiteralExpression); ok {
						value = litExpr.Value
					} else {
						// For other expressions, store the original string for now
						value = s.Value.String()
					}
				}
			}

			variable := &Variable{
				Name:    s.Name.Value,
				Type:    c.convertTypeAnnotation(s.Type),
				Value:   value,
				IsConst: false,
				Line:    s.Token.Line,
				Column:  s.Token.Column,
			}
			job.Variables = append(job.Variables, variable)
		}
	}
}

// extractVariablesFromBlock extracts variables from a block
func (c *ASTToIRConverter) extractVariablesFromBlock(step *Step, block *parser.BlockStatement) {
	for _, stmt := range block.Statements {
		if varStmt, ok := stmt.(*parser.VarStatement); ok {
			variable := &Variable{
				Name:    varStmt.Name.Value,
				Type:    c.convertTypeAnnotation(varStmt.Type),
				IsConst: false,
				Line:    varStmt.Token.Line,
				Column:  varStmt.Token.Column,
			}
			step.Variables = append(step.Variables, variable)
		}
	}
}

// convertPerformStatement converts a PERFORM statement AST to IR
func (c *ASTToIRConverter) convertPerformStatement(stmtAST *parser.PerformStatement) *PerformStatement {
	stmt := &PerformStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	if stmtAST.Target != nil {
		stmt.Target = stmtAST.Target.Value
	}

	if stmtAST.Condition != nil {
		stmt.Condition = c.convertExpression(stmtAST.Condition)
	}

	if stmtAST.Varying != nil {
		stmt.Varying = stmtAST.Varying.Value
	}

	if stmtAST.From != nil {
		stmt.From = c.convertExpression(stmtAST.From)
	}

	if stmtAST.To != nil {
		stmt.To = c.convertExpression(stmtAST.To)
	}

	if stmtAST.By != nil {
		stmt.By = c.convertExpression(stmtAST.By)
	}

	if stmtAST.Times != nil {
		stmt.Times = c.convertExpression(stmtAST.Times)
	}

	if stmtAST.Body != nil {
		stmt.Body = c.convertBlock(stmtAST.Body)
	}

	return stmt
}

// convertEvaluateStatement converts an EVALUATE statement AST to IR
func (c *ASTToIRConverter) convertEvaluateStatement(stmtAST *parser.EvaluateStatement) *EvaluateStatement {
	stmt := &EvaluateStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	if stmtAST.Expression != nil {
		stmt.Expression = c.convertExpression(stmtAST.Expression)
	}

	// Convert WHEN clauses
	for _, whenAST := range stmtAST.Cases {
		whenCase := &WhenCase{
			Line:   whenAST.Token.Line,
			Column: whenAST.Token.Column,
		}

		if whenAST.Value != nil {
			whenCase.Value = c.convertExpression(whenAST.Value)
		}

		if whenAST.Body != nil {
			whenCase.Body = c.convertBlock(whenAST.Body)
		}

		stmt.Cases = append(stmt.Cases, whenCase)
	}

	if stmtAST.Default != nil {
		stmt.Default = c.convertBlock(stmtAST.Default)
	}

	return stmt
}

// convertMoveStatement converts a MOVE statement AST to IR
func (c *ASTToIRConverter) convertMoveStatement(stmtAST *parser.MoveStatement) *MoveStatement {
	stmt := &MoveStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	if stmtAST.From != nil {
		stmt.From = c.convertExpression(stmtAST.From)
	}

	if stmtAST.To != nil {
		stmt.To = c.convertExpression(stmtAST.To)
	}

	return stmt
}

// convertComputeStatement converts a COMPUTE statement AST to IR
func (c *ASTToIRConverter) convertComputeStatement(stmtAST *parser.ComputeStatement) *ComputeStatement {
	stmt := &ComputeStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	if stmtAST.Target != nil {
		stmt.Target = c.convertExpression(stmtAST.Target)
	}

	if stmtAST.Value != nil {
		stmt.Value = c.convertExpression(stmtAST.Value)
	}

	return stmt
}

// convertFileOperationStatement converts a file operation statement AST to IR
func (c *ASTToIRConverter) convertFileOperationStatement(stmtAST *parser.FileOperationStatement) *FileOperationStatement {
	stmt := &FileOperationStatement{
		Operation: stmtAST.Operation,
		Line:      stmtAST.Token.Line,
		Column:    stmtAST.Token.Column,
	}

	if stmtAST.File != nil {
		stmt.File = stmtAST.File.Value
	}

	if stmtAST.Record != nil {
		stmt.Record = stmtAST.Record.Value
	}

	if stmtAST.Into != nil {
		stmt.Into = stmtAST.Into.Value
	}

	if stmtAST.From != nil {
		stmt.From = stmtAST.From.Value
	}

	if stmtAST.At != nil {
		stmt.At = c.convertExpression(stmtAST.At)
	}

	if stmtAST.Invalid != nil {
		stmt.Invalid = c.convertBlock(stmtAST.Invalid)
	}

	if stmtAST.End != nil {
		stmt.End = c.convertBlock(stmtAST.End)
	}

	return stmt
}

// convertCopyStatement converts a COPY statement AST to IR
func (c *ASTToIRConverter) convertCopyStatement(stmtAST *parser.CopyStatement) *CopyStatement {
	stmt := &CopyStatement{
		Line:   stmtAST.Token.Line,
		Column: stmtAST.Token.Column,
	}

	if stmtAST.File != nil {
		stmt.File = stmtAST.File.Value
	}

	return stmt
}

// convertInspectStatement converts an INSPECT statement AST to IR
func (c *ASTToIRConverter) convertInspectStatement(stmtAST *parser.InspectStatementAST) *InspectStatement {
	stmt := &InspectStatement{
		Identifier: stmtAST.Target.Value,
		Operation:  stmtAST.Operation,
		Mode:       stmtAST.Mode,
		Line:       stmtAST.Line,
		Column:     stmtAST.Column,
	}

	// Convert pattern
	if stmtAST.Pattern != nil {
		switch p := stmtAST.Pattern.(type) {
		case *parser.StringLiteral:
			stmt.Pattern = p.Value
		case *parser.Identifier:
			stmt.Pattern = p.Value
		}
	}

	// Convert replacement
	if stmtAST.Replacement != nil {
		switch r := stmtAST.Replacement.(type) {
		case *parser.StringLiteral:
			stmt.Replacement = r.Value
		case *parser.Identifier:
			stmt.Replacement = r.Value
		}
	}

	// Convert counter
	if stmtAST.Counter != nil {
		stmt.Counter = stmtAST.Counter.Value
	}

	// Convert before/after clause
	if stmtAST.BeforeAfter != nil {
		stmt.BeforeAfter = stmtAST.BeforeAfter.Type
		switch d := stmtAST.BeforeAfter.Delimiter.(type) {
		case *parser.StringLiteral:
			stmt.Delimiter = d.Value
		case *parser.Identifier:
			stmt.Delimiter = d.Value
		}
	}

	return stmt
}

// convertStringStatement converts a STRING statement AST to IR
func (c *ASTToIRConverter) convertStringStatement(stmtAST *parser.StringStatementAST) *StringStatement {
	stmt := &StringStatement{
		Sources:     []StringSource{},
		Destination: stmtAST.Destination.Value,
		Line:        stmtAST.Line,
		Column:      stmtAST.Column,
	}

	// Convert sources
	for _, sourceAST := range stmtAST.Sources {
		var source string
		switch s := sourceAST.Value.(type) {
		case *parser.StringLiteral:
			source = s.Value
		case *parser.Identifier:
			source = s.Value
		}

		stmt.Sources = append(stmt.Sources, StringSource{
			Source:    source,
			Delimiter: sourceAST.Delimiter,
		})
	}

	// Convert pointer
	if stmtAST.Pointer != nil {
		stmt.Pointer = stmtAST.Pointer.Value
	}

	// Convert overflow handler
	if stmtAST.OnOverflow != nil {
		stmt.OnOverflow = c.convertBlock(stmtAST.OnOverflow)
	}

	return stmt
}

// convertUnstringStatement converts an UNSTRING statement AST to IR
func (c *ASTToIRConverter) convertUnstringStatement(stmtAST *parser.UnstringStatementAST) *UnstringStatement {
	stmt := &UnstringStatement{
		Delimiters: stmtAST.Delimiters,
		Targets:    []string{},
		Line:       stmtAST.Line,
		Column:     stmtAST.Column,
	}

	// Convert source
	switch s := stmtAST.Source.(type) {
	case *parser.StringLiteral:
		stmt.Source = s.Value
	case *parser.Identifier:
		stmt.Source = s.Value
	}

	// Convert targets
	for _, targetAST := range stmtAST.Targets {
		stmt.Targets = append(stmt.Targets, targetAST.Value)
	}

	// Convert pointer
	if stmtAST.Pointer != nil {
		stmt.Pointer = stmtAST.Pointer.Value
	}

	// Convert tallying
	if stmtAST.Tallying != nil {
		stmt.Tallying = stmtAST.Tallying.Value
	}

	// Convert overflow handler
	if stmtAST.OnOverflow != nil {
		stmt.OnOverflow = c.convertBlock(stmtAST.OnOverflow)
	}

	return stmt
}

// convertSearchStatement converts a SEARCH statement AST to IR
func (c *ASTToIRConverter) convertSearchStatement(stmtAST *parser.SearchStatementAST) *SearchStatement {
	stmt := &SearchStatement{
		TableName:   stmtAST.TableName.Value,
		IsSearchAll: stmtAST.IsSearchAll,
		WhenClauses: []SearchWhen{},
		Line:        stmtAST.Line,
		Column:      stmtAST.Column,
	}

	// Convert index name or varying
	if stmtAST.IndexName != nil {
		stmt.IndexName = stmtAST.IndexName.Value
	}
	if stmtAST.Varying != nil {
		stmt.IndexName = stmtAST.Varying.Value
	}

	// Convert WHEN clauses
	for _, whenAST := range stmtAST.WhenClauses {
		whenClause := SearchWhen{
			Body: c.convertBlock(whenAST.Body),
		}
		// Note: Condition conversion is simplified for now
		// A full implementation would need proper expression conversion
		stmt.WhenClauses = append(stmt.WhenClauses, whenClause)
	}

	// Convert AT END handler
	if stmtAST.AtEnd != nil {
		stmt.AtEnd = c.convertBlock(stmtAST.AtEnd)
	}

	return stmt
}
