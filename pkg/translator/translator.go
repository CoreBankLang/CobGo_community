//go:build ignore
// +build ignore

package translator

import (
	"fmt"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/cobolparser"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

// Translator converts COBOL AST to CobGO DSL AST
type Translator struct {
	cobolAST *cobolparser.Program
	errors   []string
}

// NewTranslator creates a new COBOL to DSL translator
func NewTranslator(cobolAST *cobolparser.Program) *Translator {
	return &Translator{
		cobolAST: cobolAST,
		errors:   []string{},
	}
}

// Translate converts COBOL AST to DSL AST
func (t *Translator) Translate() (*parser.Program, error) {
	dslProgram := &parser.Program{
		Jobs: []*parser.Job{},
	}

	// Create main job from COBOL program
	job := t.translateProgram()
	if job != nil {
		dslProgram.Jobs = append(dslProgram.Jobs, job)
	}

	if len(t.errors) > 0 {
		return dslProgram, fmt.Errorf("translation errors: %v", t.errors)
	}

	return dslProgram, nil
}

// translateProgram converts a COBOL program to a DSL job
func (t *Translator) translateProgram() *parser.Job {
	job := &parser.Job{
		Name:      t.getProgramName(),
		Steps:     []*parser.Step{},
		Variables: []*parser.Variable{},
		Records:   []*parser.RecordDef{},
		Files:     []*parser.FileDef{},
	}

	// Translate file bindings from ENVIRONMENT DIVISION
	if t.cobolAST.Environment != nil && t.cobolAST.Environment.InputOutput != nil {
		t.translateFileBindings(job)
	}

	// Translate data structures from DATA DIVISION
	if t.cobolAST.Data != nil {
		t.translateDataStructures(job)
	}

	// Translate procedures from PROCEDURE DIVISION
	if t.cobolAST.Procedure != nil {
		t.translateProcedures(job)
	}

	return job
}

// getProgramName extracts the program name from IDENTIFICATION DIVISION
func (t *Translator) getProgramName() string {
	if t.cobolAST.Identification != nil && t.cobolAST.Identification.ProgramID != "" {
		// Convert COBOL name to CamelCase (SAMPLE-PROGRAM -> SampleProgram)
		return toCamelCase(t.cobolAST.Identification.ProgramID)
	}
	return "ConvertedProgram"
}

// translateFileBindings converts SELECT/FD statements to DSL file definitions
func (t *Translator) translateFileBindings(job *parser.Job) {
	if t.cobolAST.Environment == nil || t.cobolAST.Environment.InputOutput == nil {
		return
	}

	for _, fileControl := range t.cobolAST.Environment.InputOutput.FileControl {
		fileDef := &parser.FileDef{
			Name:         toSnakeCase(fileControl.FileName),
			Assign:       fileControl.AssignTo,
			Organization: strings.ToLower(fileControl.Organization),
			AccessMode:   strings.ToLower(fileControl.AccessMode),
			FileStatus:   toSnakeCase(fileControl.FileStatus),
		}

		job.Files = append(job.Files, fileDef)
	}
}

// translateDataStructures converts COBOL data items to DSL records and variables
func (t *Translator) translateDataStructures(job *parser.Job) {
	if t.cobolAST.Data == nil {
		return
	}

	// Translate FILE SECTION
	for _, fd := range t.cobolAST.Data.FileSection {
		record := t.translateDataItemToRecord(fd.RecordName, fd.RecordItems)
		if record != nil {
			job.Records = append(job.Records, record)
		}
	}

	// Translate WORKING-STORAGE SECTION
	for _, item := range t.cobolAST.Data.WorkingStorage {
		if item.Level == 1 || item.Level == 77 {
			if len(item.Children) > 0 {
				// Group item -> Record
				record := t.translateDataItemToRecord(item.Name, item.Children)
				if record != nil {
					job.Records = append(job.Records, record)
				}
				// Also create a variable of this record type
				variable := &parser.Variable{
					Name: toSnakeCase(item.Name),
					Type: toCamelCase(item.Name),
				}
				job.Variables = append(job.Variables, variable)
			} else {
				// Elementary item -> Variable
				variable := t.translateDataItemToVariable(item)
				if variable != nil {
					job.Variables = append(job.Variables, variable)
				}
			}
		}
	}
}

// translateDataItemToRecord converts a COBOL group item to a DSL record
func (t *Translator) translateDataItemToRecord(name string, children []*cobolparser.DataItem) *parser.RecordDef {
	if len(children) == 0 {
		return nil
	}

	record := &parser.RecordDef{
		Name:   toCamelCase(name),
		Fields: []*parser.Field{},
	}

	for _, child := range children {
		field := &parser.Field{
			Name: toSnakeCase(child.Name),
			Type: t.pictureToType(child.Picture),
		}

		// Handle OCCURS (arrays)
		if child.Occurs > 0 {
			field.IsArray = true
			field.ArraySize = child.Occurs
		}

		record.Fields = append(record.Fields, field)
	}

	return record
}

// translateDataItemToVariable converts a COBOL elementary item to a DSL variable
func (t *Translator) translateDataItemToVariable(item *cobolparser.DataItem) *parser.Variable {
	variable := &parser.Variable{
		Name: toSnakeCase(item.Name),
		Type: t.pictureToType(item.Picture),
	}

	// Handle initial value
	if item.Value != "" {
		variable.InitValue = item.Value
	}

	return variable
}

// pictureToType converts COBOL PICTURE clause to DSL type
func (t *Translator) pictureToType(pic string) string {
	pic = strings.ToUpper(strings.TrimSpace(pic))

	// Handle common PICTURE patterns
	if strings.Contains(pic, "9") && strings.Contains(pic, "V") {
		// Decimal: PIC 9(n)V99 -> decimal(n+m,m)
		return "decimal(15,2)" // Default decimal type
	} else if strings.Contains(pic, "9") {
		// Integer: PIC 9(n) -> int32 or int64
		return "int32"
	} else if strings.Contains(pic, "X") {
		// String: PIC X(n) -> string(n)
		// Extract length from X(n)
		return "string(50)" // Default string length
	}

	return "string(50)" // Default fallback
}

// translateProcedures converts PROCEDURE DIVISION to DSL steps
func (t *Translator) translateProcedures(job *parser.Job) {
	if t.cobolAST.Procedure == nil {
		return
	}

	// Create main step
	mainStep := &parser.Step{
		Name:       "Main",
		Statements: []parser.Statement{},
	}

	// Translate standalone statements (before any sections)
	for _, stmt := range t.cobolAST.Procedure.Statements {
		dslStmt := t.translateStatement(stmt)
		if dslStmt != nil {
			mainStep.Statements = append(mainStep.Statements, dslStmt)
		}
	}

	job.Steps = append(job.Steps, mainStep)

	// Translate sections to steps
	for _, section := range t.cobolAST.Procedure.Sections {
		step := t.translateSectionToStep(section)
		if step != nil {
			job.Steps = append(job.Steps, step)
		}
	}
}

// translateSectionToStep converts a COBOL SECTION to a DSL step
func (t *Translator) translateSectionToStep(section *cobolparser.Section) *parser.Step {
	step := &parser.Step{
		Name:       toCamelCase(section.Name),
		Statements: []parser.Statement{},
	}

	// Translate all paragraphs in the section
	for _, para := range section.Paragraphs {
		// Add paragraph as a comment
		comment := &parser.CommentStatement{
			Text: fmt.Sprintf("Paragraph: %s", para.Name),
		}
		step.Statements = append(step.Statements, comment)

		// Translate paragraph statements
		for _, stmt := range para.Statements {
			dslStmt := t.translateStatement(stmt)
			if dslStmt != nil {
				step.Statements = append(step.Statements, dslStmt)
			}
		}
	}

	return step
}

// translateStatement converts a COBOL statement to a DSL statement
func (t *Translator) translateStatement(cobolStmt cobolparser.Statement) parser.Statement {
	switch stmt := cobolStmt.(type) {
	case *cobolparser.PerformStatement:
		return t.translatePerformStatement(stmt)
	case *cobolparser.MoveStatement:
		return t.translateMoveStatement(stmt)
	case *cobolparser.DisplayStatement:
		return t.translateDisplayStatement(stmt)
	case *cobolparser.OpenStatement:
		return t.translateOpenStatement(stmt)
	case *cobolparser.CloseStatement:
		return t.translateCloseStatement(stmt)
	case *cobolparser.ReadStatement:
		return t.translateReadStatement(stmt)
	case *cobolparser.WriteStatement:
		return t.translateWriteStatement(stmt)
	case *cobolparser.StopStatement:
		return t.translateStopStatement(stmt)
	case *cobolparser.ExitStatement:
		return t.translateExitStatement(stmt)
	default:
		return nil
	}
}

// translatePerformStatement converts PERFORM to step call
func (t *Translator) translatePerformStatement(stmt *cobolparser.PerformStatement) parser.Statement {
	return &parser.CallStepStatement{
		StepName: toCamelCase(stmt.Target),
	}
}

// translateMoveStatement converts MOVE to assignment
func (t *Translator) translateMoveStatement(stmt *cobolparser.MoveStatement) parser.Statement {
	if len(stmt.Destination) == 0 {
		return nil
	}

	// For simplicity, translate to assignment of first destination
	dest := toSnakeCase(stmt.Destination[0])

	return &parser.AssignmentStatement{
		Target: dest,
		Value:  t.translateExpression(stmt.Source),
	}
}

// translateDisplayStatement converts DISPLAY statement
func (t *Translator) translateDisplayStatement(stmt *cobolparser.DisplayStatement) parser.Statement {
	items := []parser.Expression{}
	for _, item := range stmt.Items {
		items = append(items, t.translateExpression(item))
	}

	return &parser.DisplayStatement{
		Items: items,
	}
}

// translateOpenStatement converts OPEN statement
func (t *Translator) translateOpenStatement(stmt *cobolparser.OpenStatement) parser.Statement {
	if len(stmt.Files) == 0 {
		return nil
	}

	return &parser.OpenFileStatement{
		FileName: toSnakeCase(stmt.Files[0]),
		Mode:     strings.ToLower(stmt.Mode),
	}
}

// translateCloseStatement converts CLOSE statement
func (t *Translator) translateCloseStatement(stmt *cobolparser.CloseStatement) parser.Statement {
	if len(stmt.Files) == 0 {
		return nil
	}

	return &parser.CloseFileStatement{
		FileName: toSnakeCase(stmt.Files[0]),
	}
}

// translateReadStatement converts READ statement
func (t *Translator) translateReadStatement(stmt *cobolparser.ReadStatement) parser.Statement {
	return &parser.ReadFileStatement{
		FileName: toSnakeCase(stmt.FileName),
		Into:     toSnakeCase(stmt.Into),
	}
}

// translateWriteStatement converts WRITE statement
func (t *Translator) translateWriteStatement(stmt *cobolparser.WriteStatement) parser.Statement {
	return &parser.WriteFileStatement{
		RecordName: toSnakeCase(stmt.RecordName),
		From:       toSnakeCase(stmt.From),
	}
}

// translateStopStatement converts STOP RUN
func (t *Translator) translateStopStatement(stmt *cobolparser.StopStatement) parser.Statement {
	return &parser.ExitStatement{}
}

// translateExitStatement converts EXIT
func (t *Translator) translateExitStatement(stmt *cobolparser.ExitStatement) parser.Statement {
	return &parser.ExitStatement{}
}

// translateExpression converts COBOL expression to DSL expression
func (t *Translator) translateExpression(cobolExpr cobolparser.Expression) parser.Expression {
	switch expr := cobolExpr.(type) {
	case *cobolparser.Identifier:
		return &parser.IdentifierExpr{
			Name: toSnakeCase(expr.Name),
		}
	case *cobolparser.NumberLiteral:
		return &parser.NumberLiteralExpr{
			Value: expr.Value,
		}
	case *cobolparser.StringLiteral:
		return &parser.StringLiteralExpr{
			Value: expr.Value,
		}
	default:
		return &parser.StringLiteralExpr{Value: ""}
	}
}

// Utility functions for naming conventions

// toCamelCase converts COBOL-STYLE-NAME to CamelCase
func toCamelCase(name string) string {
	parts := strings.Split(name, "-")
	result := ""
	for _, part := range parts {
		if len(part) > 0 {
			result += strings.ToUpper(part[0:1]) + strings.ToLower(part[1:])
		}
	}
	return result
}

// toSnakeCase converts COBOL-STYLE-NAME to snake_case
func toSnakeCase(name string) string {
	return strings.ToLower(strings.ReplaceAll(name, "-", "_"))
}

// Errors returns translation errors
func (t *Translator) Errors() []string {
	return t.errors
}
