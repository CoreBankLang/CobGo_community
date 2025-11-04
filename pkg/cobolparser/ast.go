package cobolparser

// COBOL AST node types

// Program represents a complete COBOL program
type Program struct {
	Identification *IdentificationDivision
	Environment    *EnvironmentDivision
	Data           *DataDivision
	Procedure      *ProcedureDivision
}

// IdentificationDivision represents the IDENTIFICATION DIVISION
type IdentificationDivision struct {
	ProgramID   string
	Author      string
	DateWritten string
	Remarks     string
}

// EnvironmentDivision represents the ENVIRONMENT DIVISION
type EnvironmentDivision struct {
	Configuration *ConfigurationSection
	InputOutput   *InputOutputSection
}

// ConfigurationSection represents the CONFIGURATION SECTION
type ConfigurationSection struct {
	SourceComputer string
	ObjectComputer string
}

// InputOutputSection represents the INPUT-OUTPUT SECTION
type InputOutputSection struct {
	FileControl []*FileControlEntry
}

// FileControlEntry represents a SELECT statement
type FileControlEntry struct {
	FileName     string
	AssignTo     string
	Organization string // SEQUENTIAL, INDEXED, RELATIVE
	AccessMode   string // SEQUENTIAL, RANDOM, DYNAMIC
	RecordKey    string // For indexed files
	FileStatus   string
}

// DataDivision represents the DATA DIVISION
type DataDivision struct {
	FileSection    []*FileDescription
	WorkingStorage []*DataItem
	LocalStorage   []*DataItem
	LinkageSection []*DataItem
}

// FileDescription represents an FD entry
type FileDescription struct {
	FileName    string
	RecordName  string
	RecordItems []*DataItem
}

// DataItem represents a data item (01, 05, etc.)
type DataItem struct {
	Level           int
	Name            string
	Picture         string
	Value           string
	Occurs          int
	OccursDepending string
	Redefines       string
	Children        []*DataItem
}

// ProcedureDivision represents the PROCEDURE DIVISION
type ProcedureDivision struct {
	Parameters []*Parameter
	Sections   []*Section
	Statements []Statement
}

// Parameter represents a parameter in USING/RETURNING
type Parameter struct {
	Name string
	Mode string // BY REFERENCE, BY CONTENT, BY VALUE
}

// Section represents a SECTION in PROCEDURE DIVISION
type Section struct {
	Name       string
	Paragraphs []*Paragraph
}

// Paragraph represents a PARAGRAPH
type Paragraph struct {
	Name       string
	Statements []Statement
}

// Statement interface - all COBOL statements implement this
type Statement interface {
	statementNode()
}

// PerformStatement represents PERFORM statement
type PerformStatement struct {
	Target  string // Paragraph or section name
	Through string // THRU paragraph
	Times   int    // PERFORM n TIMES
	Until   Expression
	Varying *VaryingClause
	Inline  []Statement // Inline PERFORM
}

func (s *PerformStatement) statementNode() {}

// VaryingClause represents PERFORM VARYING
type VaryingClause struct {
	Variable string
	From     Expression
	By       Expression
	Until    Expression
}

// MoveStatement represents MOVE statement
type MoveStatement struct {
	Source      Expression
	Destination []string
}

func (s *MoveStatement) statementNode() {}

// ComputeStatement represents COMPUTE statement
type ComputeStatement struct {
	Target     string
	Expression Expression
	Rounded    bool
}

func (s *ComputeStatement) statementNode() {}

// AddStatement represents ADD statement
type AddStatement struct {
	Values  []Expression
	To      []string
	Giving  string
	Rounded bool
}

func (s *AddStatement) statementNode() {}

// SubtractStatement represents SUBTRACT statement
type SubtractStatement struct {
	Values  []Expression
	From    []string
	Giving  string
	Rounded bool
}

func (s *SubtractStatement) statementNode() {}

// MultiplyStatement represents MULTIPLY statement
type MultiplyStatement struct {
	Value1  Expression
	By      Expression
	Giving  string
	Rounded bool
}

func (s *MultiplyStatement) statementNode() {}

// DivideStatement represents DIVIDE statement
type DivideStatement struct {
	Value1  Expression
	By      Expression
	Into    string
	Giving  string
	Rounded bool
}

func (s *DivideStatement) statementNode() {}

// DisplayStatement represents DISPLAY statement
type DisplayStatement struct {
	Items []Expression
	Upon  string
}

func (s *DisplayStatement) statementNode() {}

// AcceptStatement represents ACCEPT statement
type AcceptStatement struct {
	Variable string
	From     string
}

func (s *AcceptStatement) statementNode() {}

// IfStatement represents IF statement
type IfStatement struct {
	Condition Expression
	ThenPart  []Statement
	ElsePart  []Statement
}

func (s *IfStatement) statementNode() {}

// EvaluateStatement represents EVALUATE statement
type EvaluateStatement struct {
	Subject Expression
	Cases   []*WhenCase
	Default []Statement
}

func (s *EvaluateStatement) statementNode() {}

// WhenCase represents a WHEN clause in EVALUATE
type WhenCase struct {
	Condition  Expression
	Statements []Statement
}

// OpenStatement represents OPEN statement
type OpenStatement struct {
	Mode  string // INPUT, OUTPUT, I-O, EXTEND
	Files []string
}

func (s *OpenStatement) statementNode() {}

// CloseStatement represents CLOSE statement
type CloseStatement struct {
	Files []string
}

func (s *CloseStatement) statementNode() {}

// ReadStatement represents READ statement
type ReadStatement struct {
	FileName string
	Into     string
	Key      string
	AtEnd    []Statement
	NotAtEnd []Statement
}

func (s *ReadStatement) statementNode() {}

// WriteStatement represents WRITE statement
type WriteStatement struct {
	RecordName string
	From       string
	After      Expression
	Before     Expression
}

func (s *WriteStatement) statementNode() {}

// RewriteStatement represents REWRITE statement
type RewriteStatement struct {
	RecordName string
	From       string
}

func (s *RewriteStatement) statementNode() {}

// DeleteStatement represents DELETE statement
type DeleteStatement struct {
	FileName string
}

func (s *DeleteStatement) statementNode() {}

// StopStatement represents STOP RUN
type StopStatement struct {
	ReturnCode Expression
}

func (s *StopStatement) statementNode() {}

// ExitStatement represents EXIT or EXIT PARAGRAPH/SECTION
type ExitStatement struct {
	Type string // "", "PARAGRAPH", "SECTION", "PROGRAM"
}

func (s *ExitStatement) statementNode() {}

// GoStatement represents GO TO statement
type GoStatement struct {
	Target string
}

func (s *GoStatement) statementNode() {}

// GobackStatement represents GOBACK statement
type GobackStatement struct {
}

func (s *GobackStatement) statementNode() {}

// CallStatement represents CALL statement
type CallStatement struct {
	ProgramName string
	Using       []string
	Returning   string
}

func (s *CallStatement) statementNode() {}

// Expression interface - all expressions implement this
type Expression interface {
	expressionNode()
}

// Identifier represents a variable reference
type Identifier struct {
	Name string
}

func (e *Identifier) expressionNode() {}

// NumberLiteral represents a numeric literal
type NumberLiteral struct {
	Value string
}

func (e *NumberLiteral) expressionNode() {}

// StringLiteral represents a string literal
type StringLiteral struct {
	Value string
}

func (e *StringLiteral) expressionNode() {}

// BinaryExpression represents a binary operation
type BinaryExpression struct {
	Left     Expression
	Operator string // +, -, *, /, =, >, <, >=, <=, <>
	Right    Expression
}

func (e *BinaryExpression) expressionNode() {}

// UnaryExpression represents a unary operation
type UnaryExpression struct {
	Operator string // -, NOT
	Operand  Expression
}

func (e *UnaryExpression) expressionNode() {}

// LogicalExpression represents AND/OR expression
type LogicalExpression struct {
	Left     Expression
	Operator string // AND, OR
	Right    Expression
}

func (e *LogicalExpression) expressionNode() {}
