package ir

import (
	"encoding/json"
	"fmt"
)

// Program represents the intermediate representation of a CobGO program
type Program struct {
	Name        string    `json:"name"`
	Jobs        []*Job    `json:"jobs"`
	Records     []*Record `json:"records"`
	Imports     []string  `json:"imports,omitempty"`
	PackageName string    `json:"package_name"`
}

// Job represents a CobGO job (main program)
type Job struct {
	Name      string      `json:"name"`
	Steps     []*Step     `json:"steps"`
	Variables []*Variable `json:"variables"`
	Body      *Block      `json:"body"`
	Line      int         `json:"line"`
	Column    int         `json:"column"`
}

// Step represents a CobGO step (procedure/function)
type Step struct {
	Name       string       `json:"name"`
	Parameters []*Parameter `json:"parameters"`
	ReturnType *TypeInfo    `json:"return_type,omitempty"`
	Variables  []*Variable  `json:"variables"`
	Body       *Block       `json:"body"`
	Line       int          `json:"line"`
	Column     int          `json:"column"`
}

// Record represents a CobGO record (data structure)
type Record struct {
	Name   string   `json:"name"`
	Fields []*Field `json:"fields"`
	Line   int      `json:"line"`
	Column int      `json:"column"`
}

// Field represents a field in a record
type Field struct {
	Name   string    `json:"name"`
	Type   *TypeInfo `json:"type"`
	Line   int       `json:"line"`
	Column int       `json:"column"`
}

// Variable represents a CobGO variable
type Variable struct {
	Name    string      `json:"name"`
	Type    *TypeInfo   `json:"type"`
	Value   interface{} `json:"value,omitempty"`
	IsConst bool        `json:"is_const"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

// Parameter represents a function parameter
type Parameter struct {
	Name   string    `json:"name"`
	Type   *TypeInfo `json:"type"`
	Line   int       `json:"line"`
	Column int       `json:"column"`
}

// TypeInfo represents type information
type TypeInfo struct {
	Type      string  `json:"type"`
	Size      *int    `json:"size,omitempty"`
	Precision *int    `json:"precision,omitempty"`
	Scale     *int    `json:"scale,omitempty"`
	IsArray   bool    `json:"is_array"`
	ArraySize *int    `json:"array_size,omitempty"`
	IsPointer bool    `json:"is_pointer"`
	GoType    string  `json:"go_type"`
	Record    *Record `json:"record,omitempty"`
	Line      int     `json:"line"`
	Column    int     `json:"column"`
}

// Block represents a block of statements
type Block struct {
	Statements []Statement `json:"statements"`
	Line       int         `json:"line"`
	Column     int         `json:"column"`
}

// Statement represents a CobGO statement
type Statement interface {
	GetType() string
	GetLine() int
	GetColumn() int
}

// ExpressionStatement represents an expression statement
type ExpressionStatement struct {
	Expression Expression `json:"expression"`
	Line       int        `json:"line"`
	Column     int        `json:"column"`
}

func (es *ExpressionStatement) GetType() string { return "expression" }
func (es *ExpressionStatement) GetLine() int    { return es.Line }
func (es *ExpressionStatement) GetColumn() int  { return es.Column }

// VarStatement represents a variable declaration statement
type VarStatement struct {
	Name    string      `json:"name"`
	Type    *TypeInfo   `json:"type"`
	Value   interface{} `json:"value,omitempty"`
	IsConst bool        `json:"is_const"`
	Line    int         `json:"line"`
	Column  int         `json:"column"`
}

func (vs *VarStatement) GetType() string { return "variable" }
func (vs *VarStatement) GetLine() int    { return vs.Line }
func (vs *VarStatement) GetColumn() int  { return vs.Column }

// IfStatement represents an if statement
type IfStatement struct {
	Condition Expression `json:"condition"`
	ThenBlock *Block     `json:"then_block"`
	ElseBlock *Block     `json:"else_block,omitempty"`
	Line      int        `json:"line"`
	Column    int        `json:"column"`
}

func (is *IfStatement) GetType() string { return "if" }
func (is *IfStatement) GetLine() int    { return is.Line }
func (is *IfStatement) GetColumn() int  { return is.Column }

// ForStatement represents a for loop
type ForStatement struct {
	Init      Statement  `json:"init,omitempty"`
	Condition Expression `json:"condition,omitempty"`
	Update    Statement  `json:"update,omitempty"`
	Body      *Block     `json:"body"`
	Line      int        `json:"line"`
	Column    int        `json:"column"`
}

func (fs *ForStatement) GetType() string { return "for" }
func (fs *ForStatement) GetLine() int    { return fs.Line }
func (fs *ForStatement) GetColumn() int  { return fs.Column }

// WhileStatement represents a while loop
type WhileStatement struct {
	Condition Expression `json:"condition"`
	Body      *Block     `json:"body"`
	Line      int        `json:"line"`
	Column    int        `json:"column"`
}

func (ws *WhileStatement) GetType() string { return "while" }
func (ws *WhileStatement) GetLine() int    { return ws.Line }
func (ws *WhileStatement) GetColumn() int  { return ws.Column }

// ReturnStatement represents a return statement
type ReturnStatement struct {
	Value  Expression `json:"value,omitempty"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

func (rs *ReturnStatement) GetType() string { return "return" }
func (rs *ReturnStatement) GetLine() int    { return rs.Line }
func (rs *ReturnStatement) GetColumn() int  { return rs.Column }

// AcceptStatement represents an accept statement
type AcceptStatement struct {
	Prompt Expression `json:"prompt"`
	Target string     `json:"target"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

func (as *AcceptStatement) GetType() string { return "accept" }
func (as *AcceptStatement) GetLine() int    { return as.Line }
func (as *AcceptStatement) GetColumn() int  { return as.Column }

// Expression represents a CobGO expression
type Expression interface {
	GetType() string
	GetLine() int
	GetColumn() int
}

// Identifier represents an identifier expression
type Identifier struct {
	Name   string `json:"name"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
}

func (i *Identifier) GetType() string { return "identifier" }
func (i *Identifier) GetLine() int    { return i.Line }
func (i *Identifier) GetColumn() int  { return i.Column }

// Literal represents a literal expression
type Literal struct {
	Value  interface{} `json:"value"`
	Type   string      `json:"type"`
	Line   int         `json:"line"`
	Column int         `json:"column"`
}

func (l *Literal) GetType() string { return "literal" }
func (l *Literal) GetLine() int    { return l.Line }
func (l *Literal) GetColumn() int  { return l.Column }

// BinaryExpression represents a binary expression
type BinaryExpression struct {
	Left     Expression `json:"left"`
	Operator string     `json:"operator"`
	Right    Expression `json:"right"`
	Line     int        `json:"line"`
	Column   int        `json:"column"`
}

func (be *BinaryExpression) GetType() string { return "binary" }
func (be *BinaryExpression) GetLine() int    { return be.Line }
func (be *BinaryExpression) GetColumn() int  { return be.Column }

// UnaryExpression represents a unary expression
type UnaryExpression struct {
	Operator string     `json:"operator"`
	Operand  Expression `json:"operand"`
	Line     int        `json:"line"`
	Column   int        `json:"column"`
}

func (ue *UnaryExpression) GetType() string { return "unary" }
func (ue *UnaryExpression) GetLine() int    { return ue.Line }
func (ue *UnaryExpression) GetColumn() int  { return ue.Column }

// CallExpression represents a function call
type CallExpression struct {
	Function  Expression   `json:"function"`
	Arguments []Expression `json:"arguments"`
	Line      int          `json:"line"`
	Column    int          `json:"column"`
}

func (ce *CallExpression) GetType() string { return "call" }
func (ce *CallExpression) GetLine() int    { return ce.Line }
func (ce *CallExpression) GetColumn() int  { return ce.Column }

// AssignmentStatement represents an assignment statement
type AssignmentStatement struct {
	Variable string     `json:"variable"`
	Value    Expression `json:"value"`
	Line     int        `json:"line"`
	Column   int        `json:"column"`
}

func (as *AssignmentStatement) GetType() string { return "assignment" }
func (as *AssignmentStatement) GetLine() int    { return as.Line }
func (as *AssignmentStatement) GetColumn() int  { return as.Column }

// DisplayStatement represents a display statement
type DisplayStatement struct {
	Args   []Expression `json:"args"`
	Line   int          `json:"line"`
	Column int          `json:"column"`
}

func (ds *DisplayStatement) GetType() string { return "display" }
func (ds *DisplayStatement) GetLine() int    { return ds.Line }
func (ds *DisplayStatement) GetColumn() int  { return ds.Column }

// ArrayAccess represents array access
type ArrayAccess struct {
	Array  Expression `json:"array"`
	Index  Expression `json:"index"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

func (aa *ArrayAccess) GetType() string { return "array_access" }
func (aa *ArrayAccess) GetLine() int    { return aa.Line }
func (aa *ArrayAccess) GetColumn() int  { return aa.Column }

// LiteralExpression represents a literal expression
type LiteralExpression struct {
	Value  interface{} `json:"value"`
	Type   string      `json:"type"`
	Line   int         `json:"line"`
	Column int         `json:"column"`
}

func (le *LiteralExpression) GetType() string { return "literal" }
func (le *LiteralExpression) GetLine() int    { return le.Line }
func (le *LiteralExpression) GetColumn() int  { return le.Column }

// PrefixExpression represents a prefix expression
type PrefixExpression struct {
	Operator string     `json:"operator"`
	Right    Expression `json:"right"`
	Line     int        `json:"line"`
	Column   int        `json:"column"`
}

func (pe *PrefixExpression) GetType() string { return "prefix" }
func (pe *PrefixExpression) GetLine() int    { return pe.Line }
func (pe *PrefixExpression) GetColumn() int  { return pe.Column }

// IdentifierExpression represents an identifier expression
type IdentifierExpression struct {
	Name   string `json:"name"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
}

func (ie *IdentifierExpression) GetType() string { return "identifier" }
func (ie *IdentifierExpression) GetLine() int    { return ie.Line }
func (ie *IdentifierExpression) GetColumn() int  { return ie.Column }

// NewProgram creates a new IR program
func NewProgram(name string) *Program {
	return &Program{
		Name:        name,
		Jobs:        []*Job{},
		Records:     []*Record{},
		Imports:     []string{},
		PackageName: "main",
	}
}

// ToJSON converts the program to JSON for debugging
func (p *Program) ToJSON() ([]byte, error) {
	return json.MarshalIndent(p, "", "  ")
}

// String returns a string representation of the program
func (p *Program) String() string {
	json, err := p.ToJSON()
	if err != nil {
		return fmt.Sprintf("Error converting to JSON: %v", err)
	}
	return string(json)
}

// AddJob adds a job to the program
func (p *Program) AddJob(job *Job) {
	p.Jobs = append(p.Jobs, job)
}

// AddRecord adds a record to the program
func (p *Program) AddRecord(record *Record) {
	p.Records = append(p.Records, record)
}

// FindRecord finds a record by name
func (p *Program) FindRecord(name string) *Record {
	for _, record := range p.Records {
		if record.Name == name {
			return record
		}
	}
	return nil
}

// FindJob finds a job by name
func (p *Program) FindJob(name string) *Job {
	for _, job := range p.Jobs {
		if job.Name == name {
			return job
		}
	}
	return nil
}

// FindStep finds a step by name in a job
func (j *Job) FindStep(name string) *Step {
	for _, step := range j.Steps {
		if step.Name == name {
			return step
		}
	}
	return nil
}

// FindVariable finds a variable by name in a job
func (j *Job) FindVariable(name string) *Variable {
	for _, variable := range j.Variables {
		if variable.Name == name {
			return variable
		}
	}
	return nil
}

// FindField finds a field by name in a record
func (r *Record) FindField(name string) *Field {
	for _, field := range r.Fields {
		if field.Name == name {
			return field
		}
	}
	return nil
}

// PerformStatement represents a PERFORM statement
type PerformStatement struct {
	Target    string     `json:"target"`
	Condition Expression `json:"condition"`
	Varying   string     `json:"varying"`
	From      Expression `json:"from"`
	To        Expression `json:"to"`
	By        Expression `json:"by"`
	Times     Expression `json:"times"`
	Body      *Block     `json:"body"`
	Line      int        `json:"line"`
	Column    int        `json:"column"`
}

func (ps *PerformStatement) GetType() string { return "perform" }
func (ps *PerformStatement) GetLine() int    { return ps.Line }
func (ps *PerformStatement) GetColumn() int  { return ps.Column }

// EvaluateStatement represents an EVALUATE statement
type EvaluateStatement struct {
	Expression Expression  `json:"expression"`
	Cases      []*WhenCase `json:"cases"`
	Default    *Block      `json:"default"`
	Line       int         `json:"line"`
	Column     int         `json:"column"`
}

func (es *EvaluateStatement) GetType() string { return "evaluate" }
func (es *EvaluateStatement) GetLine() int    { return es.Line }
func (es *EvaluateStatement) GetColumn() int  { return es.Column }

// WhenCase represents a WHEN clause in EVALUATE
type WhenCase struct {
	Value  Expression `json:"value"`
	Body   *Block     `json:"body"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

// MoveStatement represents a MOVE statement
type MoveStatement struct {
	From   Expression `json:"from"`
	To     Expression `json:"to"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

func (ms *MoveStatement) GetType() string { return "move" }
func (ms *MoveStatement) GetLine() int    { return ms.Line }
func (ms *MoveStatement) GetColumn() int  { return ms.Column }

// ComputeStatement represents a COMPUTE statement
type ComputeStatement struct {
	Target Expression `json:"target"`
	Value  Expression `json:"value"`
	Line   int        `json:"line"`
	Column int        `json:"column"`
}

func (cs *ComputeStatement) GetType() string { return "compute" }
func (cs *ComputeStatement) GetLine() int    { return cs.Line }
func (cs *ComputeStatement) GetColumn() int  { return cs.Column }

// FileOperationStatement represents file operations
type FileOperationStatement struct {
	Operation string     `json:"operation"`
	File      string     `json:"file"`
	Record    string     `json:"record"`
	Into      string     `json:"into"`
	From      string     `json:"from"`
	At        Expression `json:"at"`
	Invalid   *Block     `json:"invalid"`
	End       *Block     `json:"end"`
	Line      int        `json:"line"`
	Column    int        `json:"column"`
}

func (fos *FileOperationStatement) GetType() string { return "file_operation" }
func (fos *FileOperationStatement) GetLine() int    { return fos.Line }
func (fos *FileOperationStatement) GetColumn() int  { return fos.Column }

// CopyStatement represents a COPY statement
type CopyStatement struct {
	File   string `json:"file"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
}

func (cs *CopyStatement) GetType() string { return "copy" }
func (cs *CopyStatement) GetLine() int    { return cs.Line }
func (cs *CopyStatement) GetColumn() int  { return cs.Column }

// ===== COBOL-85 Advanced Statements =====

// InspectStatement represents a COBOL INSPECT statement
type InspectStatement struct {
Identifier  string `json:"identifier"`  // The string to inspect
Operation   string `json:"operation"`   // TALLYING, REPLACING, CONVERTING
Counter     string `json:"counter"`     // For TALLYING mode
Mode        string `json:"mode"`        // ALL, LEADING, FIRST, CHARACTERS
Pattern     string `json:"pattern"`     // Pattern to search for
Replacement string `json:"replacement"` // Replacement string (for REPLACING/CONVERTING)
BeforeAfter string `json:"before_after"` // BEFORE or AFTER
Delimiter   string `json:"delimiter"`   // INITIAL delimiter
Line        int    `json:"line"`
Column      int    `json:"column"`
}

func (is *InspectStatement) GetType() string { return "inspect" }
func (is *InspectStatement) GetLine() int    { return is.Line }
func (is *InspectStatement) GetColumn() int  { return is.Column }

// StringStatement represents a COBOL STRING statement
type StringStatement struct {
Sources     []StringSource `json:"sources"`
Destination string         `json:"destination"`
Pointer     string         `json:"pointer"` // Optional pointer variable
OnOverflow  *Block         `json:"on_overflow"` // Optional overflow handler
Line        int            `json:"line"`
Column      int            `json:"column"`
}

// StringSource represents a source in STRING statement
type StringSource struct {
Source    string `json:"source"`
Delimiter string `json:"delimiter"` // DELIMITED BY (literal or SIZE)
}

func (ss *StringStatement) GetType() string { return "string" }
func (ss *StringStatement) GetLine() int    { return ss.Line }
func (ss *StringStatement) GetColumn() int  { return ss.Column }

// UnstringStatement represents a COBOL UNSTRING statement
type UnstringStatement struct {
Source     string   `json:"source"`
Delimiters []string `json:"delimiters"`
Targets    []string `json:"targets"`
Pointer    string   `json:"pointer"` // Optional pointer variable
Tallying   string   `json:"tallying"` // Optional counter variable
OnOverflow *Block   `json:"on_overflow"` // Optional overflow handler
Line       int      `json:"line"`
Column     int      `json:"column"`
}

func (us *UnstringStatement) GetType() string { return "unstring" }
func (us *UnstringStatement) GetLine() int    { return us.Line }
func (us *UnstringStatement) GetColumn() int  { return us.Column }

// SearchStatement represents a COBOL SEARCH statement
type SearchStatement struct {
TableName   string       `json:"table_name"`
IndexName   string       `json:"index_name"` // VARYING clause
AtEnd       *Block       `json:"at_end"` // AT END handler
WhenClauses []SearchWhen `json:"when_clauses"`
IsSearchAll bool         `json:"is_search_all"` // True for SEARCH ALL (binary search)
Line        int          `json:"line"`
Column      int          `json:"column"`
}

// SearchWhen represents a WHEN clause in SEARCH
type SearchWhen struct {
Condition Expression `json:"condition"`
Body      *Block     `json:"body"`
}

func (ss *SearchStatement) GetType() string { return "search" }
func (ss *SearchStatement) GetLine() int    { return ss.Line }
func (ss *SearchStatement) GetColumn() int  { return ss.Column }
