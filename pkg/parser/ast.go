package parser

import "fmt"

// Node represents a node in the Abstract Syntax Tree
type Node interface {
	TokenLiteral() string
	String() string
}

// Statement represents a statement node
type Statement interface {
	Node
	statementNode()
}

// Expression represents an expression node
type Expression interface {
	Node
	expressionNode()
}

// Program represents the root node of the AST
type Program struct {
	Jobs    []*JobStatement
	Records []*RecordStatement
}

func (p *Program) TokenLiteral() string {
	if len(p.Jobs) > 0 {
		return p.Jobs[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out string
	for _, job := range p.Jobs {
		out += job.String()
	}
	return out
}

// JobStatement represents a job declaration
type JobStatement struct {
	Token Token // the 'job' token
	Name  *Identifier
	Body  *BlockStatement
}

func (js *JobStatement) statementNode()       {}
func (js *JobStatement) TokenLiteral() string { return js.Token.Literal }
func (js *JobStatement) String() string {
	return fmt.Sprintf("job %s %s", js.Name.String(), js.Body.String())
}

// StepStatement represents a step declaration
type StepStatement struct {
	Token      Token // the 'step' token
	Name       *Identifier
	Parameters []*Identifier
	ReturnType *TypeAnnotation
	Body       *BlockStatement
}

func (ss *StepStatement) statementNode()       {}
func (ss *StepStatement) TokenLiteral() string { return ss.Token.Literal }
func (ss *StepStatement) String() string {
	var params string
	for i, param := range ss.Parameters {
		if i > 0 {
			params += ", "
		}
		params += param.String()
	}

	var returnType string
	if ss.ReturnType != nil {
		returnType = " " + ss.ReturnType.String()
	}

	return fmt.Sprintf("step %s(%s)%s %s", ss.Name.String(), params, returnType, ss.Body.String())
}

// RecordStatement represents a record declaration
type RecordStatement struct {
	Token  Token // the 'record' token
	Name   *Identifier
	Fields []*FieldDeclaration
}

func (rs *RecordStatement) statementNode()       {}
func (rs *RecordStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *RecordStatement) String() string {
	var fields string
	for i, field := range rs.Fields {
		if i > 0 {
			fields += ", "
		}
		fields += field.String()
	}
	return fmt.Sprintf("record %s { %s }", rs.Name.String(), fields)
}

// FieldDeclaration represents a field in a record
type FieldDeclaration struct {
	Name *Identifier
	Type *TypeAnnotation
}

func (fd *FieldDeclaration) String() string {
	return fmt.Sprintf("%s %s", fd.Name.String(), fd.Type.String())
}

// TypeAnnotation represents a type annotation
type TypeAnnotation struct {
	Type      TokenType
	TypeName  string
	Size      *IntegerLiteral
	Precision *IntegerLiteral
}

func (ta *TypeAnnotation) String() string {
	if ta.Size != nil && ta.Precision != nil {
		return fmt.Sprintf("%s(%d,%d)", ta.TypeName, ta.Size.Value, ta.Precision.Value)
	} else if ta.Size != nil {
		return fmt.Sprintf("%s(%d)", ta.TypeName, ta.Size.Value)
	}
	return ta.TypeName
}

// BlockStatement represents a block of statements
type BlockStatement struct {
	Token      Token // the '{' token
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out string
	for _, s := range bs.Statements {
		out += s.String()
	}
	return out
}

// VarStatement represents a variable declaration
type VarStatement struct {
	Token Token // the 'var' token
	Name  *Identifier
	Type  *TypeAnnotation
	Value Expression
}

func (vs *VarStatement) statementNode()       {}
func (vs *VarStatement) TokenLiteral() string { return vs.Token.Literal }
func (vs *VarStatement) String() string {
	var typeStr string
	if vs.Type != nil {
		typeStr = " " + vs.Type.String()
	}

	var valueStr string
	if vs.Value != nil {
		valueStr = " = " + vs.Value.String()
	}

	return fmt.Sprintf("var %s%s%s", vs.Name.String(), typeStr, valueStr)
}

// ExpressionStatement represents an expression statement
type ExpressionStatement struct {
	Token      Token // the first token of the expression
	Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// IfStatement represents an if statement
type IfStatement struct {
	Token       Token // the 'if' token
	Condition   Expression
	Consequence *BlockStatement
	Alternative Statement
}

func (is *IfStatement) statementNode()       {}
func (is *IfStatement) TokenLiteral() string { return is.Token.Literal }
func (is *IfStatement) String() string {
	var out string
	out += fmt.Sprintf("if %s %s", is.Condition.String(), is.Consequence.String())
	if is.Alternative != nil {
		out += fmt.Sprintf(" else %s", is.Alternative.String())
	}
	return out
}

// ForStatement represents a for loop
type ForStatement struct {
	Token     Token // the 'for' token
	Init      Statement
	Condition Expression
	Update    Statement
	Body      *BlockStatement
}

func (fs *ForStatement) statementNode()       {}
func (fs *ForStatement) TokenLiteral() string { return fs.Token.Literal }
func (fs *ForStatement) String() string {
	return fmt.Sprintf("for %s; %s; %s %s",
		fs.Init.String(), fs.Condition.String(), fs.Update.String(), fs.Body.String())
}

// WhileStatement represents a while loop
type WhileStatement struct {
	Token     Token // the 'while' token
	Condition Expression
	Body      *BlockStatement
}

func (ws *WhileStatement) statementNode()       {}
func (ws *WhileStatement) TokenLiteral() string { return ws.Token.Literal }
func (ws *WhileStatement) String() string {
	return fmt.Sprintf("while %s %s", ws.Condition.String(), ws.Body.String())
}

// ReturnStatement represents a return statement
type ReturnStatement struct {
	Token       Token // the 'return' token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	if rs.ReturnValue != nil {
		return fmt.Sprintf("return %s", rs.ReturnValue.String())
	}
	return "return"
}

// DisplayStatement represents a display statement
type DisplayStatement struct {
	Token Token // the 'display' token
	Args  []Expression
}

func (ds *DisplayStatement) statementNode()       {}
func (ds *DisplayStatement) TokenLiteral() string { return ds.Token.Literal }
func (ds *DisplayStatement) String() string {
	var args string
	for i, arg := range ds.Args {
		if i > 0 {
			args += ", "
		}
		args += arg.String()
	}
	return fmt.Sprintf("display(%s)", args)
}

// AcceptStatement represents an accept statement
type AcceptStatement struct {
	Token  Token // the 'accept' token
	Prompt Expression
	Target *Identifier
}

func (as *AcceptStatement) statementNode()       {}
func (as *AcceptStatement) TokenLiteral() string { return as.Token.Literal }
func (as *AcceptStatement) String() string {
	return fmt.Sprintf("accept(%s, %s)", as.Prompt.String(), as.Target.String())
}

// Identifier represents an identifier
type Identifier struct {
	Token Token // the token.IDENTIFIER token
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

// IntegerLiteral represents an integer literal
type IntegerLiteral struct {
	Token Token
	Value int64
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

// DecimalLiteral represents a decimal literal
type DecimalLiteral struct {
	Token Token
	Value float64
}

func (dl *DecimalLiteral) expressionNode()      {}
func (dl *DecimalLiteral) TokenLiteral() string { return dl.Token.Literal }
func (dl *DecimalLiteral) String() string       { return dl.Token.Literal }

// StringLiteral represents a string literal
type StringLiteral struct {
	Token Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Token Token
	Value bool
}

func (bl *BooleanLiteral) expressionNode()      {}
func (bl *BooleanLiteral) TokenLiteral() string { return bl.Token.Literal }
func (bl *BooleanLiteral) String() string       { return bl.Token.Literal }

// PrefixExpression represents a prefix expression
type PrefixExpression struct {
	Token    Token // the prefix token, e.g. !
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	return fmt.Sprintf("(%s%s)", pe.Operator, pe.Right.String())
}

// InfixExpression represents an infix expression
type InfixExpression struct {
	Token    Token // the operator token, e.g. +
	Left     Expression
	Operator string
	Right    Expression
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	return fmt.Sprintf("(%s %s %s)", ie.Left.String(), ie.Operator, ie.Right.String())
}

// CallExpression represents a function call
type CallExpression struct {
	Token     Token // the '(' token
	Function  Expression
	Arguments []Expression
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CallExpression) String() string {
	var args string
	for i, arg := range ce.Arguments {
		if i > 0 {
			args += ", "
		}
		args += arg.String()
	}
	return fmt.Sprintf("%s(%s)", ce.Function.String(), args)
}

// ArrayLiteral represents an array literal
type ArrayLiteral struct {
	Token    Token // the '[' token
	Elements []Expression
}

func (al *ArrayLiteral) expressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
func (al *ArrayLiteral) String() string {
	var elements string
	for i, el := range al.Elements {
		if i > 0 {
			elements += ", "
		}
		elements += el.String()
	}
	return fmt.Sprintf("[%s]", elements)
}

// IndexExpression represents an array index expression
type IndexExpression struct {
	Token Token // the '[' token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	return fmt.Sprintf("(%s[%s])", ie.Left.String(), ie.Index.String())
}

// PerformStatement represents a PERFORM statement
type PerformStatement struct {
	Token     Token // the 'perform' token
	Target    *Identifier
	Condition Expression
	Varying   *Identifier
	From      Expression
	To        Expression
	By        Expression
	Times     Expression
	Body      *BlockStatement
}

func (ps *PerformStatement) statementNode()       {}
func (ps *PerformStatement) TokenLiteral() string { return ps.Token.Literal }
func (ps *PerformStatement) String() string {
	var out string
	if ps.Target != nil {
		out += fmt.Sprintf("perform %s", ps.Target.String())
	}
	if ps.Condition != nil {
		out += fmt.Sprintf(" until %s", ps.Condition.String())
	}
	if ps.Varying != nil {
		out += fmt.Sprintf(" varying %s", ps.Varying.String())
		if ps.From != nil {
			out += fmt.Sprintf(" from %s", ps.From.String())
		}
		if ps.To != nil {
			out += fmt.Sprintf(" to %s", ps.To.String())
		}
		if ps.By != nil {
			out += fmt.Sprintf(" by %s", ps.By.String())
		}
	}
	if ps.Times != nil {
		out += fmt.Sprintf(" %s times", ps.Times.String())
	}
	if ps.Body != nil {
		out += " " + ps.Body.String()
	}
	return out
}

// EvaluateStatement represents an EVALUATE statement
type EvaluateStatement struct {
	Token      Token // the 'evaluate' token
	Expression Expression
	Cases      []*WhenClause
	Default    *BlockStatement
}

func (es *EvaluateStatement) statementNode()       {}
func (es *EvaluateStatement) TokenLiteral() string { return es.Token.Literal }
func (es *EvaluateStatement) String() string {
	var out string
	out += fmt.Sprintf("evaluate %s", es.Expression.String())
	for _, case_ := range es.Cases {
		out += " " + case_.String()
	}
	if es.Default != nil {
		out += " default " + es.Default.String()
	}
	return out
}

// WhenClause represents a WHEN clause in EVALUATE
type WhenClause struct {
	Token Token // the 'when' token
	Value Expression
	Body  *BlockStatement
}

func (wc *WhenClause) String() string {
	return fmt.Sprintf("when %s %s", wc.Value.String(), wc.Body.String())
}

// MoveStatement represents a MOVE statement
type MoveStatement struct {
	Token Token // the 'move' token
	From  Expression
	To    Expression
}

func (ms *MoveStatement) statementNode()       {}
func (ms *MoveStatement) TokenLiteral() string { return ms.Token.Literal }
func (ms *MoveStatement) String() string {
	return fmt.Sprintf("move %s to %s", ms.From.String(), ms.To.String())
}

// ComputeStatement represents a COMPUTE statement
type ComputeStatement struct {
	Token  Token // the 'compute' token
	Target Expression
	Value  Expression
}

func (cs *ComputeStatement) statementNode()       {}
func (cs *ComputeStatement) TokenLiteral() string { return cs.Token.Literal }
func (cs *ComputeStatement) String() string {
	return fmt.Sprintf("compute %s = %s", cs.Target.String(), cs.Value.String())
}

// FileOperationStatement represents file operations (READ, WRITE, OPEN, CLOSE, etc.)
type FileOperationStatement struct {
	Token     Token // the operation token (READ, WRITE, etc.)
	Operation string
	File      *Identifier
	Record    *Identifier
	Into      *Identifier
	From      *Identifier
	At        Expression
	Invalid   *BlockStatement
	End       *BlockStatement
}

func (fos *FileOperationStatement) statementNode()       {}
func (fos *FileOperationStatement) TokenLiteral() string { return fos.Token.Literal }
func (fos *FileOperationStatement) String() string {
	var out string
	out += fos.Operation
	if fos.File != nil {
		out += " " + fos.File.String()
	}
	if fos.Record != nil {
		out += " " + fos.Record.String()
	}
	if fos.Into != nil {
		out += " into " + fos.Into.String()
	}
	if fos.From != nil {
		out += " from " + fos.From.String()
	}
	if fos.At != nil {
		out += " at " + fos.At.String()
	}
	if fos.Invalid != nil {
		out += " invalid " + fos.Invalid.String()
	}
	if fos.End != nil {
		out += " end " + fos.End.String()
	}
	return out
}

// CopyStatement represents a COPY statement
type CopyStatement struct {
	Token Token // the 'copy' token
	File  *StringLiteral
}

func (cs *CopyStatement) statementNode()       {}
func (cs *CopyStatement) TokenLiteral() string { return cs.Token.Literal }
func (cs *CopyStatement) String() string {
	return fmt.Sprintf("copy %s", cs.File.String())
}
