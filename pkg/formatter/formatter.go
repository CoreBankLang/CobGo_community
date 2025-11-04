package formatter

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

// Options contains formatting options
type Options struct {
	TabWidth int  // Number of spaces per tab
	UseTabs  bool // Use tabs instead of spaces
}

// Formatter formats CobGO DSL code
type Formatter struct {
	options *Options
}

// New creates a new formatter with the given options
func New(options *Options) *Formatter {
	if options == nil {
		options = &Options{
			TabWidth: 4,
			UseTabs:  false,
		}
	}
	return &Formatter{
		options: options,
	}
}

// Format formats the given DSL code
func (f *Formatter) Format(input string) (string, error) {
	// Parse the input to get AST
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(input))
	if err != nil {
		return "", fmt.Errorf("failed to parse input: %w", err)
	}

	// Format the AST
	var buf bytes.Buffer
	if err := f.formatProgram(ast, &buf); err != nil {
		return "", fmt.Errorf("failed to format AST: %w", err)
	}

	return buf.String(), nil
}

// formatProgram formats a program node
func (f *Formatter) formatProgram(program *parser.Program, w io.Writer) error {
	for i, job := range program.Jobs {
		if i > 0 {
			fmt.Fprint(w, "\n")
		}
		if err := f.formatJob(job, w, 0); err != nil {
			return err
		}
	}
	return nil
}

// formatJob formats a job statement
func (f *Formatter) formatJob(job *parser.JobStatement, w io.Writer, indent int) error {
	// Format job declaration
	fmt.Fprintf(w, "%sjob %s {\n", f.indent(indent), job.Name.Value)

	// Format job body
	if job.Body != nil {
		if err := f.formatBlock(job.Body, w, indent+1); err != nil {
			return err
		}
	}

	fmt.Fprintf(w, "%s}\n", f.indent(indent))
	return nil
}

// formatBlock formats a block statement
func (f *Formatter) formatBlock(block *parser.BlockStatement, w io.Writer, indent int) error {
	for i, stmt := range block.Statements {
		if i > 0 {
			fmt.Fprint(w, "\n")
		}
		if err := f.formatStatement(stmt, w, indent); err != nil {
			return err
		}
	}
	return nil
}

// formatStatement formats a statement
func (f *Formatter) formatStatement(stmt parser.Statement, w io.Writer, indent int) error {
	switch s := stmt.(type) {
	case *parser.VarStatement:
		return f.formatVarStatement(s, w, indent)
	case *parser.StepStatement:
		return f.formatStepStatement(s, w, indent)
	case *parser.RecordStatement:
		return f.formatRecordStatement(s, w, indent)
	case *parser.ExpressionStatement:
		return f.formatExpressionStatement(s, w, indent)
	case *parser.ReturnStatement:
		return f.formatReturnStatement(s, w, indent)
	case *parser.DisplayStatement:
		return f.formatDisplayStatement(s, w, indent)
	case *parser.WhileStatement:
		return f.formatWhileStatement(s, w, indent)
	case *parser.BlockStatement:
		return f.formatBlock(s, w, indent)
	default:
		return fmt.Errorf("unsupported statement type: %T", stmt)
	}
}

// formatVarStatement formats a variable declaration
func (f *Formatter) formatVarStatement(stmt *parser.VarStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%svar %s", f.indent(indent), stmt.Name.Value)

	if stmt.Type != nil {
		fmt.Fprintf(w, " %s", f.formatTypeAnnotation(stmt.Type))
	}

	if stmt.Value != nil {
		fmt.Fprintf(w, " = %s", f.formatExpression(stmt.Value))
	}

	fmt.Fprint(w, "\n")
	return nil
}

// formatStepStatement formats a step statement
func (f *Formatter) formatStepStatement(stmt *parser.StepStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%sstep %s", f.indent(indent), stmt.Name.Value)

	// Format parameters
	if len(stmt.Parameters) > 0 {
		fmt.Fprint(w, "(")
		for i, param := range stmt.Parameters {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "%s", param.Value)
		}
		fmt.Fprint(w, ")")
	}

	// Format return type
	if stmt.ReturnType != nil {
		fmt.Fprintf(w, " %s", f.formatTypeAnnotation(stmt.ReturnType))
	}

	fmt.Fprint(w, " {\n")

	// Format step body
	if stmt.Body != nil {
		if err := f.formatBlock(stmt.Body, w, indent+1); err != nil {
			return err
		}
	}

	fmt.Fprintf(w, "%s}\n", f.indent(indent))
	return nil
}

// formatRecordStatement formats a record statement
func (f *Formatter) formatRecordStatement(stmt *parser.RecordStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%srecord %s {\n", f.indent(indent), stmt.Name.Value)

	// Format fields
	for i, field := range stmt.Fields {
		if i > 0 {
			fmt.Fprint(w, "\n")
		}
		fmt.Fprintf(w, "%s%s %s\n", f.indent(indent+1), field.Name.Value, f.formatTypeAnnotation(field.Type))
	}

	fmt.Fprintf(w, "%s}\n", f.indent(indent))
	return nil
}

// formatExpressionStatement formats an expression statement
func (f *Formatter) formatExpressionStatement(stmt *parser.ExpressionStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%s%s\n", f.indent(indent), f.formatExpression(stmt.Expression))
	return nil
}

// formatReturnStatement formats a return statement
func (f *Formatter) formatReturnStatement(stmt *parser.ReturnStatement, w io.Writer, indent int) error {
	if stmt.ReturnValue != nil {
		fmt.Fprintf(w, "%sreturn %s\n", f.indent(indent), f.formatExpression(stmt.ReturnValue))
	} else {
		fmt.Fprintf(w, "%sreturn\n", f.indent(indent))
	}
	return nil
}

// formatDisplayStatement formats a display statement
func (f *Formatter) formatDisplayStatement(stmt *parser.DisplayStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%sdisplay(", f.indent(indent))

	for i, arg := range stmt.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, f.formatExpression(arg))
	}

	fmt.Fprint(w, ")\n")
	return nil
}

// formatWhileStatement formats a while statement
func (f *Formatter) formatWhileStatement(stmt *parser.WhileStatement, w io.Writer, indent int) error {
	fmt.Fprintf(w, "%swhile (%s) {\n", f.indent(indent), f.formatExpression(stmt.Condition))

	// Format while body
	if stmt.Body != nil {
		if err := f.formatBlock(stmt.Body, w, indent+1); err != nil {
			return err
		}
	}

	fmt.Fprintf(w, "%s}\n", f.indent(indent))
	return nil
}

// formatExpression formats an expression
func (f *Formatter) formatExpression(expr parser.Expression) string {
	switch e := expr.(type) {
	case *parser.Identifier:
		return e.Value
	case *parser.IntegerLiteral:
		return fmt.Sprintf("%d", e.Value)
	case *parser.DecimalLiteral:
		return e.Token.Literal
	case *parser.StringLiteral:
		return fmt.Sprintf("\"%s\"", e.Value)
	case *parser.BooleanLiteral:
		if e.Value {
			return "true"
		}
		return "false"
	case *parser.InfixExpression:
		return fmt.Sprintf("%s %s %s",
			f.formatExpression(e.Left),
			e.Operator,
			f.formatExpression(e.Right))
	case *parser.PrefixExpression:
		return fmt.Sprintf("%s%s", e.Operator, f.formatExpression(e.Right))
	case *parser.CallExpression:
		args := make([]string, len(e.Arguments))
		for i, arg := range e.Arguments {
			args[i] = f.formatExpression(arg)
		}
		return fmt.Sprintf("%s(%s)", f.formatExpression(e.Function), strings.Join(args, ", "))
	case *parser.IndexExpression:
		return fmt.Sprintf("%s[%s]",
			f.formatExpression(e.Left),
			f.formatExpression(e.Index))
	default:
		return fmt.Sprintf("/* unsupported expression: %T */", expr)
	}
}

// formatTypeAnnotation formats a type annotation
func (f *Formatter) formatTypeAnnotation(typeAnnotation *parser.TypeAnnotation) string {
	if typeAnnotation == nil {
		return "unknown"
	}

	return typeAnnotation.String()
}

// indent returns the appropriate indentation string
func (f *Formatter) indent(level int) string {
	if f.options.UseTabs {
		return strings.Repeat("\t", level)
	}
	return strings.Repeat(" ", level*f.options.TabWidth)
}
