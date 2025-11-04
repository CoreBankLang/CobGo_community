//go:build ignore
// +build ignore

package translator

import (
	"fmt"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

// DSLGenerator generates CobGO DSL code from translated AST
type DSLGenerator struct {
	indent int
}

// NewDSLGenerator creates a new DSL code generator
func NewDSLGenerator() *DSLGenerator {
	return &DSLGenerator{indent: 0}
}

// Generate generates DSL code from a program AST
func (g *DSLGenerator) Generate(program *parser.Program) string {
	var sb strings.Builder

	for _, job := range program.Jobs {
		sb.WriteString(g.generateJob(job))
		sb.WriteString("\n")
	}

	return sb.String()
}

// generateJob generates code for a job
func (g *DSLGenerator) generateJob(job *parser.Job) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("job %s {\n", job.Name))
	g.indent++

	// Generate file definitions
	for _, file := range job.Files {
		sb.WriteString(g.indentStr())
		sb.WriteString(g.generateFile(file))
		sb.WriteString("\n")
	}

	// Generate record definitions
	for _, record := range job.Records {
		sb.WriteString(g.indentStr())
		sb.WriteString(g.generateRecord(record))
		sb.WriteString("\n")
	}

	// Generate variables
	for _, variable := range job.Variables {
		sb.WriteString(g.indentStr())
		sb.WriteString(g.generateVariable(variable))
		sb.WriteString("\n")
	}

	// Generate steps
	for _, step := range job.Steps {
		sb.WriteString("\n")
		sb.WriteString(g.indentStr())
		sb.WriteString(g.generateStep(step))
		sb.WriteString("\n")
	}

	g.indent--
	sb.WriteString("}\n")

	return sb.String()
}

// generateFile generates code for a file definition
func (g *DSLGenerator) generateFile(file *parser.FileDef) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("file %s {\n", file.Name))
	g.indent++

	if file.Assign != "" {
		sb.WriteString(g.indentStr())
		sb.WriteString(fmt.Sprintf("assign: %q\n", file.Assign))
	}
	if file.Organization != "" {
		sb.WriteString(g.indentStr())
		sb.WriteString(fmt.Sprintf("organization: %s\n", file.Organization))
	}
	if file.AccessMode != "" {
		sb.WriteString(g.indentStr())
		sb.WriteString(fmt.Sprintf("access: %s\n", file.AccessMode))
	}
	if file.FileStatus != "" {
		sb.WriteString(g.indentStr())
		sb.WriteString(fmt.Sprintf("file_status: %s\n", file.FileStatus))
	}

	g.indent--
	sb.WriteString(g.indentStr())
	sb.WriteString("}")

	return sb.String()
}

// generateRecord generates code for a record definition
func (g *DSLGenerator) generateRecord(record *parser.RecordDef) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("record %s {\n", record.Name))
	g.indent++

	for _, field := range record.Fields {
		sb.WriteString(g.indentStr())
		if field.IsArray {
			sb.WriteString(fmt.Sprintf("%s %s[%d]\n", field.Name, field.Type, field.ArraySize))
		} else {
			sb.WriteString(fmt.Sprintf("%s %s\n", field.Name, field.Type))
		}
	}

	g.indent--
	sb.WriteString(g.indentStr())
	sb.WriteString("}")

	return sb.String()
}

// generateVariable generates code for a variable
func (g *DSLGenerator) generateVariable(variable *parser.Variable) string {
	if variable.InitValue != "" {
		return fmt.Sprintf("var %s %s = %s", variable.Name, variable.Type, variable.InitValue)
	}
	return fmt.Sprintf("var %s %s", variable.Name, variable.Type)
}

// generateStep generates code for a step
func (g *DSLGenerator) generateStep(step *parser.Step) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("step %s() {\n", step.Name))
	g.indent++

	for _, stmt := range step.Statements {
		sb.WriteString(g.indentStr())
		sb.WriteString(g.generateStatement(stmt))
		sb.WriteString("\n")
	}

	g.indent--
	sb.WriteString(g.indentStr())
	sb.WriteString("}")

	return sb.String()
}

// generateStatement generates code for a statement
func (g *DSLGenerator) generateStatement(stmt parser.Statement) string {
	switch s := stmt.(type) {
	case *parser.CallStepStatement:
		return fmt.Sprintf("step %s()", s.StepName)
	case *parser.AssignmentStatement:
		return fmt.Sprintf("%s = %s", s.Target, g.generateExpression(s.Value))
	case *parser.DisplayStatement:
		items := []string{}
		for _, item := range s.Items {
			items = append(items, g.generateExpression(item))
		}
		return fmt.Sprintf("display(%s)", strings.Join(items, ", "))
	case *parser.OpenFileStatement:
		return fmt.Sprintf("open %s %s", s.FileName, s.Mode)
	case *parser.CloseFileStatement:
		return fmt.Sprintf("close %s", s.FileName)
	case *parser.ReadFileStatement:
		return fmt.Sprintf("read %s into %s", s.FileName, s.Into)
	case *parser.WriteFileStatement:
		if s.From != "" {
			return fmt.Sprintf("write %s from %s", s.RecordName, s.From)
		}
		return fmt.Sprintf("write %s", s.RecordName)
	case *parser.ExitStatement:
		return "exit"
	case *parser.CommentStatement:
		return fmt.Sprintf("// %s", s.Text)
	default:
		return "// Unknown statement"
	}
}

// generateExpression generates code for an expression
func (g *DSLGenerator) generateExpression(expr parser.Expression) string {
	switch e := expr.(type) {
	case *parser.IdentifierExpr:
		return e.Name
	case *parser.NumberLiteralExpr:
		return e.Value
	case *parser.StringLiteralExpr:
		return fmt.Sprintf("%q", e.Value)
	default:
		return ""
	}
}

// indentStr returns the current indentation string
func (g *DSLGenerator) indentStr() string {
	return strings.Repeat("    ", g.indent)
}
