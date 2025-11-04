package codegen

import (
	"fmt"
	"io"
	"strings"
	"text/template"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

// Generator represents a Go code generator
type Generator struct {
	PackageName   string
	Imports       []string
	VariableTypes map[string]string // Maps variable names to their Go types
}

// New creates a new code generator
func New() *Generator {
	return &Generator{
		PackageName: "main",
		Imports: []string{
			"fmt",
			"os",
			"log",
		},
		VariableTypes: make(map[string]string),
	}
}

// Generate generates Go code from IR
func (g *Generator) Generate(program *ir.Program, writer io.Writer) error {
	if program == nil {
		return fmt.Errorf("program cannot be nil")
	}

	// Collect variable types first (needed for import generation)
	g.collectVariableTypes(program)

	// Generate package header
	if err := g.generatePackageHeader(writer); err != nil {
		return err
	}

	// Generate imports
	if err := g.generateImports(writer); err != nil {
		return err
	}

	// Generate record structs
	if err := g.generateRecordStructs(program, writer); err != nil {
		return err
	}

	// Generate global variables
	if err := g.generateGlobalVariables(program, writer); err != nil {
		return err
	}

	// Generate job functions
	if err := g.generateJobFunctions(program, writer); err != nil {
		return err
	}

	// Generate main function
	if err := g.generateMainFunction(program, writer); err != nil {
		return err
	}

	return nil
}

// collectVariableTypes collects variable types from the program
func (g *Generator) collectVariableTypes(program *ir.Program) {
	for _, job := range program.Jobs {
		for _, variable := range job.Variables {
			goType := g.getGoType(variable.Type)
			g.VariableTypes[variable.Name] = goType
		}
	}
}

// generatePackageHeader generates the package declaration
func (g *Generator) generatePackageHeader(writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "package %s\n\n", g.PackageName)
	return err
}

// generateImports generates the import statements
func (g *Generator) generateImports(writer io.Writer) error {
	// Only include necessary imports
	imports := []string{"fmt", "log"}

	// Check if decimal types are used
	needsDecimal := false
	for _, varType := range g.VariableTypes {
		if varType == "*decimal.Decimal" {
			needsDecimal = true
			break
		}
	}

	_, err := fmt.Fprintf(writer, "import (\n")
	if err != nil {
		return err
	}

	for _, imp := range imports {
		_, err = fmt.Fprintf(writer, "\t\"%s\"\n", imp)
		if err != nil {
			return err
		}
	}

	// Add decimal import if needed
	if needsDecimal {
		_, err = fmt.Fprintf(writer, "\t\"github.com/cobgo/cobgo-community/pkg/decimal\"\n")
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, ")\n\n")
	return err
}

// generateRecordStructs generates Go structs from IR records
func (g *Generator) generateRecordStructs(program *ir.Program, writer io.Writer) error {
	for _, record := range program.Records {
		if err := g.generateRecordStruct(record, writer); err != nil {
			return err
		}
	}
	return nil
}

// generateGlobalVariables generates global variables from job variables
func (g *Generator) generateGlobalVariables(program *ir.Program, writer io.Writer) error {
	// Collect all variables from all jobs
	for _, job := range program.Jobs {
		for _, variable := range job.Variables {
			goType := g.getGoType(variable.Type)

			if variable.Value != nil {
				// Variable with initial value
				if goType == "*decimal.Decimal" {
					_, err := fmt.Fprintf(writer, "var %s %s\n", variable.Name, goType)
					if err != nil {
						return err
					}
					_, err = fmt.Fprintf(writer, "func init() {\n")
					if err != nil {
						return err
					}
					_, err = fmt.Fprintf(writer, "\tif temp, err := decimal.NewFromString(\"%v\"); err == nil {\n", variable.Value)
					if err != nil {
						return err
					}
					_, err = fmt.Fprintf(writer, "\t\t%s = temp\n", variable.Name)
					if err != nil {
						return err
					}
					_, err = fmt.Fprintf(writer, "\t}\n")
					if err != nil {
						return err
					}
					_, err = fmt.Fprintf(writer, "}\n")
					if err != nil {
						return err
					}
				} else {
					// Use proper formatting based on type
					var formatStr string
					if goType == "string" {
						formatStr = "var %s %s = %q\n"
					} else {
						formatStr = "var %s %s = %v\n"
					}
					_, err := fmt.Fprintf(writer, formatStr, variable.Name, goType, variable.Value)
					if err != nil {
						return err
					}
				}
			} else {
				// Variable without initial value
				_, err := fmt.Fprintf(writer, "var %s %s\n", variable.Name, goType)
				if err != nil {
					return err
				}
			}
		}
	}

	if len(program.Jobs) > 0 && len(program.Jobs[0].Variables) > 0 {
		_, err := fmt.Fprintf(writer, "\n")
		return err
	}

	return nil
}

// generateRecordStruct generates a single record struct
func (g *Generator) generateRecordStruct(record *ir.Record, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "// %s represents a record definition\n", record.Name)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "type %s struct {\n", record.Name)
	if err != nil {
		return err
	}

	for _, field := range record.Fields {
		goType := g.getGoType(field.Type)
		_, err = fmt.Fprintf(writer, "\t%s %s `json:\"%s\"`\n",
			strings.Title(field.Name), goType, field.Name)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "}\n\n")
	return err
}

// generateJobFunctions generates Go functions from IR jobs
func (g *Generator) generateJobFunctions(program *ir.Program, writer io.Writer) error {
	for _, job := range program.Jobs {
		if err := g.generateJobFunction(job, writer); err != nil {
			return err
		}
	}
	return nil
}

// generateJobFunction generates a single job function
func (g *Generator) generateJobFunction(job *ir.Job, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "// %s executes the %s job\n", job.Name, job.Name)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "func %s() error {\n", job.Name)
	if err != nil {
		return err
	}

	// Generate job body statements (skip variable declarations as they're now global)
	if job.Body != nil {
		if err := g.generateBlockSkipVars(job.Body, writer); err != nil {
			return err
		}
	}

	// Generate step calls
	for _, step := range job.Steps {
		_, err = fmt.Fprintf(writer, "\tif err := %s_%s(); err != nil {\n", job.Name, step.Name)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t\treturn fmt.Errorf(\"step %s failed: %%w\", err)\n", step.Name)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t}\n")
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\treturn nil\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "}\n\n")
	if err != nil {
		return err
	}

	// Generate step functions
	for _, step := range job.Steps {
		if err := g.generateStepFunction(job.Name, step, writer); err != nil {
			return err
		}
	}

	return nil
}

// generateStepFunction generates a step function
func (g *Generator) generateStepFunction(jobName string, step *ir.Step, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "// %s_%s executes the %s step\n", jobName, step.Name, step.Name)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "func %s_%s() error {\n", jobName, step.Name)
	if err != nil {
		return err
	}

	// Generate step body
	if step.Body != nil {
		if err := g.generateBlock(step.Body, writer); err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\treturn nil\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "}\n\n")
	return err
}

// generateBlock generates a block of statements
func (g *Generator) generateBlock(block *ir.Block, writer io.Writer) error {
	for _, stmt := range block.Statements {
		if err := g.generateStatement(stmt, writer); err != nil {
			return err
		}
	}
	return nil
}

// generateBlockSkipVars generates a block of statements but skips variable declarations
func (g *Generator) generateBlockSkipVars(block *ir.Block, writer io.Writer) error {
	for _, stmt := range block.Statements {
		// Skip variable declarations as they're handled globally
		if _, ok := stmt.(*ir.VarStatement); ok {
			continue
		}
		if err := g.generateStatement(stmt, writer); err != nil {
			return err
		}
	}
	return nil
}

// generateStatement generates a single statement
func (g *Generator) generateStatement(stmt ir.Statement, writer io.Writer) error {
	switch s := stmt.(type) {
	case *ir.ExpressionStatement:
		return g.generateExpressionStatement(s, writer)
	case *ir.VarStatement:
		return g.generateVarStatement(s, writer)
	case *ir.AssignmentStatement:
		return g.generateAssignmentStatement(s, writer)
	case *ir.DisplayStatement:
		return g.generateDisplayStatement(s, writer)
	case *ir.IfStatement:
		return g.generateIfStatement(s, writer)
	case *ir.ForStatement:
		return g.generateForStatement(s, writer)
	case *ir.WhileStatement:
		return g.generateWhileStatement(s, writer)
	case *ir.ReturnStatement:
		return g.generateReturnStatement(s, writer)
	case *ir.PerformStatement:
		return g.generatePerformStatement(s, writer)
	case *ir.EvaluateStatement:
		return g.generateEvaluateStatement(s, writer)
	case *ir.MoveStatement:
		return g.generateMoveStatement(s, writer)
	case *ir.ComputeStatement:
		return g.generateComputeStatement(s, writer)
	case *ir.FileOperationStatement:
		return g.generateFileOperationStatement(s, writer)
	case *ir.CopyStatement:
		return g.generateCopyStatement(s, writer)
	case *ir.InspectStatement:
		return g.generateInspectStatement(s, writer)
	case *ir.StringStatement:
		return g.generateStringStatement(s, writer)
	case *ir.UnstringStatement:
		return g.generateUnstringStatement(s, writer)
	case *ir.SearchStatement:
		return g.generateSearchStatement(s, writer)
	default:
		_, err := fmt.Fprintf(writer, "\t// Unknown statement type: %T\n", stmt)
		return err
	}
}

// generateExpressionStatement generates an expression statement
func (g *Generator) generateExpressionStatement(stmt *ir.ExpressionStatement, writer io.Writer) error {
	expr, err := g.generateExpression(stmt.Expression)
	if err != nil {
		return err
	}
	_, err = fmt.Fprintf(writer, "\t%s\n", expr)
	return err
}

// generateVarStatement generates a variable declaration statement
func (g *Generator) generateVarStatement(stmt *ir.VarStatement, writer io.Writer) error {
	goType := g.getGoType(stmt.Type)

	if stmt.Value != nil {
		// Variable with initial value
		if goType == "*decimal.Decimal" {
			_, err := fmt.Fprintf(writer, "\tvar %s %s\n", stmt.Name, goType)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\tif temp, err := decimal.NewFromString(\"%v\"); err == nil {\n", stmt.Value)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t%s = temp\n", stmt.Name)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t}\n")
			return err
		} else {
			_, err := fmt.Fprintf(writer, "\tvar %s %s = %v\n", stmt.Name, goType, stmt.Value)
			return err
		}
	} else {
		// Variable without initial value
		_, err := fmt.Fprintf(writer, "\tvar %s %s\n", stmt.Name, goType)
		return err
	}
}

// generateAssignmentStatement generates an assignment statement
func (g *Generator) generateAssignmentStatement(stmt *ir.AssignmentStatement, writer io.Writer) error {
	value, err := g.generateExpression(stmt.Value)
	if err != nil {
		return err
	}
	_, err = fmt.Fprintf(writer, "\t%s = %s\n", stmt.Variable, value)
	return err
}

// generateDisplayStatement generates a display statement
func (g *Generator) generateDisplayStatement(stmt *ir.DisplayStatement, writer io.Writer) error {
	if len(stmt.Args) == 0 {
		_, err := fmt.Fprintf(writer, "\tfmt.Println()\n")
		return err
	}

	var args []string
	for _, arg := range stmt.Args {
		argStr, err := g.generateExpression(arg)
		if err != nil {
			return err
		}
		args = append(args, argStr)
	}

	_, err := fmt.Fprintf(writer, "\tfmt.Println(%s)\n", strings.Join(args, ", "))
	return err
}

// generateExpression generates an expression
func (g *Generator) generateExpression(expr ir.Expression) (string, error) {
	switch e := expr.(type) {
	case *ir.LiteralExpression:
		return g.generateLiteralExpression(e)
	case *ir.BinaryExpression:
		return g.generateBinaryExpression(e)
	case *ir.PrefixExpression:
		return g.generatePrefixExpression(e)
	case *ir.IdentifierExpression:
		return g.generateIdentifierExpression(e)
	default:
		return "", fmt.Errorf("unknown expression type: %T", expr)
	}
}

// generateLiteralExpression generates a literal expression
func (g *Generator) generateLiteralExpression(expr *ir.LiteralExpression) (string, error) {
	switch expr.Type {
	case "string":
		return fmt.Sprintf("\"%s\"", expr.Value), nil
	case "int32":
		return fmt.Sprintf("%v", expr.Value), nil
	case "int64":
		return fmt.Sprintf("%v", expr.Value), nil
	case "decimal":
		// For decimal literals, we need to handle the error properly
		// We'll create a temporary variable to hold the result
		return fmt.Sprintf("func() *decimal.Decimal { result, err := decimal.NewFromString(\"%v\"); if err != nil { return nil }; return result }()", expr.Value), nil
	default:
		return "", fmt.Errorf("unknown literal type: %s", expr.Type)
	}
}

// generateBinaryExpression generates a binary expression
func (g *Generator) generateBinaryExpression(expr *ir.BinaryExpression) (string, error) {
	left, err := g.generateExpression(expr.Left)
	if err != nil {
		return "", err
	}
	right, err := g.generateExpression(expr.Right)
	if err != nil {
		return "", err
	}

	op := expr.Operator

	// Check if we're dealing with decimal types
	leftType := g.getExpressionType(expr.Left)
	rightType := g.getExpressionType(expr.Right)

	// If either operand is a decimal type, use decimal methods
	if leftType == "*decimal.Decimal" || rightType == "*decimal.Decimal" {
		switch op {
		case "+":
			return fmt.Sprintf("(%s.Add(%s))", left, right), nil
		case "-":
			return fmt.Sprintf("(%s.Sub(%s))", left, right), nil
		case "*":
			return fmt.Sprintf("(%s.Mul(%s))", left, right), nil
		case "/":
			return fmt.Sprintf("func() *decimal.Decimal { result, err := %s.Div(%s); if err != nil { return nil }; return result }()", left, right), nil
		case "==":
			return fmt.Sprintf("(%s.String() == %s.String())", left, right), nil
		case "!=":
			return fmt.Sprintf("(%s.String() != %s.String())", left, right), nil
		case "<":
			return fmt.Sprintf("(%s.String() < %s.String())", left, right), nil
		case ">":
			return fmt.Sprintf("(%s.String() > %s.String())", left, right), nil
		case "<=":
			return fmt.Sprintf("(%s.String() <= %s.String())", left, right), nil
		case ">=":
			return fmt.Sprintf("(%s.String() >= %s.String())", left, right), nil
		case "=":
			return fmt.Sprintf("%s = %s", left, right), nil
		default:
			return "", fmt.Errorf("unknown binary operator for decimal: %s", op)
		}
	}

	// For non-decimal types, use regular Go operators
	switch op {
	case "+":
		return fmt.Sprintf("(%s + %s)", left, right), nil
	case "-":
		return fmt.Sprintf("(%s - %s)", left, right), nil
	case "*":
		return fmt.Sprintf("(%s * %s)", left, right), nil
	case "/":
		return fmt.Sprintf("(%s / %s)", left, right), nil
	case "==":
		return fmt.Sprintf("(%s == %s)", left, right), nil
	case "!=":
		return fmt.Sprintf("(%s != %s)", left, right), nil
	case "<":
		return fmt.Sprintf("(%s < %s)", left, right), nil
	case ">":
		return fmt.Sprintf("(%s > %s)", left, right), nil
	case "<=":
		return fmt.Sprintf("(%s <= %s)", left, right), nil
	case ">=":
		return fmt.Sprintf("(%s >= %s)", left, right), nil
	case "=":
		return fmt.Sprintf("%s = %s", left, right), nil
	default:
		return "", fmt.Errorf("unknown binary operator: %s", op)
	}
}

// generatePrefixExpression generates a prefix expression
func (g *Generator) generatePrefixExpression(expr *ir.PrefixExpression) (string, error) {
	right, err := g.generateExpression(expr.Right)
	if err != nil {
		return "", err
	}

	switch expr.Operator {
	case "!":
		return fmt.Sprintf("!(%s)", right), nil
	case "-":
		return fmt.Sprintf("-(%s)", right), nil
	default:
		return "", fmt.Errorf("unknown prefix operator: %s", expr.Operator)
	}
}

// generateIdentifierExpression generates an identifier expression
func (g *Generator) generateIdentifierExpression(expr *ir.IdentifierExpression) (string, error) {
	return expr.Name, nil
}

// getExpressionType determines the Go type of an expression
func (g *Generator) getExpressionType(expr ir.Expression) string {
	switch e := expr.(type) {
	case *ir.LiteralExpression:
		switch e.Type {
		case "string":
			return "string"
		case "int32", "int64":
			return "int64"
		case "decimal":
			return "*decimal.Decimal"
		default:
			return "interface{}"
		}
	case *ir.IdentifierExpression:
		// Look up the variable type from our type map
		if varType, exists := g.VariableTypes[e.Name]; exists {
			return varType
		}
		// Default to decimal for unknown variables
		return "*decimal.Decimal"
	case *ir.BinaryExpression:
		// For binary expressions, return the type of the left operand
		// In a more sophisticated implementation, we'd do proper type inference
		return g.getExpressionType(e.Left)
	default:
		return "interface{}"
	}
}

// generateIfStatement generates an if statement
func (g *Generator) generateIfStatement(stmt *ir.IfStatement, writer io.Writer) error {
	condition, err := g.generateExpression(stmt.Condition)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tif %s {\n", condition)
	if err != nil {
		return err
	}

	// Generate then block
	if stmt.ThenBlock != nil {
		if err := g.generateBlock(stmt.ThenBlock, writer); err != nil {
			return err
		}
	}

	// Generate else block if present
	if stmt.ElseBlock != nil {
		_, err = fmt.Fprintf(writer, "\t} else {\n")
		if err != nil {
			return err
		}
		if err := g.generateBlock(stmt.ElseBlock, writer); err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateForStatement generates a for statement
func (g *Generator) generateForStatement(stmt *ir.ForStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\tfor ")
	if err != nil {
		return err
	}

	// Generate init statement
	if stmt.Init != nil {
		initCode, err := g.generateStatementCode(stmt.Init)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "%s", initCode)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "; ")
	if err != nil {
		return err
	}

	// Generate condition
	if stmt.Condition != nil {
		condition, err := g.generateExpression(stmt.Condition)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "%s", condition)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "; ")
	if err != nil {
		return err
	}

	// Generate update statement
	if stmt.Update != nil {
		updateCode, err := g.generateStatementCode(stmt.Update)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "%s", updateCode)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, " {\n")
	if err != nil {
		return err
	}

	// Generate body
	if stmt.Body != nil {
		if err := g.generateBlock(stmt.Body, writer); err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateWhileStatement generates a while statement
func (g *Generator) generateWhileStatement(stmt *ir.WhileStatement, writer io.Writer) error {
	condition, err := g.generateExpression(stmt.Condition)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tfor %s {\n", condition)
	if err != nil {
		return err
	}

	// Generate body
	if stmt.Body != nil {
		if err := g.generateBlock(stmt.Body, writer); err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateReturnStatement generates a return statement
func (g *Generator) generateReturnStatement(stmt *ir.ReturnStatement, writer io.Writer) error {
	if stmt.Value != nil {
		value, err := g.generateExpression(stmt.Value)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\treturn %s\n", value)
		return err
	}
	_, err := fmt.Fprintf(writer, "\treturn\n")
	return err
}

// generateStatementCode generates statement code without indentation (for use in for loops)
func (g *Generator) generateStatementCode(stmt ir.Statement) (string, error) {
	switch s := stmt.(type) {
	case *ir.AssignmentStatement:
		value, err := g.generateExpression(s.Value)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s = %s", s.Variable, value), nil
	case *ir.ExpressionStatement:
		return g.generateExpression(s.Expression)
	default:
		return "", fmt.Errorf("unsupported statement type in for loop: %T", stmt)
	}
}

// generateMainFunction generates the main function
func (g *Generator) generateMainFunction(program *ir.Program, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "func main() {\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tlog.SetFlags(log.LstdFlags | log.Lshortfile)\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tfmt.Println(\"Starting CobGO program\")\n")
	if err != nil {
		return err
	}

	// Generate job calls
	for _, job := range program.Jobs {
		_, err = fmt.Fprintf(writer, "\tif err := %s(); err != nil {\n", job.Name)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t\tlog.Fatalf(\"Job %s failed: %%v\", err)\n", job.Name)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t}\n")
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\tfmt.Println(\"Program completed successfully\")\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "}\n")
	return err
}

// getGoType converts IR type to Go type
func (g *Generator) getGoType(typeInfo *ir.TypeInfo) string {
	if typeInfo == nil {
		return "interface{}"
	}

	switch typeInfo.Type {
	case "int32":
		return "int32"
	case "int64":
		return "int64"
	case "string":
		if typeInfo.Size != nil {
			return "string"
		}
		return "string"
	case "decimal":
		return "*decimal.Decimal"
	case "record":
		if typeInfo.Record != nil {
			return typeInfo.Record.Name
		}
		return "interface{}"
	default:
		return "interface{}"
	}
}

// TemplateData represents data for code generation templates
type TemplateData struct {
	PackageName string
	Imports     []string
	Functions   []Function
	Variables   []Variable
}

// Function represents a Go function
type Function struct {
	Name       string
	Parameters []Parameter
	ReturnType string
	Body       string
}

// Parameter represents a function parameter
type Parameter struct {
	Name string
	Type string
}

// Variable represents a Go variable
type Variable struct {
	Name  string
	Type  string
	Value string
}

// GenerateFromTemplate generates code using Go templates
func (g *Generator) GenerateFromTemplate(data TemplateData, writer io.Writer) error {
	tmpl := `package {{.PackageName}}

import (
{{range .Imports}}
	"{{.}}"
{{end}}
)

{{range .Variables}}
var {{.Name}} {{.Type}} = {{.Value}}
{{end}}

{{range .Functions}}
func {{.Name}}({{range $i, $p := .Parameters}}{{if $i}}, {{end}}{{$p.Name}} {{$p.Type}}{{end}}) {{.ReturnType}} {
{{.Body}}
}
{{end}}
`

	t, err := template.New("codegen").Parse(tmpl)
	if err != nil {
		return fmt.Errorf("failed to parse template: %w", err)
	}

	return t.Execute(writer, data)
}

// generatePerformStatement generates a PERFORM statement
func (g *Generator) generatePerformStatement(stmt *ir.PerformStatement, writer io.Writer) error {
	if stmt.Target != "" {
		// PERFORM target UNTIL condition
		if stmt.Condition != nil {
			condition, err := g.generateExpression(stmt.Condition)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\tfor !(%s) {\n", condition)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t%s()\n", stmt.Target)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t}\n")
			return err
		}

		// PERFORM target VARYING
		if stmt.Varying != "" {
			_, err := fmt.Fprintf(writer, "\tfor %s := ", stmt.Varying)
			if err != nil {
				return err
			}

			if stmt.From != nil {
				from, err := g.generateExpression(stmt.From)
				if err != nil {
					return err
				}
				_, err = fmt.Fprintf(writer, "%s; ", from)
				if err != nil {
					return err
				}
			}

			if stmt.To != nil {
				to, err := g.generateExpression(stmt.To)
				if err != nil {
					return err
				}
				_, err = fmt.Fprintf(writer, "%s <= %s; ", stmt.Varying, to)
				if err != nil {
					return err
				}
			}

			if stmt.By != nil {
				by, err := g.generateExpression(stmt.By)
				if err != nil {
					return err
				}
				_, err = fmt.Fprintf(writer, "%s += %s", stmt.Varying, by)
				if err != nil {
					return err
				}
			} else {
				_, err = fmt.Fprintf(writer, "%s++", stmt.Varying)
				if err != nil {
					return err
				}
			}

			_, err = fmt.Fprintf(writer, " {\n")
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t%s()\n", stmt.Target)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t}\n")
			return err
		}

		// PERFORM target TIMES
		if stmt.Times != nil {
			times, err := g.generateExpression(stmt.Times)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\tfor i := 0; i < %s; i++ {\n", times)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t%s()\n", stmt.Target)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t}\n")
			return err
		}

		// Simple PERFORM target
		_, err := fmt.Fprintf(writer, "\t%s()\n", stmt.Target)
		return err
	}

	// PERFORM with inline body
	if stmt.Body != nil {
		_, err := fmt.Fprintf(writer, "\tfor {\n")
		if err != nil {
			return err
		}
		if err := g.generateBlock(stmt.Body, writer); err != nil {
			return err
		}
		if stmt.Condition != nil {
			condition, err := g.generateExpression(stmt.Condition)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\tif %s {\n", condition)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t\tbreak\n")
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t}\n")
			if err != nil {
				return err
			}
		}
		_, err = fmt.Fprintf(writer, "\t}\n")
		return err
	}

	return nil
}

// generateEvaluateStatement generates an EVALUATE statement
func (g *Generator) generateEvaluateStatement(stmt *ir.EvaluateStatement, writer io.Writer) error {
	expression, err := g.generateExpression(stmt.Expression)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tswitch %s {\n", expression)
	if err != nil {
		return err
	}

	// Generate WHEN cases
	for _, whenCase := range stmt.Cases {
		value, err := g.generateExpression(whenCase.Value)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\tcase %s:\n", value)
		if err != nil {
			return err
		}
		if whenCase.Body != nil {
			if err := g.generateBlock(whenCase.Body, writer); err != nil {
				return err
			}
		}
	}

	// Generate DEFAULT case
	if stmt.Default != nil {
		_, err = fmt.Fprintf(writer, "\tdefault:\n")
		if err != nil {
			return err
		}
		if err := g.generateBlock(stmt.Default, writer); err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateMoveStatement generates a MOVE statement
func (g *Generator) generateMoveStatement(stmt *ir.MoveStatement, writer io.Writer) error {
	from, err := g.generateExpression(stmt.From)
	if err != nil {
		return err
	}
	to, err := g.generateExpression(stmt.To)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t%s = %s\n", to, from)
	return err
}

// generateComputeStatement generates a COMPUTE statement
func (g *Generator) generateComputeStatement(stmt *ir.ComputeStatement, writer io.Writer) error {
	target, err := g.generateExpression(stmt.Target)
	if err != nil {
		return err
	}
	value, err := g.generateExpression(stmt.Value)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t%s = %s\n", target, value)
	return err
}

// generateFileOperationStatement generates file operation statements
func (g *Generator) generateFileOperationStatement(stmt *ir.FileOperationStatement, writer io.Writer) error {
	switch stmt.Operation {
	case "open":
		_, err := fmt.Fprintf(writer, "\t// Open file: %s\n", stmt.File)
		return err
	case "close":
		_, err := fmt.Fprintf(writer, "\t// Close file: %s\n", stmt.File)
		return err
	case "read":
		_, err := fmt.Fprintf(writer, "\t// Read from file: %s into %s\n", stmt.File, stmt.Into)
		return err
	case "write":
		_, err := fmt.Fprintf(writer, "\t// Write to file: %s from %s\n", stmt.File, stmt.From)
		return err
	case "rewrite":
		_, err := fmt.Fprintf(writer, "\t// Rewrite file: %s\n", stmt.File)
		return err
	case "delete":
		_, err := fmt.Fprintf(writer, "\t// Delete from file: %s\n", stmt.File)
		return err
	default:
		_, err := fmt.Fprintf(writer, "\t// File operation: %s on %s\n", stmt.Operation, stmt.File)
		return err
	}
}

// generateCopyStatement generates a COPY statement
func (g *Generator) generateCopyStatement(stmt *ir.CopyStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\t// Copy file: %s\n", stmt.File)
	return err
}
