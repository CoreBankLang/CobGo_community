package codegen

import (
	"bytes"
	"strings"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

func TestNew(t *testing.T) {
	gen := New()
	if gen == nil {
		t.Fatal("Expected generator to be created")
	}
	if gen.PackageName != "main" {
		t.Errorf("Expected package name 'main', got '%s'", gen.PackageName)
	}
	if len(gen.Imports) == 0 {
		t.Error("Expected default imports to be set")
	}
}

func TestGenerateSimpleProgram(t *testing.T) {
	// Create a simple IR program
	program := &ir.Program{
		Name: "TestProgram",
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.DisplayStatement{
									Args: []ir.Expression{
										&ir.LiteralExpression{
											Value:  "Hello World",
											Type:   "string",
											Line:   1,
											Column: 1,
										},
									},
									Line:   1,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(program, &buf)
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	output := buf.String()

	// Check for package declaration
	if !strings.Contains(output, "package main") {
		t.Error("Expected package declaration")
	}

	// Check for imports
	if !strings.Contains(output, "import (") {
		t.Error("Expected import block")
	}

	// Check for job function
	if !strings.Contains(output, "func TestJob() error") {
		t.Error("Expected TestJob function")
	}

	// Check for step function
	if !strings.Contains(output, "func TestJob_Main() error") {
		t.Error("Expected TestJob_Main function")
	}

	// Check for main function
	if !strings.Contains(output, "func main()") {
		t.Error("Expected main function")
	}

	// Check for display statement
	if !strings.Contains(output, "fmt.Println") {
		t.Error("Expected fmt.Println call")
	}
}

func TestGenerateWithRecord(t *testing.T) {
	// Create a program with a record
	program := &ir.Program{
		Name: "TestProgram",
		Records: []*ir.Record{
			{
				Name: "CustomerRecord",
				Fields: []*ir.Field{
					{
						Name: "id",
						Type: &ir.TypeInfo{
							Type:   "int32",
							GoType: "int32",
						},
						Line:   1,
						Column: 1,
					},
					{
						Name: "name",
						Type: &ir.TypeInfo{
							Type:   "string",
							GoType: "string",
						},
						Line:   2,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.DisplayStatement{
									Args: []ir.Expression{
										&ir.LiteralExpression{
											Value:  "Processing customer",
											Type:   "string",
											Line:   1,
											Column: 1,
										},
									},
									Line:   1,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(program, &buf)
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	output := buf.String()

	// Check for record struct
	if !strings.Contains(output, "type CustomerRecord struct") {
		t.Error("Expected CustomerRecord struct")
	}

	// Check for struct fields
	if !strings.Contains(output, "Id int32") {
		t.Error("Expected Id field")
	}
	if !strings.Contains(output, "Name string") {
		t.Error("Expected Name field")
	}
}

func TestGenerateWithVariables(t *testing.T) {
	// Create a program with variables
	program := &ir.Program{
		Name: "TestProgram",
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Variables: []*ir.Variable{
					{
						Name: "count",
						Type: &ir.TypeInfo{
							Type:   "int32",
							GoType: "int32",
						},
						Line:   1,
						Column: 1,
					},
					{
						Name: "name",
						Type: &ir.TypeInfo{
							Type:   "string",
							GoType: "string",
						},
						Line:   2,
						Column: 1,
					},
				},
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.AssignmentStatement{
									Variable: "count",
									Value: &ir.LiteralExpression{
										Value:  "42",
										Type:   "int32",
										Line:   1,
										Column: 1,
									},
									Line:   1,
									Column: 1,
								},
								&ir.DisplayStatement{
									Args: []ir.Expression{
										&ir.IdentifierExpression{
											Name:   "count",
											Line:   2,
											Column: 1,
										},
									},
									Line:   2,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(program, &buf)
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	output := buf.String()

	// Check for variable declarations
	if !strings.Contains(output, "var count int32") {
		t.Error("Expected count variable declaration")
	}
	if !strings.Contains(output, "var name string") {
		t.Error("Expected name variable declaration")
	}

	// Check for assignment
	if !strings.Contains(output, "count = 42") {
		t.Error("Expected count assignment")
	}
}

func TestGetGoType(t *testing.T) {
	gen := New()

	tests := []struct {
		name     string
		typeInfo *ir.TypeInfo
		expected string
	}{
		{
			name: "int32",
			typeInfo: &ir.TypeInfo{
				Type:   "int32",
				GoType: "int32",
			},
			expected: "int32",
		},
		{
			name: "string",
			typeInfo: &ir.TypeInfo{
				Type:   "string",
				GoType: "string",
			},
			expected: "string",
		},
		{
			name: "decimal",
			typeInfo: &ir.TypeInfo{
				Type:   "decimal",
				GoType: "decimal.Decimal",
			},
			expected: "*decimal.Decimal",
		},
		{
			name: "record",
			typeInfo: &ir.TypeInfo{
				Type: "record",
				Record: &ir.Record{
					Name: "CustomerRecord",
				},
			},
			expected: "CustomerRecord",
		},
		{
			name:     "nil type",
			typeInfo: nil,
			expected: "interface{}",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gen.getGoType(tt.typeInfo)
			if result != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result)
			}
		})
	}
}

func TestGenerateWithLoops(t *testing.T) {
	// Create a program with loops
	program := &ir.Program{
		Name: "TestProgram",
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.ForStatement{
									Init: &ir.AssignmentStatement{
										Variable: "i",
										Value: &ir.LiteralExpression{
											Value:  "0",
											Type:   "int32",
											Line:   1,
											Column: 1,
										},
										Line:   1,
										Column: 1,
									},
									Condition: &ir.BinaryExpression{
										Left: &ir.IdentifierExpression{
											Name:   "i",
											Line:   1,
											Column: 1,
										},
										Operator: "<",
										Right: &ir.LiteralExpression{
											Value:  "10",
											Type:   "int32",
											Line:   1,
											Column: 1,
										},
										Line:   1,
										Column: 1,
									},
									Update: &ir.AssignmentStatement{
										Variable: "i",
										Value: &ir.BinaryExpression{
											Left: &ir.IdentifierExpression{
												Name:   "i",
												Line:   1,
												Column: 1,
											},
											Operator: "+",
											Right: &ir.LiteralExpression{
												Value:  "1",
												Type:   "int32",
												Line:   1,
												Column: 1,
											},
											Line:   1,
											Column: 1,
										},
										Line:   1,
										Column: 1,
									},
									Body: &ir.Block{
										Statements: []ir.Statement{
											&ir.DisplayStatement{
												Args: []ir.Expression{
													&ir.IdentifierExpression{
														Name:   "i",
														Line:   1,
														Column: 1,
													},
												},
												Line:   1,
												Column: 1,
											},
										},
									},
									Line:   1,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(program, &buf)
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	output := buf.String()

	// Check for for loop
	if !strings.Contains(output, "for i = 0; (i < 10); i = (i + 1)") {
		t.Error("Expected for loop with init, condition, and update")
	}

	// Check for loop body
	if !strings.Contains(output, "fmt.Println(i)") {
		t.Error("Expected fmt.Println in loop body")
	}
}

func TestGenerateWithWhileLoop(t *testing.T) {
	// Create a program with while loop
	program := &ir.Program{
		Name: "TestProgram",
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.WhileStatement{
									Condition: &ir.BinaryExpression{
										Left: &ir.IdentifierExpression{
											Name:   "count",
											Line:   1,
											Column: 1,
										},
										Operator: ">",
										Right: &ir.LiteralExpression{
											Value:  "0",
											Type:   "int32",
											Line:   1,
											Column: 1,
										},
										Line:   1,
										Column: 1,
									},
									Body: &ir.Block{
										Statements: []ir.Statement{
											&ir.DisplayStatement{
												Args: []ir.Expression{
													&ir.LiteralExpression{
														Value:  "Processing...",
														Type:   "string",
														Line:   1,
														Column: 1,
													},
												},
												Line:   1,
												Column: 1,
											},
										},
									},
									Line:   1,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(program, &buf)
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	output := buf.String()

	// Check for while loop (converted to for loop)
	if !strings.Contains(output, "for (count > 0)") {
		t.Error("Expected while loop converted to for loop")
	}

	// Check for loop body
	if !strings.Contains(output, "fmt.Println(\"Processing...\")") {
		t.Error("Expected fmt.Println in loop body")
	}
}
