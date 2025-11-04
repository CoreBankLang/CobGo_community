package ir

import (
	"strings"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

func TestParseToIRPipeline(t *testing.T) {
	// Test the complete pipeline: Parse -> AST -> IR -> Semantic Analysis

	// 1. Parse CobGO source code
	source := `job TestJob {
    step Main {
        display("Hello World")
    }
}`

	parser := parser.New()
	astProgram, err := parser.Parse(strings.NewReader(source))
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// 2. Convert AST to IR
	converter := NewASTToIRConverter()
	irProgram, err := converter.Convert(astProgram)
	if err != nil {
		t.Fatalf("AST to IR conversion error: %v", err)
	}

	// 3. Perform semantic analysis
	analyzer := NewSemanticAnalyzer(irProgram)
	err = analyzer.Analyze()
	if err != nil {
		t.Fatalf("Semantic analysis error: %v", err)
	}

	// 4. Verify the IR structure
	if len(irProgram.Jobs) != 1 {
		t.Errorf("Expected 1 job, got %d", len(irProgram.Jobs))
	}

	job := irProgram.Jobs[0]
	if job.Name != "TestJob" {
		t.Errorf("Expected job name 'TestJob', got '%s'", job.Name)
	}

	if len(job.Steps) != 1 {
		t.Errorf("Expected 1 step, got %d", len(job.Steps))
	}

	step := job.Steps[0]
	if step.Name != "Main" {
		t.Errorf("Expected step name 'Main', got '%s'", step.Name)
	}

	// 5. Verify no semantic errors
	if analyzer.HasErrors() {
		t.Errorf("Expected no semantic errors, got: %s", analyzer.GetErrorSummary())
	}
}

func TestParseToIRWithRecord(t *testing.T) {
	// Test parsing a program with a record definition

	source := `job CustomerService {
    step ProcessCustomer {
        display("Customer processed")
    }
}`

	parser := parser.New()
	astProgram, err := parser.Parse(strings.NewReader(source))
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	converter := NewASTToIRConverter()
	irProgram, err := converter.Convert(astProgram)
	if err != nil {
		t.Fatalf("AST to IR conversion error: %v", err)
	}

	analyzer := NewSemanticAnalyzer(irProgram)
	err = analyzer.Analyze()
	if err != nil {
		t.Fatalf("Semantic analysis error: %v", err)
	}

	// Verify job was created
	if len(irProgram.Jobs) != 1 {
		t.Errorf("Expected 1 job, got %d", len(irProgram.Jobs))
	}

	job := irProgram.Jobs[0]
	if job.Name != "CustomerService" {
		t.Errorf("Expected job name 'CustomerService', got '%s'", job.Name)
	}

	if len(job.Steps) != 1 {
		t.Errorf("Expected 1 step, got %d", len(job.Steps))
	}

	step := job.Steps[0]
	if step.Name != "ProcessCustomer" {
		t.Errorf("Expected step name 'ProcessCustomer', got '%s'", step.Name)
	}
}

func TestParseToIRWithErrors(t *testing.T) {
	// Test parsing a program with semantic errors

	source := `job TestJob {
    var invalid decimal = 10.5  // Missing precision and scale
    var duplicate int32 = 5
    var duplicate int32 = 10    // Duplicate variable name
}`

	parser := parser.New()
	astProgram, err := parser.Parse(strings.NewReader(source))
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	converter := NewASTToIRConverter()
	irProgram, err := converter.Convert(astProgram)
	if err != nil {
		t.Fatalf("AST to IR conversion error: %v", err)
	}

	analyzer := NewSemanticAnalyzer(irProgram)
	err = analyzer.Analyze()

	// Should have errors
	if err == nil {
		t.Error("Expected semantic analysis to fail with errors")
	}

	if !analyzer.HasErrors() {
		t.Error("Expected semantic errors")
	}

	// Check specific errors
	errors := analyzer.GetErrors()
	if len(errors) == 0 {
		t.Error("Expected at least one error")
	}

	// Look for decimal constraint error
	foundDecimalError := false
	for _, err := range errors {
		if strings.Contains(err.Message, "decimal") && strings.Contains(err.Message, "precision") {
			foundDecimalError = true
			break
		}
	}
	if !foundDecimalError {
		t.Error("Expected decimal constraint error")
	}

	// Look for duplicate variable error
	foundDuplicateError := false
	for _, err := range errors {
		if strings.Contains(err.Message, "duplicate") && strings.Contains(err.Message, "variable") {
			foundDuplicateError = true
			break
		}
	}
	if !foundDuplicateError {
		t.Error("Expected duplicate variable error")
	}
}

func TestIRJSONSerialization(t *testing.T) {
	// Test JSON serialization of IR

	program := NewProgram("test")
	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name:   "count",
				Type:   &TypeInfo{Type: "int32", GoType: "int32"},
				Line:   1,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	json, err := program.ToJSON()
	if err != nil {
		t.Fatalf("JSON serialization error: %v", err)
	}

	// Check that JSON contains expected content
	jsonStr := string(json)
	if !strings.Contains(jsonStr, "test") {
		t.Error("Expected JSON to contain program name")
	}
	if !strings.Contains(jsonStr, "TestJob") {
		t.Error("Expected JSON to contain job name")
	}
	if !strings.Contains(jsonStr, "count") {
		t.Error("Expected JSON to contain variable name")
	}
	if !strings.Contains(jsonStr, "int32") {
		t.Error("Expected JSON to contain variable type")
	}
}

func TestSemanticAnalysisDecimalValidation(t *testing.T) {
	// Test decimal validation specifically

	program := NewProgram("test")
	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name: "validDecimal",
				Type: &TypeInfo{
					Type:      "decimal",
					GoType:    "decimal.Decimal",
					Precision: intPtr(10),
					Scale:     intPtr(2),
				},
				Line:   1,
				Column: 1,
			},
			{
				Name: "invalidDecimal",
				Type: &TypeInfo{
					Type:      "decimal",
					GoType:    "decimal.Decimal",
					Precision: intPtr(5),
					Scale:     intPtr(10), // Scale > Precision
				},
				Line:   2,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	// Should have errors due to invalid decimal
	if err == nil {
		t.Error("Expected semantic analysis to fail with decimal constraint error")
	}

	if !analyzer.HasErrors() {
		t.Error("Expected semantic errors")
	}

	// Check for specific decimal constraint error
	errors := analyzer.GetErrors()
	foundDecimalError := false
	for _, err := range errors {
		if strings.Contains(err.Message, "scale") && strings.Contains(err.Message, "precision") {
			foundDecimalError = true
			break
		}
	}
	if !foundDecimalError {
		t.Error("Expected decimal constraint error about scale and precision")
	}
}

// Helper function to create int pointer
func intPtr(i int) *int {
	return &i
}
