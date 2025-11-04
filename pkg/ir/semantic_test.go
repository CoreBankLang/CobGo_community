package ir

import (
	"strings"
	"testing"
)

func TestNewSemanticAnalyzer(t *testing.T) {
	program := NewProgram("test")
	analyzer := NewSemanticAnalyzer(program)

	if analyzer.Program != program {
		t.Error("Expected analyzer program to match input program")
	}
	if len(analyzer.Errors) != 0 {
		t.Errorf("Expected no errors, got %d", len(analyzer.Errors))
	}
	if len(analyzer.Warnings) != 0 {
		t.Errorf("Expected no warnings, got %d", len(analyzer.Warnings))
	}
}

func TestSemanticAnalysisValidProgram(t *testing.T) {
	program := NewProgram("test")

	// Create a valid job with proper types
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

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err != nil {
		t.Errorf("Expected no error for valid program, got %v", err)
	}
	if analyzer.HasErrors() {
		t.Errorf("Expected no errors, got %d", len(analyzer.Errors))
	}
}

func TestSemanticAnalysisMissingJob(t *testing.T) {
	program := NewProgram("test")
	// No jobs added

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err == nil {
		t.Error("Expected error for program with no jobs")
	}
	if !analyzer.HasErrors() {
		t.Error("Expected errors for program with no jobs")
	}
}

func TestSemanticAnalysisInvalidType(t *testing.T) {
	program := NewProgram("test")

	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name:   "invalid",
				Type:   &TypeInfo{Type: "invalid_type", GoType: "interface{}"},
				Line:   1,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err == nil {
		t.Error("Expected error for invalid type")
	}
	if !analyzer.HasErrors() {
		t.Error("Expected errors for invalid type")
	}
}

func TestSemanticAnalysisDecimalConstraints(t *testing.T) {
	program := NewProgram("test")

	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name: "amount",
				Type: &TypeInfo{
					Type:      "decimal",
					GoType:    "decimal.Decimal",
					Precision: intPtr(5),
					Scale:     intPtr(2),
				},
				Line:   1,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err != nil {
		t.Errorf("Expected no error for valid decimal, got %v", err)
	}
	if analyzer.HasErrors() {
		t.Errorf("Expected no errors for valid decimal, got %d", len(analyzer.Errors))
	}
}

func TestSemanticAnalysisInvalidDecimalConstraints(t *testing.T) {
	program := NewProgram("test")

	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name: "amount",
				Type: &TypeInfo{
					Type:      "decimal",
					GoType:    "decimal.Decimal",
					Precision: intPtr(2),
					Scale:     intPtr(5), // Scale > Precision
				},
				Line:   1,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err == nil {
		t.Error("Expected error for invalid decimal constraints")
	}
	if !analyzer.HasErrors() {
		t.Error("Expected errors for invalid decimal constraints")
	}
}

func TestSemanticAnalysisDuplicateStepNames(t *testing.T) {
	program := NewProgram("test")

	job := &Job{
		Name: "TestJob",
		Steps: []*Step{
			{Name: "Step1", Line: 1, Column: 1},
			{Name: "Step1", Line: 2, Column: 1}, // Duplicate name
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err == nil {
		t.Error("Expected error for duplicate step names")
	}
	if !analyzer.HasErrors() {
		t.Error("Expected errors for duplicate step names")
	}
}

func TestSemanticAnalysisDuplicateVariableNames(t *testing.T) {
	program := NewProgram("test")

	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{Name: "var1", Type: &TypeInfo{Type: "int32"}, Line: 1, Column: 1},
			{Name: "var1", Type: &TypeInfo{Type: "int32"}, Line: 2, Column: 1}, // Duplicate name
		},
		Line:   1,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err == nil {
		t.Error("Expected error for duplicate variable names")
	}
	if !analyzer.HasErrors() {
		t.Error("Expected errors for duplicate variable names")
	}
}

func TestSemanticAnalysisRecordTypes(t *testing.T) {
	program := NewProgram("test")

	// Create a record
	record := &Record{
		Name: "Customer",
		Fields: []*Field{
			{
				Name:   "id",
				Type:   &TypeInfo{Type: "int32", GoType: "int32"},
				Line:   1,
				Column: 1,
			},
			{
				Name:   "name",
				Type:   &TypeInfo{Type: "string", GoType: "string", Size: intPtr(50)},
				Line:   2,
				Column: 1,
			},
		},
		Line:   1,
		Column: 1,
	}
	program.AddRecord(record)

	// Create a job that uses the record
	job := &Job{
		Name: "TestJob",
		Variables: []*Variable{
			{
				Name:   "customer",
				Type:   &TypeInfo{Type: "Customer", GoType: "Customer"},
				Line:   3,
				Column: 1,
			},
		},
		Line:   3,
		Column: 1,
	}
	program.AddJob(job)

	analyzer := NewSemanticAnalyzer(program)
	err := analyzer.Analyze()

	if err != nil {
		t.Errorf("Expected no error for valid record usage, got %v", err)
	}
	if analyzer.HasErrors() {
		t.Errorf("Expected no errors for valid record usage, got %d", len(analyzer.Errors))
	}
}

func TestSemanticError(t *testing.T) {
	err := &SemanticError{
		Message: "Test error",
		Line:    10,
		Column:  5,
		Type:    "TEST_ERROR",
	}

	expected := "TEST_ERROR at line 10, column 5: Test error"
	if err.Error() != expected {
		t.Errorf("Expected error message '%s', got '%s'", expected, err.Error())
	}
}

func TestGetErrorSummary(t *testing.T) {
	program := NewProgram("test")
	analyzer := NewSemanticAnalyzer(program)

	// Add some errors and warnings
	analyzer.addError(1, 1, "Test error 1", "ERROR")
	analyzer.addError(2, 1, "Test error 2", "ERROR")
	analyzer.addWarning(3, 1, "Test warning 1", "WARNING")

	summary := analyzer.GetErrorSummary()

	if !strings.Contains(summary, "Errors (2)") {
		t.Error("Expected error summary to contain 'Errors (2)'")
	}
	if !strings.Contains(summary, "Warnings (1)") {
		t.Error("Expected error summary to contain 'Warnings (1)'")
	}
	if !strings.Contains(summary, "Test error 1") {
		t.Error("Expected error summary to contain 'Test error 1'")
	}
	if !strings.Contains(summary, "Test warning 1") {
		t.Error("Expected error summary to contain 'Test warning 1'")
	}
}
