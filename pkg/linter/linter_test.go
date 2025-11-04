package linter

import (
	"strings"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/ir"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

func TestLinter(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected int // Expected number of issues
	}{
		{
			name: "clean code",
			input: `job TestJob {
    var count int32 = 10
    
    step Main {
        display("Hello")
    }
}`,
			expected: 0,
		},
		{
			name: "unused variable",
			input: `job TestJob {
    var unused int32 = 10
    var used int32 = 20
    
    step Main {
        display(string(used))
    }
}`,
			expected: 1, // unused variable
		},
		{
			name: "inconsistent naming",
			input: `job test_job {
    step main_step {
        display("Hello")
    }
}`,
			expected: 2, // inconsistent naming for job and step
		},
		{
			name: "empty block",
			input: `job TestJob {
    step EmptyStep {
    }
}`,
			expected: 1, // empty step
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Parse the input
			p := parser.New()
			ast, err := p.Parse(strings.NewReader(tt.input))
			if err != nil {
				t.Fatalf("Parse failed: %v", err)
			}

			// Convert to IR
			converter := ir.NewASTToIRConverter()
			program, err := converter.Convert(ast)
			if err != nil {
				t.Fatalf("AST to IR conversion failed: %v", err)
			}

			// Create linter
			linter := New(nil)

			// Create a mock results structure
			results := &Results{
				Issues: []Issue{},
				File:   "test.cobgo",
			}

			// Run all rules
			for _, rule := range linter.rules {
				ruleIssues := rule.Check(program, ast)
				results.Issues = append(results.Issues, ruleIssues...)
			}

			if len(results.Issues) != tt.expected {
				t.Errorf("Expected %d issues, got %d", tt.expected, len(results.Issues))
				for _, issue := range results.Issues {
					t.Logf("Issue: %s - %s", issue.Rule, issue.Message)
				}
			}
		})
	}
}

func TestUnusedVariableRule(t *testing.T) {
	rule := &UnusedVariableRule{}

	// Create a mock program with unused variables
	program := &ir.Program{
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Variables: []*ir.Variable{
					{
						Name:   "used",
						Line:   2,
						Column: 5,
					},
					{
						Name:   "unused",
						Line:   3,
						Column: 5,
					},
				},
				Steps: []*ir.Step{
					{
						Name: "Main",
						Variables: []*ir.Variable{
							{
								Name:   "localUnused",
								Line:   5,
								Column: 9,
							},
						},
					},
				},
			},
		},
	}

	issues := rule.Check(program, nil)

	expectedIssues := 2 // unused and localUnused
	if len(issues) != expectedIssues {
		t.Errorf("Expected %d issues, got %d", expectedIssues, len(issues))
	}

	// Check that the issues are for the correct variables
	unusedFound := false
	localUnusedFound := false

	for _, issue := range issues {
		if strings.Contains(issue.Message, "unused") {
			unusedFound = true
		}
		if strings.Contains(issue.Message, "localUnused") {
			localUnusedFound = true
		}
	}

	if !unusedFound {
		t.Error("Expected issue for 'unused' variable")
	}
	if !localUnusedFound {
		t.Error("Expected issue for 'localUnused' variable")
	}
}

func TestUnsafeDecimalRule(t *testing.T) {
	rule := &UnsafeDecimalRule{}

	// Create a mock program with decimal operations
	program := &ir.Program{
		Jobs: []*ir.Job{
			{
				Name: "TestJob",
				Steps: []*ir.Step{
					{
						Name:   "CalculateTax",
						Line:   5,
						Column: 5,
					},
					{
						Name:   "CalculateInterest",
						Line:   10,
						Column: 5,
					},
					{
						Name:   "NormalStep",
						Line:   15,
						Column: 5,
					},
				},
			},
		},
	}

	issues := rule.Check(program, nil)

	expectedIssues := 2 // CalculateTax and CalculateInterest
	if len(issues) != expectedIssues {
		t.Errorf("Expected %d issues, got %d", expectedIssues, len(issues))
	}
}

func TestInconsistentNamingRule(t *testing.T) {
	rule := &InconsistentNamingRule{}

	// Create a mock program with inconsistent naming
	program := &ir.Program{
		Jobs: []*ir.Job{
			{
				Name:   "test_job", // Should be camelCase
				Line:   1,
				Column: 1,
				Steps: []*ir.Step{
					{
						Name:   "main_step", // Should be camelCase
						Line:   3,
						Column: 5,
					},
					{
						Name:   "ValidStep", // This is correct
						Line:   6,
						Column: 5,
					},
				},
			},
		},
	}

	issues := rule.Check(program, nil)

	expectedIssues := 2 // test_job and main_step
	if len(issues) != expectedIssues {
		t.Errorf("Expected %d issues, got %d", expectedIssues, len(issues))
	}
}

func TestIsCamelCase(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"testJob", true},
		{"TestJob", false}, // Should start with lowercase
		{"test_job", false},
		{"test-job", false},
		{"test123", true},
		{"test123Job", true},
		{"", true},
		{"a", true},
		{"A", false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := isCamelCase(tt.input)
			if result != tt.expected {
				t.Errorf("isCamelCase(%q) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

func TestMigrationRiskRule(t *testing.T) {
	rule := &MigrationRiskRule{}

	// Create a mock program with high-precision decimals
	program := &ir.Program{
		Records: []*ir.Record{
			{
				Name: "TestRecord",
				Fields: []*ir.Field{
					{
						Name: "normalDecimal",
						Type: &ir.TypeInfo{
							Type:      "decimal",
							Precision: intPtr(10),
							Scale:     intPtr(2),
						},
						Line:   3,
						Column: 5,
					},
					{
						Name: "highPrecisionDecimal",
						Type: &ir.TypeInfo{
							Type:      "decimal",
							Precision: intPtr(25), // High precision
							Scale:     intPtr(5),
						},
						Line:   4,
						Column: 5,
					},
				},
			},
		},
	}

	issues := rule.Check(program, nil)

	expectedIssues := 1 // highPrecisionDecimal
	if len(issues) != expectedIssues {
		t.Errorf("Expected %d issues, got %d", expectedIssues, len(issues))
	}

	if len(issues) > 0 && !strings.Contains(issues[0].Message, "highPrecisionDecimal") {
		t.Error("Expected issue for highPrecisionDecimal field")
	}
}

func TestEmptyBlockRule(t *testing.T) {
	rule := &EmptyBlockRule{}

	// Create a mock program with empty blocks
	program := &ir.Program{
		Jobs: []*ir.Job{
			{
				Name:   "EmptyJob", // No steps
				Line:   1,
				Column: 1,
			},
			{
				Name:   "ValidJob",
				Line:   5,
				Column: 1,
				Steps: []*ir.Step{
					{
						Name:   "EmptyStep",
						Line:   6,
						Column: 5,
						Body: &ir.Block{
							Statements: []ir.Statement{}, // Empty body
						},
					},
					{
						Name:   "ValidStep",
						Line:   10,
						Column: 5,
						Body: &ir.Block{
							Statements: []ir.Statement{
								// Mock statement
							},
						},
					},
				},
			},
		},
	}

	issues := rule.Check(program, nil)

	expectedIssues := 2 // EmptyJob and EmptyStep
	if len(issues) != expectedIssues {
		t.Errorf("Expected %d issues, got %d", expectedIssues, len(issues))
	}
}

// Helper function to create int pointers for test data
func intPtr(i int) *int {
	return &i
}
