package validation

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

// DSLValidator validates DSL output against golden datasets
type DSLValidator struct {
	GoldenDir string
}

// NewDSLValidator creates a new DSL validator
func NewDSLValidator(goldenDir string) *DSLValidator {
	return &DSLValidator{
		GoldenDir: goldenDir,
	}
}

// ValidateDSL validates generated DSL against golden dataset
func (v *DSLValidator) ValidateDSL(generatedDSL, testName string) error {
	// Read golden dataset
	goldenFile := filepath.Join(v.GoldenDir, "dsl", testName+".cobgo")
	goldenContent, err := os.ReadFile(goldenFile)
	if err != nil {
		return fmt.Errorf("failed to read golden dataset: %w", err)
	}

	// Parse both DSL files
	generatedAST, err := v.parseDSL(generatedDSL)
	if err != nil {
		return fmt.Errorf("failed to parse generated DSL: %w", err)
	}

	goldenAST, err := v.parseDSL(string(goldenContent))
	if err != nil {
		return fmt.Errorf("failed to parse golden DSL: %w", err)
	}

	// Compare ASTs
	if err := v.compareASTs(generatedAST, goldenAST); err != nil {
		return fmt.Errorf("DSL validation failed: %w", err)
	}

	return nil
}

// parseDSL parses DSL content into AST
func (v *DSLValidator) parseDSL(content string) (*parser.Program, error) {
	p := parser.New()
	return p.Parse(strings.NewReader(content))
}

// compareASTs compares two ASTs for structural equivalence
func (v *DSLValidator) compareASTs(ast1, ast2 *parser.Program) error {
	// Compare number of jobs
	if len(ast1.Jobs) != len(ast2.Jobs) {
		return fmt.Errorf("job count mismatch: got %d, expected %d", len(ast1.Jobs), len(ast2.Jobs))
	}

	// Compare each job
	for i, job1 := range ast1.Jobs {
		job2 := ast2.Jobs[i]
		if err := v.compareJobs(job1, job2); err != nil {
			return fmt.Errorf("job %d comparison failed: %w", i, err)
		}
	}

	return nil
}

// compareJobs compares two job statements
func (v *DSLValidator) compareJobs(job1, job2 *parser.JobStatement) error {
	// Compare job names
	if job1.Name.Value != job2.Name.Value {
		return fmt.Errorf("job name mismatch: got %q, expected %q", job1.Name.Value, job2.Name.Value)
	}

	// Compare job bodies
	if err := v.compareBlocks(job1.Body, job2.Body); err != nil {
		return fmt.Errorf("job body comparison failed: %w", err)
	}

	return nil
}

// compareBlocks compares two block statements
func (v *DSLValidator) compareBlocks(block1, block2 *parser.BlockStatement) error {
	if block1 == nil && block2 == nil {
		return nil
	}
	if block1 == nil || block2 == nil {
		return fmt.Errorf("block nil mismatch")
	}

	// Compare number of statements
	if len(block1.Statements) != len(block2.Statements) {
		return fmt.Errorf("statement count mismatch: got %d, expected %d", len(block1.Statements), len(block2.Statements))
	}

	// Compare each statement
	for i, stmt1 := range block1.Statements {
		stmt2 := block2.Statements[i]
		if err := v.compareStatements(stmt1, stmt2); err != nil {
			return fmt.Errorf("statement %d comparison failed: %w", i, err)
		}
	}

	return nil
}

// compareStatements compares two statements
func (v *DSLValidator) compareStatements(stmt1, stmt2 parser.Statement) error {
	// For now, we'll do a simple string comparison
	// In a more sophisticated implementation, we'd compare the AST structures
	if stmt1.String() != stmt2.String() {
		return fmt.Errorf("statement mismatch: got %q, expected %q", stmt1.String(), stmt2.String())
	}

	return nil
}

// ValidateDSLFile validates a DSL file against golden dataset
func (v *DSLValidator) ValidateDSLFile(dslFile, testName string) error {
	content, err := os.ReadFile(dslFile)
	if err != nil {
		return fmt.Errorf("failed to read DSL file: %w", err)
	}

	return v.ValidateDSL(string(content), testName)
}

// ValidateDSLContent validates DSL content from reader
func (v *DSLValidator) ValidateDSLContent(reader io.Reader, testName string) error {
	content, err := io.ReadAll(reader)
	if err != nil {
		return fmt.Errorf("failed to read DSL content: %w", err)
	}

	return v.ValidateDSL(string(content), testName)
}

// ListAvailableTests returns a list of available test names
func (v *DSLValidator) ListAvailableTests() ([]string, error) {
	dslDir := filepath.Join(v.GoldenDir, "dsl")
	files, err := filepath.Glob(filepath.Join(dslDir, "*.cobgo"))
	if err != nil {
		return nil, fmt.Errorf("failed to list DSL files: %w", err)
	}

	var tests []string
	for _, file := range files {
		base := filepath.Base(file)
		testName := strings.TrimSuffix(base, ".cobgo")
		tests = append(tests, testName)
	}

	return tests, nil
}

// ValidateAllTests validates all available tests
func (v *DSLValidator) ValidateAllTests() map[string]error {
	tests, err := v.ListAvailableTests()
	if err != nil {
		return map[string]error{"list_tests": err}
	}

	results := make(map[string]error)
	for _, test := range tests {
		// For now, we'll just validate that the golden file exists
		// In a real implementation, we'd run the actual validation
		goldenFile := filepath.Join(v.GoldenDir, "dsl", test+".cobgo")
		if _, err := os.Stat(goldenFile); err != nil {
			results[test] = fmt.Errorf("golden file not found: %w", err)
		} else {
			results[test] = nil
		}
	}

	return results
}
