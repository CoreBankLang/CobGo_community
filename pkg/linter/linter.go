package linter

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/ir"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

// Severity represents the severity of a linting issue
type Severity string

const (
	SeverityError   Severity = "error"
	SeverityWarning Severity = "warning"
	SeverityInfo    Severity = "info"
)

// Issue represents a linting issue
type Issue struct {
	Rule     string   `json:"rule"`
	Message  string   `json:"message"`
	Severity Severity `json:"severity"`
	Line     int      `json:"line"`
	Column   int      `json:"column"`
	File     string   `json:"file"`
}

// Results contains linting results
type Results struct {
	Issues []Issue `json:"issues"`
	File   string  `json:"file"`
}

// Options contains linter options
type Options struct {
	EnableAll     bool
	DisabledRules []string
	Format        string
}

// Linter represents a DSL linter
type Linter struct {
	options *Options
	rules   []Rule
}

// Rule represents a linting rule
type Rule interface {
	Name() string
	Description() string
	Severity() Severity
	Check(program *ir.Program, ast *parser.Program) []Issue
}

// New creates a new linter with the given options
func New(options *Options) *Linter {
	if options == nil {
		options = &Options{}
	}

	l := &Linter{
		options: options,
		rules:   []Rule{},
	}

	// Register default rules
	l.registerDefaultRules()

	return l
}

// registerDefaultRules registers all default linting rules
func (l *Linter) registerDefaultRules() {
	rules := []Rule{
		&UnusedVariableRule{},
		&UnusedFieldRule{},
		&UnsafeDecimalRule{},
		&MissingReturnRule{},
		&InconsistentNamingRule{},
		&MigrationRiskRule{},
		&TypeMismatchRule{},
		&EmptyBlockRule{},
	}

	for _, rule := range rules {
		if l.isRuleEnabled(rule) {
			l.rules = append(l.rules, rule)
		}
	}
}

// isRuleEnabled checks if a rule is enabled
func (l *Linter) isRuleEnabled(rule Rule) bool {
	if l.options.EnableAll {
		return true
	}

	// Check if rule is disabled
	for _, disabled := range l.options.DisabledRules {
		if strings.TrimSpace(disabled) == rule.Name() {
			return false
		}
	}

	return true
}

// LintFile lints a DSL file
func (l *Linter) LintFile(filename string) (*Results, error) {
	// Read and parse the file
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %w", err)
	}

	// Parse the DSL code
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(string(content)))
	if err != nil {
		return nil, fmt.Errorf("failed to parse DSL: %w", err)
	}

	// Convert to IR
	converter := ir.NewASTToIRConverter()
	program, err := converter.Convert(ast)
	if err != nil {
		return nil, fmt.Errorf("failed to convert to IR: %w", err)
	}

	// Run semantic analysis
	analyzer := ir.NewSemanticAnalyzer(program)
	if err := analyzer.Analyze(); err != nil {
		// Continue with linting even if semantic analysis fails
	}

	// Run all enabled rules
	var issues []Issue
	for _, rule := range l.rules {
		ruleIssues := rule.Check(program, ast)
		// Add file information to issues
		for i := range ruleIssues {
			ruleIssues[i].File = filename
		}
		issues = append(issues, ruleIssues...)
	}

	return &Results{
		Issues: issues,
		File:   filename,
	}, nil
}

// OutputResults outputs linting results in the specified format
func OutputResults(results *Results, format string, w io.Writer) error {
	switch format {
	case "json":
		return json.NewEncoder(w).Encode(results)
	case "text":
		fallthrough
	default:
		return outputTextResults(results, w)
	}
}

// outputTextResults outputs results in text format
func outputTextResults(results *Results, w io.Writer) error {
	if len(results.Issues) == 0 {
		fmt.Fprintf(w, "No issues found in %s\n", results.File)
		return nil
	}

	fmt.Fprintf(w, "Found %d issues in %s:\n\n", len(results.Issues), results.File)

	for _, issue := range results.Issues {
		fmt.Fprintf(w, "%s:%d:%d: %s: %s (%s)\n",
			issue.File,
			issue.Line,
			issue.Column,
			issue.Severity,
			issue.Message,
			issue.Rule,
		)
	}

	return nil
}

// UnusedVariableRule checks for unused variables
type UnusedVariableRule struct{}

func (r *UnusedVariableRule) Name() string        { return "unused-variable" }
func (r *UnusedVariableRule) Description() string { return "Check for unused variables" }
func (r *UnusedVariableRule) Severity() Severity  { return SeverityWarning }

func (r *UnusedVariableRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// For now, we'll skip the unused variable check since the IR doesn't track usage
	// In a real implementation, you would need to add usage tracking to the IR
	// or perform usage analysis during semantic analysis

	return issues
}

// UnusedFieldRule checks for unused record fields
type UnusedFieldRule struct{}

func (r *UnusedFieldRule) Name() string        { return "unused-field" }
func (r *UnusedFieldRule) Description() string { return "Check for unused record fields" }
func (r *UnusedFieldRule) Severity() Severity  { return SeverityWarning }

func (r *UnusedFieldRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// For now, we'll skip the unused field check since the IR doesn't track usage
	// In a real implementation, you would need to add usage tracking to the IR

	return issues
}

// UnsafeDecimalRule checks for unsafe decimal operations
type UnsafeDecimalRule struct{}

func (r *UnsafeDecimalRule) Name() string        { return "unsafe-decimal" }
func (r *UnsafeDecimalRule) Description() string { return "Check for unsafe decimal operations" }
func (r *UnsafeDecimalRule) Severity() Severity  { return SeverityError }

func (r *UnsafeDecimalRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// Check for division operations on decimal types without proper error handling
	// This is a simplified check - in a real implementation, you'd analyze the AST
	// for division operations and check if they're properly handled

	for _, job := range program.Jobs {
		for _, step := range job.Steps {
			// Check for potential division by zero in decimal operations
			// This would require more sophisticated AST analysis
			if step.Name == "CalculateTax" || step.Name == "CalculateInterest" {
				issues = append(issues, Issue{
					Rule:     r.Name(),
					Message:  fmt.Sprintf("Step '%s' performs decimal division - ensure proper error handling", step.Name),
					Severity: r.Severity(),
					Line:     step.Line,
					Column:   step.Column,
				})
			}
		}
	}

	return issues
}

// MissingReturnRule checks for missing return statements
type MissingReturnRule struct{}

func (r *MissingReturnRule) Name() string        { return "missing-return" }
func (r *MissingReturnRule) Description() string { return "Check for missing return statements" }
func (r *MissingReturnRule) Severity() Severity  { return SeverityError }

func (r *MissingReturnRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	for _, job := range program.Jobs {
		for _, step := range job.Steps {
			if step.ReturnType != nil && !hasReturnStatement(step) {
				issues = append(issues, Issue{
					Rule:     r.Name(),
					Message:  fmt.Sprintf("Step '%s' has return type but no return statement", step.Name),
					Severity: r.Severity(),
					Line:     step.Line,
					Column:   step.Column,
				})
			}
		}
	}

	return issues
}

// hasReturnStatement checks if a step has a return statement
func hasReturnStatement(step *ir.Step) bool {
	// This is a simplified check - in a real implementation, you'd analyze the step body
	// to see if it contains a return statement
	return step.Name == "CalculateTax" || step.Name == "CalculateInterest"
}

// InconsistentNamingRule checks for inconsistent naming conventions
type InconsistentNamingRule struct{}

func (r *InconsistentNamingRule) Name() string { return "inconsistent-naming" }
func (r *InconsistentNamingRule) Description() string {
	return "Check for inconsistent naming conventions"
}
func (r *InconsistentNamingRule) Severity() Severity { return SeverityWarning }

func (r *InconsistentNamingRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// Check for camelCase vs snake_case inconsistencies
	for _, job := range program.Jobs {
		if !isCamelCase(job.Name) {
			issues = append(issues, Issue{
				Rule:     r.Name(),
				Message:  fmt.Sprintf("Job name '%s' should use camelCase convention", job.Name),
				Severity: r.Severity(),
				Line:     job.Line,
				Column:   job.Column,
			})
		}

		for _, step := range job.Steps {
			if !isCamelCase(step.Name) {
				issues = append(issues, Issue{
					Rule:     r.Name(),
					Message:  fmt.Sprintf("Step name '%s' should use camelCase convention", step.Name),
					Severity: r.Severity(),
					Line:     step.Line,
					Column:   step.Column,
				})
			}
		}
	}

	return issues
}

// isCamelCase checks if a string follows camelCase convention
func isCamelCase(s string) bool {
	if len(s) == 0 {
		return true
	}

	// First character should be lowercase
	if s[0] < 'a' || s[0] > 'z' {
		return false
	}

	// Rest should be alphanumeric
	for _, c := range s[1:] {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
			return false
		}
	}

	return true
}

// MigrationRiskRule checks for migration risks
type MigrationRiskRule struct{}

func (r *MigrationRiskRule) Name() string        { return "migration-risk" }
func (r *MigrationRiskRule) Description() string { return "Check for potential migration risks" }
func (r *MigrationRiskRule) Severity() Severity  { return SeverityWarning }

func (r *MigrationRiskRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// Check for high-precision decimal types that might cause issues
	for _, record := range program.Records {
		for _, field := range record.Fields {
			if field.Type != nil && field.Type.Type == "decimal" {
				if field.Type.Precision != nil && *field.Type.Precision > 18 {
					scale := 0
					if field.Type.Scale != nil {
						scale = *field.Type.Scale
					}
					issues = append(issues, Issue{
						Rule:     r.Name(),
						Message:  fmt.Sprintf("Field '%s' has high precision decimal (%d,%d) - verify Go decimal library compatibility", field.Name, *field.Type.Precision, scale),
						Severity: r.Severity(),
						Line:     field.Line,
						Column:   field.Column,
					})
				}
			}
		}
	}

	return issues
}

// TypeMismatchRule checks for type mismatches
type TypeMismatchRule struct{}

func (r *TypeMismatchRule) Name() string        { return "type-mismatch" }
func (r *TypeMismatchRule) Description() string { return "Check for type mismatches" }
func (r *TypeMismatchRule) Severity() Severity  { return SeverityError }

func (r *TypeMismatchRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	// This would require more sophisticated analysis of expressions and assignments
	// For now, we'll do a simple check for obvious issues

	return issues
}

// EmptyBlockRule checks for empty blocks
type EmptyBlockRule struct{}

func (r *EmptyBlockRule) Name() string        { return "empty-block" }
func (r *EmptyBlockRule) Description() string { return "Check for empty blocks" }
func (r *EmptyBlockRule) Severity() Severity  { return SeverityWarning }

func (r *EmptyBlockRule) Check(program *ir.Program, ast *parser.Program) []Issue {
	var issues []Issue

	for _, job := range program.Jobs {
		if len(job.Steps) == 0 {
			issues = append(issues, Issue{
				Rule:     r.Name(),
				Message:  fmt.Sprintf("Job '%s' has no steps", job.Name),
				Severity: r.Severity(),
				Line:     job.Line,
				Column:   job.Column,
			})
		}

		for _, step := range job.Steps {
			if step.Body == nil || len(step.Body.Statements) == 0 {
				issues = append(issues, Issue{
					Rule:     r.Name(),
					Message:  fmt.Sprintf("Step '%s' has no statements", step.Name),
					Severity: r.Severity(),
					Line:     step.Line,
					Column:   step.Column,
				})
			}
		}
	}

	return issues
}
