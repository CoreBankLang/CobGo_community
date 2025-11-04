package ir

import (
	"fmt"
	"strings"
)

// SemanticError represents a semantic analysis error
type SemanticError struct {
	Message string
	Line    int
	Column  int
	Type    string
}

func (se *SemanticError) Error() string {
	return fmt.Sprintf("%s at line %d, column %d: %s", se.Type, se.Line, se.Column, se.Message)
}

// SemanticAnalyzer performs semantic analysis on the IR
type SemanticAnalyzer struct {
	Program     *Program
	Errors      []*SemanticError
	Warnings    []*SemanticError
	SymbolTable map[string]*SymbolInfo
}

// SymbolInfo represents information about a symbol
type SymbolInfo struct {
	Name      string
	Type      *TypeInfo
	Kind      SymbolKind
	Scope     string
	Line      int
	Column    int
	IsDefined bool
	IsUsed    bool
}

// SymbolKind represents the kind of symbol
type SymbolKind int

const (
	SymbolVariable SymbolKind = iota
	SymbolFunction
	SymbolRecord
	SymbolField
	SymbolParameter
)

// NewSemanticAnalyzer creates a new semantic analyzer
func NewSemanticAnalyzer(program *Program) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		Program:     program,
		Errors:      []*SemanticError{},
		Warnings:    []*SemanticError{},
		SymbolTable: make(map[string]*SymbolInfo),
	}
}

// Analyze performs semantic analysis on the program
func (sa *SemanticAnalyzer) Analyze() error {
	// Build symbol table
	sa.buildSymbolTable()

	// Validate types
	sa.validateTypes()

	// Validate decimal constraints
	sa.validateDecimalConstraints()

	// Validate job/step structure
	sa.validateJobStepStructure()

	// Check for unused variables
	sa.checkUnusedVariables()

	if len(sa.Errors) > 0 {
		return fmt.Errorf("semantic analysis failed with %d errors", len(sa.Errors))
	}

	return nil
}

// buildSymbolTable builds the symbol table from the program
func (sa *SemanticAnalyzer) buildSymbolTable() {
	// Add records to symbol table
	for _, record := range sa.Program.Records {
		sa.SymbolTable[record.Name] = &SymbolInfo{
			Name:      record.Name,
			Type:      &TypeInfo{Type: "record", GoType: "struct"},
			Kind:      SymbolRecord,
			Scope:     "global",
			Line:      record.Line,
			Column:    record.Column,
			IsDefined: true,
		}

		// Add fields to symbol table
		for _, field := range record.Fields {
			fieldKey := fmt.Sprintf("%s.%s", record.Name, field.Name)
			sa.SymbolTable[fieldKey] = &SymbolInfo{
				Name:      field.Name,
				Type:      field.Type,
				Kind:      SymbolField,
				Scope:     record.Name,
				Line:      field.Line,
				Column:    field.Column,
				IsDefined: true,
			}
		}
	}

	// Add jobs and their contents to symbol table
	for _, job := range sa.Program.Jobs {
		sa.SymbolTable[job.Name] = &SymbolInfo{
			Name:      job.Name,
			Type:      &TypeInfo{Type: "job", GoType: "func"},
			Kind:      SymbolFunction,
			Scope:     "global",
			Line:      job.Line,
			Column:    job.Column,
			IsDefined: true,
		}

		// Add job variables
		for _, variable := range job.Variables {
			sa.SymbolTable[variable.Name] = &SymbolInfo{
				Name:      variable.Name,
				Type:      variable.Type,
				Kind:      SymbolVariable,
				Scope:     job.Name,
				Line:      variable.Line,
				Column:    variable.Column,
				IsDefined: true,
			}
		}

		// Add steps
		for _, step := range job.Steps {
			stepKey := fmt.Sprintf("%s.%s", job.Name, step.Name)
			sa.SymbolTable[stepKey] = &SymbolInfo{
				Name:      step.Name,
				Type:      step.ReturnType,
				Kind:      SymbolFunction,
				Scope:     job.Name,
				Line:      step.Line,
				Column:    step.Column,
				IsDefined: true,
			}

			// Add step parameters
			for _, param := range step.Parameters {
				paramKey := fmt.Sprintf("%s.%s.%s", job.Name, step.Name, param.Name)
				sa.SymbolTable[paramKey] = &SymbolInfo{
					Name:      param.Name,
					Type:      param.Type,
					Kind:      SymbolParameter,
					Scope:     stepKey,
					Line:      param.Line,
					Column:    param.Column,
					IsDefined: true,
				}
			}

			// Add step variables
			for _, variable := range step.Variables {
				varKey := fmt.Sprintf("%s.%s.%s", job.Name, step.Name, variable.Name)
				sa.SymbolTable[varKey] = &SymbolInfo{
					Name:      variable.Name,
					Type:      variable.Type,
					Kind:      SymbolVariable,
					Scope:     stepKey,
					Line:      variable.Line,
					Column:    variable.Column,
					IsDefined: true,
				}
			}
		}
	}
}

// validateTypes validates type information
func (sa *SemanticAnalyzer) validateTypes() {
	for _, record := range sa.Program.Records {
		sa.validateRecordTypes(record)
	}

	for _, job := range sa.Program.Jobs {
		sa.validateJobTypes(job)
	}
}

// validateRecordTypes validates types in a record
func (sa *SemanticAnalyzer) validateRecordTypes(record *Record) {
	for _, field := range record.Fields {
		if field.Type == nil {
			sa.addError(field.Line, field.Column, "field type is required", "TYPE_ERROR")
			continue
		}

		if !sa.isValidType(field.Type) {
			sa.addError(field.Line, field.Column,
				fmt.Sprintf("invalid type '%s' for field '%s'", field.Type.Type, field.Name), "TYPE_ERROR")
		}
	}
}

// validateJobTypes validates types in a job
func (sa *SemanticAnalyzer) validateJobTypes(job *Job) {
	// Validate job variables
	for _, variable := range job.Variables {
		if variable.Type == nil {
			sa.addError(variable.Line, variable.Column, "variable type is required", "TYPE_ERROR")
			continue
		}

		if !sa.isValidType(variable.Type) {
			sa.addError(variable.Line, variable.Column,
				fmt.Sprintf("invalid type '%s' for variable '%s'", variable.Type.Type, variable.Name), "TYPE_ERROR")
		}
	}

	// Validate steps
	for _, step := range job.Steps {
		sa.validateStepTypes(step)
	}
}

// validateStepTypes validates types in a step
func (sa *SemanticAnalyzer) validateStepTypes(step *Step) {
	// Validate parameters
	for _, param := range step.Parameters {
		if param.Type == nil {
			sa.addError(param.Line, param.Column, "parameter type is required", "TYPE_ERROR")
			continue
		}

		if !sa.isValidType(param.Type) {
			sa.addError(param.Line, param.Column,
				fmt.Sprintf("invalid type '%s' for parameter '%s'", param.Type.Type, param.Name), "TYPE_ERROR")
		}
	}

	// Validate return type
	if step.ReturnType != nil && !sa.isValidType(step.ReturnType) {
		sa.addError(step.Line, step.Column,
			fmt.Sprintf("invalid return type '%s'", step.ReturnType.Type), "TYPE_ERROR")
	}

	// Validate step variables
	for _, variable := range step.Variables {
		if variable.Type == nil {
			sa.addError(variable.Line, variable.Column, "variable type is required", "TYPE_ERROR")
			continue
		}

		if !sa.isValidType(variable.Type) {
			sa.addError(variable.Line, variable.Column,
				fmt.Sprintf("invalid type '%s' for variable '%s'", variable.Type.Type, variable.Name), "TYPE_ERROR")
		}
	}
}

// isValidType checks if a type is valid
func (sa *SemanticAnalyzer) isValidType(typeInfo *TypeInfo) bool {
	validTypes := map[string]bool{
		"int32":   true,
		"int64":   true,
		"string":  true,
		"decimal": true,
		"date":    true,
		"bool":    true,
		"record":  true,
	}

	// Check if it's a built-in type
	if validTypes[typeInfo.Type] {
		return true
	}

	// Check if it's a user-defined record
	if sa.Program.FindRecord(typeInfo.Type) != nil {
		return true
	}

	return false
}

// validateDecimalConstraints validates decimal type constraints
func (sa *SemanticAnalyzer) validateDecimalConstraints() {
	for _, record := range sa.Program.Records {
		sa.validateDecimalConstraintsInRecord(record)
	}

	for _, job := range sa.Program.Jobs {
		sa.validateDecimalConstraintsInJob(job)
	}
}

// validateDecimalConstraintsInRecord validates decimal constraints in a record
func (sa *SemanticAnalyzer) validateDecimalConstraintsInRecord(record *Record) {
	for _, field := range record.Fields {
		sa.validateDecimalConstraintsInType(field.Type, field.Line, field.Column)
	}
}

// validateDecimalConstraintsInJob validates decimal constraints in a job
func (sa *SemanticAnalyzer) validateDecimalConstraintsInJob(job *Job) {
	for _, variable := range job.Variables {
		sa.validateDecimalConstraintsInType(variable.Type, variable.Line, variable.Column)
	}

	for _, step := range job.Steps {
		sa.validateDecimalConstraintsInStep(step)
	}
}

// validateDecimalConstraintsInStep validates decimal constraints in a step
func (sa *SemanticAnalyzer) validateDecimalConstraintsInStep(step *Step) {
	for _, param := range step.Parameters {
		sa.validateDecimalConstraintsInType(param.Type, param.Line, param.Column)
	}

	if step.ReturnType != nil {
		sa.validateDecimalConstraintsInType(step.ReturnType, step.Line, step.Column)
	}

	for _, variable := range step.Variables {
		sa.validateDecimalConstraintsInType(variable.Type, variable.Line, variable.Column)
	}
}

// validateDecimalConstraintsInType validates decimal constraints for a type
func (sa *SemanticAnalyzer) validateDecimalConstraintsInType(typeInfo *TypeInfo, line, column int) {
	if typeInfo.Type != "decimal" {
		return
	}

	if typeInfo.Precision == nil || typeInfo.Scale == nil {
		sa.addError(line, column, "decimal type requires precision and scale", "DECIMAL_ERROR")
		return
	}

	precision := *typeInfo.Precision
	scale := *typeInfo.Scale

	if precision <= 0 {
		sa.addError(line, column, "decimal precision must be positive", "DECIMAL_ERROR")
	}

	if scale < 0 {
		sa.addError(line, column, "decimal scale must be non-negative", "DECIMAL_ERROR")
	}

	if scale > precision {
		sa.addError(line, column, "decimal scale cannot be greater than precision", "DECIMAL_ERROR")
	}

	if precision > 38 {
		sa.addError(line, column, "decimal precision cannot exceed 38", "DECIMAL_ERROR")
	}
}

// validateJobStepStructure validates job and step structure
func (sa *SemanticAnalyzer) validateJobStepStructure() {
	// Check that there's at least one job
	if len(sa.Program.Jobs) == 0 {
		sa.addError(0, 0, "program must have at least one job", "STRUCTURE_ERROR")
		return
	}

	// Validate each job
	for _, job := range sa.Program.Jobs {
		sa.validateJobStructure(job)
	}
}

// validateJobStructure validates a job structure
func (sa *SemanticAnalyzer) validateJobStructure(job *Job) {
	// Check for duplicate step names
	stepNames := make(map[string]bool)
	for _, step := range job.Steps {
		if stepNames[step.Name] {
			sa.addError(step.Line, step.Column,
				fmt.Sprintf("duplicate step name '%s'", step.Name), "STRUCTURE_ERROR")
		}
		stepNames[step.Name] = true
	}

	// Check for duplicate variable names
	varNames := make(map[string]bool)
	for _, variable := range job.Variables {
		if varNames[variable.Name] {
			sa.addError(variable.Line, variable.Column,
				fmt.Sprintf("duplicate variable name '%s'", variable.Name), "STRUCTURE_ERROR")
		}
		varNames[variable.Name] = true
	}

	// Validate steps
	for _, step := range job.Steps {
		sa.validateStepStructure(step)
	}
}

// validateStepStructure validates a step structure
func (sa *SemanticAnalyzer) validateStepStructure(step *Step) {
	// Check for duplicate parameter names
	paramNames := make(map[string]bool)
	for _, param := range step.Parameters {
		if paramNames[param.Name] {
			sa.addError(param.Line, param.Column,
				fmt.Sprintf("duplicate parameter name '%s'", param.Name), "STRUCTURE_ERROR")
		}
		paramNames[param.Name] = true
	}

	// Check for duplicate variable names
	varNames := make(map[string]bool)
	for _, variable := range step.Variables {
		if varNames[variable.Name] {
			sa.addError(variable.Line, variable.Column,
				fmt.Sprintf("duplicate variable name '%s'", variable.Name), "STRUCTURE_ERROR")
		}
		varNames[variable.Name] = true
	}

	// Check for parameter/variable name conflicts
	for _, param := range step.Parameters {
		for _, variable := range step.Variables {
			if param.Name == variable.Name {
				sa.addError(variable.Line, variable.Column,
					fmt.Sprintf("variable name '%s' conflicts with parameter", variable.Name), "STRUCTURE_ERROR")
			}
		}
	}
}

// checkUnusedVariables checks for unused variables
func (sa *SemanticAnalyzer) checkUnusedVariables() {
	// This is a simplified check - in a real implementation,
	// you would track variable usage during expression analysis
	for name, symbol := range sa.SymbolTable {
		if symbol.Kind == SymbolVariable && !symbol.IsUsed {
			sa.addWarning(symbol.Line, symbol.Column,
				fmt.Sprintf("unused variable '%s'", name), "UNUSED_VARIABLE")
		}
	}
}

// addError adds a semantic error
func (sa *SemanticAnalyzer) addError(line, column int, message, errorType string) {
	sa.Errors = append(sa.Errors, &SemanticError{
		Message: message,
		Line:    line,
		Column:  column,
		Type:    errorType,
	})
}

// addWarning adds a semantic warning
func (sa *SemanticAnalyzer) addWarning(line, column int, message, warningType string) {
	sa.Warnings = append(sa.Warnings, &SemanticError{
		Message: message,
		Line:    line,
		Column:  column,
		Type:    warningType,
	})
}

// GetErrors returns all semantic errors
func (sa *SemanticAnalyzer) GetErrors() []*SemanticError {
	return sa.Errors
}

// GetWarnings returns all semantic warnings
func (sa *SemanticAnalyzer) GetWarnings() []*SemanticError {
	return sa.Warnings
}

// HasErrors returns true if there are semantic errors
func (sa *SemanticAnalyzer) HasErrors() bool {
	return len(sa.Errors) > 0
}

// HasWarnings returns true if there are semantic warnings
func (sa *SemanticAnalyzer) HasWarnings() bool {
	return len(sa.Warnings) > 0
}

// GetErrorSummary returns a summary of errors and warnings
func (sa *SemanticAnalyzer) GetErrorSummary() string {
	var summary strings.Builder

	if len(sa.Errors) > 0 {
		summary.WriteString(fmt.Sprintf("Errors (%d):\n", len(sa.Errors)))
		for _, err := range sa.Errors {
			summary.WriteString(fmt.Sprintf("  %s\n", err.Error()))
		}
	}

	if len(sa.Warnings) > 0 {
		summary.WriteString(fmt.Sprintf("Warnings (%d):\n", len(sa.Warnings)))
		for _, warn := range sa.Warnings {
			summary.WriteString(fmt.Sprintf("  %s\n", warn.Error()))
		}
	}

	return summary.String()
}
