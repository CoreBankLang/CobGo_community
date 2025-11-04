package cobolparser

import (
	"fmt"
	"strings"
)

// ConvertToDSL converts a COBOL Program AST to CobGO DSL source code
func ConvertToDSL(program *Program) (string, error) {
	var dsl strings.Builder

	// Convert IDENTIFICATION DIVISION -> job declaration
	jobName := "MAIN"
	if program.Identification != nil && program.Identification.ProgramID != "" {
		jobName = sanitizeIdentifier(program.Identification.ProgramID)
	}

	dsl.WriteString(fmt.Sprintf("job %s {\n", jobName))

	// Convert DATA DIVISION -> data declarations at job level
	if program.Data != nil {
		dataDecl := convertDataDivision(program.Data)
		if dataDecl != "" {
			dsl.WriteString(dataDecl)
		}
	}

	// Convert PROCEDURE DIVISION -> step with statements
	if program.Procedure != nil {
		procCode := convertProcedureDivision(program.Procedure)
		dsl.WriteString(procCode)
	}

	dsl.WriteString("}\n")

	return dsl.String(), nil
}

// convertDataDivision converts DATA DIVISION to DSL data declarations
func convertDataDivision(data *DataDivision) string {
	var result strings.Builder

	// Convert WORKING-STORAGE SECTION
	if len(data.WorkingStorage) > 0 {
		result.WriteString("    // WORKING-STORAGE SECTION\n")
		for _, item := range data.WorkingStorage {
			if item.Level == 77 || item.Level == 1 {
				varDecl := convertDataItem(item, "    ")
				if varDecl != "" {
					result.WriteString(varDecl)
				}
			}
		}
		result.WriteString("\n")
	}

	return result.String()
}

// convertDataItem converts a single data item to DSL variable declaration
func convertDataItem(item *DataItem, indent string) string {
	if item.Name == "" {
		return ""
	}

	// Map COBOL PIC to DSL type
	dslType := convertPictureToType(item.Picture)
	name := sanitizeIdentifier(item.Name)

	// Generate var declaration
	var decl string
	if item.Value != "" {
		// Remove quotes from string literals
		value := strings.Trim(item.Value, "\"'")

		if dslType == "string" {
			decl = fmt.Sprintf("%svar %s %s = \"%s\"\n", indent, name, dslType, value)
		} else {
			// For numeric types, handle leading zeros
			// In COBOL, 0123 means 123 (leading zeros are insignificant)
			// But in Go, 0123 is octal notation
			value = sanitizeNumericValue(value)
			decl = fmt.Sprintf("%svar %s %s = %s\n", indent, name, dslType, value)
		}
	} else {
		// No initial value
		decl = fmt.Sprintf("%svar %s %s\n", indent, name, dslType)
	}

	return decl
}

// sanitizeNumericValue removes leading zeros and handles special cases
func sanitizeNumericValue(value string) string {
	// Remove leading/trailing whitespace
	value = strings.TrimSpace(value)

	// Handle sign prefixes
	sign := ""
	if strings.HasPrefix(value, "+") || strings.HasPrefix(value, "-") {
		sign = string(value[0])
		value = value[1:]
	}

	// Split on decimal point if present
	parts := strings.Split(value, ".")
	intPart := parts[0]
	decPart := ""
	if len(parts) > 1 {
		decPart = parts[1]
	}

	// Remove leading zeros from integer part (but keep at least one digit)
	intPart = strings.TrimLeft(intPart, "0")
	if intPart == "" {
		intPart = "0"
	}

	// Reconstruct the value
	if decPart != "" {
		return sign + intPart + "." + decPart
	}
	return sign + intPart
}

// convertPictureToType maps COBOL PICTURE clause to DSL type
func convertPictureToType(pic string) string {
	if pic == "" {
		return "string"
	}

	pic = strings.ToUpper(strings.TrimSpace(pic))

	// Remove IS if present
	pic = strings.TrimPrefix(pic, "IS ")

	// Handle signed decimal
	if strings.Contains(pic, "S") && strings.Contains(pic, "V") {
		return "decimal"
	}

	// Handle unsigned with decimal
	if strings.Contains(pic, "V") && !strings.Contains(pic, "S") {
		return "decimal"
	}

	// Handle signed integer
	if strings.Contains(pic, "S") && strings.Contains(pic, "9") {
		// Count digits
		if strings.Count(pic, "9") > 9 || strings.Contains(pic, "(") {
			// Check for parentheses indicating size
			if strings.Contains(pic, "(18)") || strings.Contains(pic, "(17)") {
				return "int64"
			}
			if strings.Contains(pic, "(1") {
				return "int64"
			}
		}
		return "int32"
	}

	// Handle unsigned integer
	if strings.Contains(pic, "9") && !strings.Contains(pic, "V") {
		if strings.Count(pic, "9") > 9 || strings.Contains(pic, "(") {
			if strings.Contains(pic, "(18)") {
				return "int64"
			}
			if strings.Contains(pic, "(1") {
				return "int64"
			}
		}
		return "int32"
	}

	// Handle alphanumeric
	if strings.Contains(pic, "X") || strings.Contains(pic, "A") {
		return "string"
	}

	// Default to string
	return "string"
}

// convertProcedureDivision converts PROCEDURE DIVISION to DSL statements
func convertProcedureDivision(proc *ProcedureDivision) string {
	var result strings.Builder

	result.WriteString("    step MAIN {\n")

	// Convert sections and paragraphs
	for _, section := range proc.Sections {
		for _, para := range section.Paragraphs {
			for _, stmt := range para.Statements {
				stmtCode := convertStatement(stmt, "        ")
				if stmtCode != "" {
					result.WriteString(stmtCode)
				}
			}
		}
	}

	// Convert standalone statements
	for _, stmt := range proc.Statements {
		stmtCode := convertStatement(stmt, "        ")
		if stmtCode != "" {
			result.WriteString(stmtCode)
		}
	}

	result.WriteString("    }\n")

	return result.String()
}

// convertStatement converts a single COBOL statement to DSL
func convertStatement(stmt Statement, indent string) string {
	switch s := stmt.(type) {
	case *MoveStatement:
		return convertMoveStatement(s, indent)
	case *DisplayStatement:
		return convertDisplayStatement(s, indent)
	case *PerformStatement:
		return convertPerformStatement(s, indent)
	case *StopStatement:
		return convertStopStatement(s, indent)
	case *ExitStatement:
		return "" // EXIT typically doesn't need conversion
	case *OpenStatement:
		return convertOpenStatement(s, indent)
	case *CloseStatement:
		return convertCloseStatement(s, indent)
	case *ReadStatement:
		return convertReadStatement(s, indent)
	case *WriteStatement:
		return convertWriteStatement(s, indent)
	default:
		return fmt.Sprintf("%s// TODO: Convert %T\n", indent, stmt)
	}
}

// convertMoveStatement converts MOVE to assignment
func convertMoveStatement(stmt *MoveStatement, indent string) string {
	if stmt.Source == nil || len(stmt.Destination) == 0 {
		return ""
	}

	source := convertExpression(stmt.Source)

	var result strings.Builder
	for _, dest := range stmt.Destination {
		destName := sanitizeIdentifier(dest)
		result.WriteString(fmt.Sprintf("%s%s = %s\n", indent, destName, source))
	}

	return result.String()
}

// convertDisplayStatement converts DISPLAY to display() function
func convertDisplayStatement(stmt *DisplayStatement, indent string) string {
	if len(stmt.Items) == 0 {
		return ""
	}

	// Convert each item
	var items []string
	for _, item := range stmt.Items {
		items = append(items, convertExpression(item))
	}

	// Join with +
	displayArg := strings.Join(items, " + \" \" + ")

	return fmt.Sprintf("%sdisplay(%s)\n", indent, displayArg)
}

// convertPerformStatement converts PERFORM (basic version)
func convertPerformStatement(stmt *PerformStatement, indent string) string {
	// For now, just add a comment
	if stmt.Target != "" {
		return fmt.Sprintf("%s// PERFORM %s\n", indent, stmt.Target)
	}
	return ""
}

// convertStopStatement converts STOP RUN
func convertStopStatement(stmt *StopStatement, indent string) string {
	return fmt.Sprintf("%s// STOP RUN\n", indent)
}

// convertOpenStatement converts OPEN
func convertOpenStatement(stmt *OpenStatement, indent string) string {
	if len(stmt.Files) == 0 {
		return ""
	}
	return fmt.Sprintf("%s// OPEN %s %s\n", indent, stmt.Mode, strings.Join(stmt.Files, ", "))
}

// convertCloseStatement converts CLOSE
func convertCloseStatement(stmt *CloseStatement, indent string) string {
	if len(stmt.Files) == 0 {
		return ""
	}
	return fmt.Sprintf("%s// CLOSE %s\n", indent, strings.Join(stmt.Files, ", "))
}

// convertReadStatement converts READ
func convertReadStatement(stmt *ReadStatement, indent string) string {
	return fmt.Sprintf("%s// READ %s\n", indent, stmt.FileName)
}

// convertWriteStatement converts WRITE
func convertWriteStatement(stmt *WriteStatement, indent string) string {
	return fmt.Sprintf("%s// WRITE %s\n", indent, stmt.RecordName)
}

// convertExpression converts a COBOL expression to DSL
func convertExpression(expr Expression) string {
	switch e := expr.(type) {
	case *Identifier:
		return sanitizeIdentifier(e.Name)
	case *NumberLiteral:
		// Sanitize numeric literals to remove leading zeros
		return sanitizeNumericValue(e.Value)
	case *StringLiteral:
		// Add quotes if not present
		if strings.HasPrefix(e.Value, "\"") || strings.HasPrefix(e.Value, "'") {
			return "\"" + strings.Trim(e.Value, "\"'") + "\""
		}
		return "\"" + e.Value + "\""
	case *BinaryExpression:
		left := convertExpression(e.Left)
		right := convertExpression(e.Right)
		return fmt.Sprintf("(%s %s %s)", left, e.Operator, right)
	default:
		return "nil"
	}
}

// sanitizeIdentifier converts COBOL identifier to DSL-compatible name
func sanitizeIdentifier(name string) string {
	// Replace hyphens with underscores
	name = strings.ReplaceAll(name, "-", "_")

	// Convert to lowercase for DSL convention (or keep uppercase)
	// For now, keep uppercase to match COBOL style
	return name
}
