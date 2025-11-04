package copybook

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Options contains conversion options
type Options struct {
	Prefix string // Prefix for generated record names
	Suffix string // Suffix for generated record names
	Format string // Output format (dsl, json)
}

// Converter converts COBOL copybooks to DSL
type Converter struct {
	options *Options
}

// NewConverter creates a new copybook converter
func NewConverter(options *Options) *Converter {
	if options == nil {
		options = &Options{
			Format: "dsl",
		}
	}
	return &Converter{
		options: options,
	}
}

// ConvertFile converts a copybook file to DSL
func (c *Converter) ConvertFile(filename string) (string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	return c.Convert(file)
}

// Convert converts a copybook from an io.Reader to DSL
func (c *Converter) Convert(reader io.Reader) (string, error) {
	records, err := c.parseCopybook(reader)
	if err != nil {
		return "", fmt.Errorf("failed to parse copybook: %w", err)
	}

	switch c.options.Format {
	case "json":
		return c.generateJSON(records)
	case "dsl":
		fallthrough
	default:
		return c.generateDSL(records)
	}
}

// Record represents a COBOL record structure
type Record struct {
	Name   string
	Fields []Field
	Line   int
}

// Field represents a COBOL field
type Field struct {
	Level     int
	Name      string
	PIC       string
	Type      string
	Length    int
	Precision int
	Scale     int
	Occurs    int
	Redefines string
	Line      int
}

// parseCopybook parses a COBOL copybook
func (c *Converter) parseCopybook(reader io.Reader) ([]Record, error) {
	var records []Record
	var currentRecord *Record
	var fieldStack []Field // For handling nested structures

	scanner := bufio.NewScanner(reader)
	lineNum := 0

	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())

		// Skip empty lines and comments
		if line == "" || strings.HasPrefix(line, "*") || strings.HasPrefix(line, "/") {
			continue
		}

		// Parse the line
		field, err := c.parseLine(line, lineNum)
		if err != nil {
			continue // Skip invalid lines
		}

		// Handle record boundaries (01 level)
		if field.Level == 1 {
			// Save previous record if exists
			if currentRecord != nil {
				records = append(records, *currentRecord)
			}

			// Start new record
			currentRecord = &Record{
				Name:   c.formatRecordName(field.Name),
				Fields: []Field{},
				Line:   lineNum,
			}
			fieldStack = []Field{}
		}

		if currentRecord != nil {
			// Only add fields with level > 1 (skip the record name itself)
			if field.Level > 1 {
				// Handle field hierarchy
				c.updateFieldStack(&fieldStack, field)
				currentRecord.Fields = append(currentRecord.Fields, field)
			}
		}
	}

	// Add the last record
	if currentRecord != nil {
		records = append(records, *currentRecord)
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading file: %w", err)
	}

	return records, nil
}

// parseLine parses a single copybook line
func (c *Converter) parseLine(line string, lineNum int) (Field, error) {
	field := Field{Line: lineNum}

	// Regular expression to match COBOL copybook line format
	// Format: level name [PIC clause] [other clauses]
	re := regexp.MustCompile(`^(\d+)\s+([A-Z0-9-]+)`)
	matches := re.FindStringSubmatch(line)

	if len(matches) < 3 {
		return field, fmt.Errorf("invalid line format: %s", line)
	}

	// Parse level
	level, err := strconv.Atoi(matches[1])
	if err != nil {
		return field, fmt.Errorf("invalid level: %s", matches[1])
	}
	field.Level = level

	// Parse name
	field.Name = matches[2]

	// Parse remaining clauses (if any)
	var clauses []string
	// Extract everything after the name
	remaining := strings.TrimSpace(line[len(matches[0]):])
	if remaining != "" {
		// Remove trailing period and split into clauses
		clauseText := strings.TrimSuffix(remaining, ".")
		clauses = strings.Fields(clauseText)
	}
	for i, clause := range clauses {
		clause = strings.ToUpper(clause)

		switch {
		case strings.HasPrefix(clause, "PIC"):
			// Parse PIC clause
			if i+1 < len(clauses) {
				field.PIC = clauses[i+1]
				field.Type, field.Length, field.Precision, field.Scale = c.parsePIC(field.PIC)
			}
		case strings.HasPrefix(clause, "OCCURS"):
			// Parse OCCURS clause
			if i+1 < len(clauses) {
				occurs, err := strconv.Atoi(clauses[i+1])
				if err == nil {
					field.Occurs = occurs
				}
			}
		case strings.HasPrefix(clause, "REDEFINES"):
			// Parse REDEFINES clause
			if i+1 < len(clauses) {
				field.Redefines = clauses[i+1]
			}
		}
	}

	return field, nil
}

// parsePIC parses a PIC clause and returns type information
func (c *Converter) parsePIC(pic string) (string, int, int, int) {
	pic = strings.ToUpper(pic)

	// Remove quotes but keep parentheses for parsing
	pic = strings.Trim(pic, "'\"")
	pic = strings.ReplaceAll(pic, " ", "")

	// Parse different PIC patterns
	switch {
	case strings.Contains(pic, "9") && strings.Contains(pic, "V"):
		// Decimal number (e.g., 999V99, S999V99)
		return c.parseDecimalPIC(pic)
	case strings.Contains(pic, "9"):
		// Integer number (e.g., 999, S999)
		return c.parseIntegerPIC(pic)
	case strings.Contains(pic, "X"):
		// Character string (e.g., X(10), X(20))
		return c.parseStringPIC(pic)
	case strings.Contains(pic, "A"):
		// Alphabetic (e.g., A(10), A(20))
		return c.parseAlphabeticPIC(pic)
	default:
		// Default to string
		return "string", 10, 0, 0
	}
}

// parseDecimalPIC parses decimal PIC clauses
func (c *Converter) parseDecimalPIC(pic string) (string, int, int, int) {
	// Handle signed decimal (S prefix)
	signed := strings.HasPrefix(pic, "S")
	if signed {
		pic = pic[1:]
	}

	// Split on V (decimal point)
	parts := strings.Split(pic, "V")
	if len(parts) != 2 {
		return "decimal", 10, 5, 2
	}

	// Calculate precision and scale
	integerDigits := len(parts[0])
	scaleDigits := len(parts[1])
	precision := integerDigits + scaleDigits

	return "decimal", precision, precision, scaleDigits
}

// parseIntegerPIC parses integer PIC clauses
func (c *Converter) parseIntegerPIC(pic string) (string, int, int, int) {
	// Handle signed integer (S prefix)
	signed := strings.HasPrefix(pic, "S")
	if signed {
		pic = pic[1:]
	}

	// Count digits
	length := len(pic)

	// Determine appropriate integer type
	if length <= 9 {
		return "int32", length, 0, 0
	}
	return "int64", length, 0, 0
}

// parseStringPIC parses string PIC clauses
func (c *Converter) parseStringPIC(pic string) (string, int, int, int) {
	// Handle X(n) format
	re := regexp.MustCompile(`X\((\d+)\)`)
	matches := re.FindStringSubmatch(pic)
	if len(matches) > 1 {
		length, _ := strconv.Atoi(matches[1])
		return "string", length, 0, 0
	}

	// Handle Xn format (e.g., X10)
	re = regexp.MustCompile(`X(\d+)`)
	matches = re.FindStringSubmatch(pic)
	if len(matches) > 1 {
		length, _ := strconv.Atoi(matches[1])
		return "string", length, 0, 0
	}

	// Default length
	return "string", 10, 0, 0
}

// parseAlphabeticPIC parses alphabetic PIC clauses
func (c *Converter) parseAlphabeticPIC(pic string) (string, int, int, int) {
	// Handle A(n) format
	re := regexp.MustCompile(`A\((\d+)\)`)
	matches := re.FindStringSubmatch(pic)
	if len(matches) > 1 {
		length, _ := strconv.Atoi(matches[1])
		return "string", length, 0, 0
	}

	// Handle An format (e.g., A10)
	re = regexp.MustCompile(`A(\d+)`)
	matches = re.FindStringSubmatch(pic)
	if len(matches) > 1 {
		length, _ := strconv.Atoi(matches[1])
		return "string", length, 0, 0
	}

	// Default length
	return "string", len(pic), 0, 0
}

// updateFieldStack manages the field hierarchy stack
func (c *Converter) updateFieldStack(stack *[]Field, field Field) {
	// Remove fields with higher or equal level
	for len(*stack) > 0 && (*stack)[len(*stack)-1].Level >= field.Level {
		*stack = (*stack)[:len(*stack)-1]
	}

	// Add current field to stack
	*stack = append(*stack, field)
}

// formatRecordName formats a record name with prefix/suffix
func (c *Converter) formatRecordName(name string) string {
	result := name

	if c.options.Prefix != "" {
		result = c.options.Prefix + result
	}

	if c.options.Suffix != "" {
		result = result + c.options.Suffix
	}

	return result
}

// generateDSL generates DSL output
func (c *Converter) generateDSL(records []Record) (string, error) {
	var output strings.Builder

	output.WriteString("// Generated from COBOL copybook\n")
	output.WriteString("// Converted by CobGO copybook2dsl\n\n")

	for _, record := range records {
		output.WriteString(fmt.Sprintf("record %s {\n", record.Name))

		for _, field := range record.Fields {
			if field.Level > 1 {
				// Indent nested fields
				indent := strings.Repeat("    ", field.Level-1)
				output.WriteString(fmt.Sprintf("%s%s %s\n", indent, c.formatFieldName(field.Name), c.formatFieldType(field)))
			} else {
				output.WriteString(fmt.Sprintf("    %s %s\n", c.formatFieldName(field.Name), c.formatFieldType(field)))
			}
		}

		output.WriteString("}\n\n")
	}

	return output.String(), nil
}

// formatFieldName converts COBOL field names to camelCase
func (c *Converter) formatFieldName(name string) string {
	// Convert COBOL field names (e.g., CUSTOMER-ID) to camelCase (e.g., CustomerId)
	parts := strings.Split(strings.ToLower(name), "-")
	result := ""
	for _, part := range parts {
		if part != "" {
			result += strings.ToUpper(part[:1]) + part[1:]
		}
	}
	return result
}

// formatFieldType formats a field type for DSL output
func (c *Converter) formatFieldType(field Field) string {
	switch field.Type {
	case "decimal":
		if field.Precision > 0 && field.Scale >= 0 {
			return fmt.Sprintf("decimal(%d,%d)", field.Precision, field.Scale)
		}
		return "decimal"
	case "int32":
		return "int32"
	case "int64":
		return "int64"
	case "string":
		if field.Length > 0 {
			return fmt.Sprintf("string(%d)", field.Length)
		}
		return "string"
	default:
		return "string"
	}
}

// generateJSON generates JSON output
func (c *Converter) generateJSON(records []Record) (string, error) {
	// This would generate JSON representation of the records
	// For now, return a simple JSON structure
	var output strings.Builder
	output.WriteString("{\n")
	output.WriteString("  \"records\": [\n")

	for i, record := range records {
		if i > 0 {
			output.WriteString(",\n")
		}
		output.WriteString(fmt.Sprintf("    {\n"))
		output.WriteString(fmt.Sprintf("      \"name\": \"%s\",\n", record.Name))
		output.WriteString(fmt.Sprintf("      \"fields\": [\n"))

		for j, field := range record.Fields {
			if j > 0 {
				output.WriteString(",\n")
			}
			output.WriteString(fmt.Sprintf("        {\n"))
			output.WriteString(fmt.Sprintf("          \"name\": \"%s\",\n", field.Name))
			output.WriteString(fmt.Sprintf("          \"type\": \"%s\",\n", field.Type))
			output.WriteString(fmt.Sprintf("          \"length\": %d,\n", field.Length))
			output.WriteString(fmt.Sprintf("          \"precision\": %d,\n", field.Precision))
			output.WriteString(fmt.Sprintf("          \"scale\": %d\n", field.Scale))
			output.WriteString(fmt.Sprintf("        }"))
		}

		output.WriteString("\n      ]\n")
		output.WriteString(fmt.Sprintf("    }"))
	}

	output.WriteString("\n  ]\n")
	output.WriteString("}\n")

	return output.String(), nil
}
