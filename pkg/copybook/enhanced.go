package copybook

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// EnhancedField extends Field with advanced COBOL features
type EnhancedField struct {
	Field
	Usage         UsageType
	Synchronized  bool
	Justified     bool
	DependingOn   string // For OCCURS DEPENDING ON
	Value         string // COBOL VALUE clause
	Condition88   []Condition88
	BlankWhenZero bool
	SignLeading   bool
	SignSeparate  bool
}

// PICType represents COBOL PICTURE clause types
type PICType int

const (
	PICTypeNumeric PICType = iota
	PICTypeAlphanumeric
	PICTypeAlphanumericEdited
	PICTypeNumericEdited
)

// UsageType represents COBOL USAGE clause
type UsageType int

const (
	UsageDisplay UsageType = iota
	UsageComp
	UsageComp1
	UsageComp2
	UsageComp3 // Packed-Decimal
	UsageComp4
	UsageComp5
	UsagePackedDecimal
	UsageBinary
	UsageIndex
	UsagePointer
)

// Condition88 represents an 88-level condition
type Condition88 struct {
	Name   string
	Values []string
	Ranges []ValueRange
}

// ValueRange represents a range in an 88-level condition
type ValueRange struct {
	Low  string
	High string
}

// EnhancedParser provides advanced copybook parsing
type EnhancedParser struct {
	replacements map[string]string // For COPY REPLACING
}

// NewEnhancedParser creates a new enhanced parser
func NewEnhancedParser() *EnhancedParser {
	return &EnhancedParser{
		replacements: make(map[string]string),
	}
}

// ParseWithReplacing parses a copybook line with REPLACING support
func (ep *EnhancedParser) ParseWithReplacing(line string, replacements map[string]string) string {
	result := line

	// Apply all replacements
	for old, new := range replacements {
		// Support both ==OLD== and :OLD: syntax
		result = strings.ReplaceAll(result, fmt.Sprintf("==%s==", old), new)
		result = strings.ReplaceAll(result, fmt.Sprintf(":%s:", old), new)
	}

	return result
}

// ParseUsage parses COBOL USAGE clause
func (ep *EnhancedParser) ParseUsage(usageStr string) UsageType {
	usageStr = strings.ToUpper(strings.TrimSpace(usageStr))

	switch {
	case usageStr == "DISPLAY":
		return UsageDisplay
	case usageStr == "COMP" || usageStr == "COMPUTATIONAL":
		return UsageComp
	case usageStr == "COMP-1" || usageStr == "COMPUTATIONAL-1":
		return UsageComp1
	case usageStr == "COMP-2" || usageStr == "COMPUTATIONAL-2":
		return UsageComp2
	case usageStr == "COMP-3" || usageStr == "COMPUTATIONAL-3" || usageStr == "PACKED-DECIMAL":
		return UsageComp3
	case usageStr == "COMP-4" || usageStr == "COMPUTATIONAL-4":
		return UsageComp4
	case usageStr == "COMP-5" || usageStr == "COMPUTATIONAL-5":
		return UsageComp5
	case usageStr == "BINARY":
		return UsageBinary
	case usageStr == "INDEX":
		return UsageIndex
	case usageStr == "POINTER":
		return UsagePointer
	default:
		return UsageDisplay
	}
}

// ParseCondition88 parses an 88-level condition
func (ep *EnhancedParser) ParseCondition88(line string) (*Condition88, error) {
	// Example: 88 CUSTOMER-ACTIVE VALUE 'A' 'Y'.
	// Example: 88 VALID-AGE VALUE 18 THRU 65.

	re := regexp.MustCompile(`88\s+(\S+)\s+VALUE\s+(.+)\.`)
	matches := re.FindStringSubmatch(line)

	if len(matches) < 3 {
		return nil, fmt.Errorf("invalid 88-level condition: %s", line)
	}

	condition := &Condition88{
		Name:   matches[1],
		Values: []string{},
		Ranges: []ValueRange{},
	}

	valuesStr := matches[2]

	// Check for THRU/THROUGH (range)
	if strings.Contains(strings.ToUpper(valuesStr), "THRU") ||
		strings.Contains(strings.ToUpper(valuesStr), "THROUGH") {
		rangeParts := regexp.MustCompile(`(?i)thru|through`).Split(valuesStr, -1)
		if len(rangeParts) == 2 {
			condition.Ranges = append(condition.Ranges, ValueRange{
				Low:  strings.TrimSpace(rangeParts[0]),
				High: strings.TrimSpace(rangeParts[1]),
			})
		}
	} else {
		// Multiple values separated by space or comma
		values := regexp.MustCompile(`[,\s]+`).Split(valuesStr, -1)
		for _, v := range values {
			v = strings.TrimSpace(v)
			if v != "" {
				// Remove quotes if present
				v = strings.Trim(v, "'\"")
				condition.Values = append(condition.Values, v)
			}
		}
	}

	return condition, nil
}

// ParseRedefines parses REDEFINES clause
func (ep *EnhancedParser) ParseRedefines(line string) (string, error) {
	// Example: 05 ALT-FIELD REDEFINES ORIG-FIELD PIC X(10).

	re := regexp.MustCompile(`REDEFINES\s+(\S+)`)
	matches := re.FindStringSubmatch(strings.ToUpper(line))

	if len(matches) < 2 {
		return "", fmt.Errorf("invalid REDEFINES clause: %s", line)
	}

	return matches[1], nil
}

// ParseOccursDependingOn parses OCCURS DEPENDING ON clause
func (ep *EnhancedParser) ParseOccursDependingOn(line string) (occurs int, dependingOn string, err error) {
	// Example: 05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT PIC X(10).

	// Try full DEPENDING ON clause
	re := regexp.MustCompile(`OCCURS\s+(?:(\d+)\s+TO\s+)?(\d+)\s+TIMES?\s+DEPENDING\s+ON\s+(\S+)`)
	matches := re.FindStringSubmatch(strings.ToUpper(line))

	if len(matches) >= 4 {
		occurs, _ = strconv.Atoi(matches[2])
		dependingOn = matches[3]
		return occurs, dependingOn, nil
	}

	// Try simple OCCURS
	re = regexp.MustCompile(`OCCURS\s+(\d+)`)
	matches = re.FindStringSubmatch(strings.ToUpper(line))

	if len(matches) >= 2 {
		occurs, err := strconv.Atoi(matches[1])
		return occurs, "", err
	}

	return 0, "", fmt.Errorf("invalid OCCURS clause: %s", line)
}

// CalculateComp3Size calculates the size of a COMP-3 (packed-decimal) field
func (ep *EnhancedParser) CalculateComp3Size(precision, scale int) int {
	// COMP-3 uses 1 byte for every 2 digits, plus 1 byte for sign
	totalDigits := precision + scale
	return (totalDigits+1)/2 + 1
}

// CalculateCompSize calculates the size of a COMP/COMP-4 (binary) field
func (ep *EnhancedParser) CalculateCompSize(precision int) int {
	// COMP/COMP-4 binary fields
	switch {
	case precision <= 4:
		return 2 // Halfword (2 bytes)
	case precision <= 9:
		return 4 // Fullword (4 bytes)
	case precision <= 18:
		return 8 // Doubleword (8 bytes)
	default:
		return 16 // Quadword (16 bytes)
	}
}

// TranslateComp3ToGo generates Go code for COMP-3 field access
func (ep *EnhancedParser) TranslateComp3ToGo(fieldName string, precision, scale int) string {
	return fmt.Sprintf(`
// COMP-3 (Packed-Decimal) field: %s
func decode%sComp3(data []byte) decimal.Decimal {
	// Decode packed-decimal format
	var result int64 = 0
	sign := 1
	
	for i, b := range data {
		if i == len(data)-1 {
			// Last byte contains sign in lower nibble
			result = result * 10 + int64((b >> 4) & 0x0F)
			signNibble := b & 0x0F
			if signNibble == 0x0D || signNibble == 0x0B {
				sign = -1
			}
		} else {
			// Two digits per byte
			result = result * 10 + int64((b >> 4) & 0x0F)
			result = result * 10 + int64(b & 0x0F)
		}
	}
	
	// Apply scale and sign
	value := decimal.NewFromInt(result * int64(sign))
	return value.Shift(-%d) // Shift by scale
}
`, fieldName, fieldName, scale)
}

// TranslateCompToGo generates Go code for COMP/COMP-4 field access
func (ep *EnhancedParser) TranslateCompToGo(fieldName string, precision int, signed bool) string {
	goType := "uint32"
	if signed {
		goType = "int32"
	}

	size := ep.CalculateCompSize(precision)

	return fmt.Sprintf(`
// COMP/COMP-4 (Binary) field: %s
func decode%sComp(data []byte) %s {
	// Decode binary format (%d bytes, big-endian)
	var result %s = 0
	for _, b := range data[:min(len(data), %d)] {
		result = (result << 8) | %s(b)
	}
	return result
}
`, fieldName, fieldName, goType, size, goType, size, goType)
}

// GenerateCondition88Go generates Go code for 88-level conditions
func (ep *EnhancedParser) GenerateCondition88Go(condition *Condition88, parentField string) string {
	var code strings.Builder

	code.WriteString(fmt.Sprintf("// Condition: %s\n", condition.Name))
	code.WriteString(fmt.Sprintf("func Is%s(%s interface{}) bool {\n", condition.Name, parentField))

	if len(condition.Values) > 0 {
		code.WriteString("\tswitch " + parentField + " {\n")
		for _, value := range condition.Values {
			code.WriteString(fmt.Sprintf("\tcase %q:\n", value))
		}
		code.WriteString("\t\treturn true\n")
		code.WriteString("\t}\n")
		code.WriteString("\treturn false\n")
	} else if len(condition.Ranges) > 0 {
		// Range check
		for i, rang := range condition.Ranges {
			if i > 0 {
				code.WriteString(" || ")
			}
			code.WriteString(fmt.Sprintf("\t// Check if %s is between %s and %s\n",
				parentField, rang.Low, rang.High))
			code.WriteString(fmt.Sprintf("\tif %s >= %s && %s <= %s {\n",
				parentField, rang.Low, parentField, rang.High))
			code.WriteString("\t\treturn true\n")
			code.WriteString("\t}\n")
		}
		code.WriteString("\treturn false\n")
	}

	code.WriteString("}\n")

	return code.String()
}

// ValidateRedefines validates REDEFINES constraints
func (ep *EnhancedParser) ValidateRedefines(redefining, redefined Field) error {
	// COBOL rules for REDEFINES:
	// 1. Redefining field must follow redefined field
	// 2. Cannot redefine a level 01, 66, 77, or 88
	// 3. Redefining field cannot have OCCURS
	// 4. Size validation

	if redefining.Level <= 1 || redefining.Level == 66 || redefining.Level == 77 || redefining.Level == 88 {
		return fmt.Errorf("cannot use REDEFINES at level %d", redefining.Level)
	}

	if redefining.Occurs > 0 {
		return fmt.Errorf("field with OCCURS cannot use REDEFINES")
	}

	// Calculate sizes for validation
	redefiningSize := ep.calculateFieldSize(redefining)
	redefinedSize := ep.calculateFieldSize(redefined)

	if redefiningSize > redefinedSize {
		return fmt.Errorf("redefining field (%d bytes) cannot be larger than redefined field (%d bytes)",
			redefiningSize, redefinedSize)
	}

	return nil
}

// calculateFieldSize calculates the total size of a field in bytes
func (ep *EnhancedParser) calculateFieldSize(field Field) int {
	// Simple calculation - would need enhancement for complex types
	size := field.Length
	if field.Occurs > 0 {
		size *= field.Occurs
	}
	return size
}

// ParseSynchronized checks for SYNCHRONIZED clause
func (ep *EnhancedParser) ParseSynchronized(line string) bool {
	upper := strings.ToUpper(line)
	return strings.Contains(upper, "SYNCHRONIZED") ||
		strings.Contains(upper, "SYNC")
}

// ParseJustified checks for JUSTIFIED clause
func (ep *EnhancedParser) ParseJustified(line string) bool {
	upper := strings.ToUpper(line)
	return strings.Contains(upper, "JUSTIFIED") ||
		strings.Contains(upper, "JUST")
}

// ParseValue extracts VALUE clause
func (ep *EnhancedParser) ParseValue(line string) string {
	re := regexp.MustCompile(`VALUE\s+(?:IS\s+)?([^.\s]+)`)
	matches := re.FindStringSubmatch(strings.ToUpper(line))

	if len(matches) >= 2 {
		value := strings.Trim(matches[1], "'\"")
		return value
	}

	return ""
}
