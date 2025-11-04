package codegen

import (
	"fmt"
	"io"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

// generateInspectStatement generates Go code for COBOL INSPECT statement
func (g *Generator) generateInspectStatement(stmt *ir.InspectStatement, writer io.Writer) error {
	switch stmt.Operation {
	case "TALLYING":
		return g.generateInspectTallying(stmt, writer)
	case "REPLACING":
		return g.generateInspectReplacing(stmt, writer)
	case "CONVERTING":
		return g.generateInspectConverting(stmt, writer)
	default:
		return fmt.Errorf("unknown INSPECT operation: %s", stmt.Operation)
	}
}

// generateInspectTallying generates Go code for INSPECT TALLYING
func (g *Generator) generateInspectTallying(stmt *ir.InspectStatement, writer io.Writer) error {
	var code string

	switch stmt.Mode {
	case "ALL":
		// Count all occurrences
		code = fmt.Sprintf("\t%s = strings.Count(%s, %q)\n",
			stmt.Counter, stmt.Identifier, stmt.Pattern)

	case "LEADING":
		// Count leading occurrences
		code = fmt.Sprintf(`	%s = 0
	for strings.HasPrefix(%s, %q) {
		%s++
		%s = %s[len(%q):]
	}
`,
			stmt.Counter, stmt.Identifier, stmt.Pattern,
			stmt.Counter,
			stmt.Identifier, stmt.Identifier, stmt.Pattern)

	case "CHARACTERS":
		// Count all characters
		code = fmt.Sprintf("\t%s = len(%s)\n", stmt.Counter, stmt.Identifier)
	}

	_, err := writer.Write([]byte(code))
	return err
}

// generateInspectReplacing generates Go code for INSPECT REPLACING
func (g *Generator) generateInspectReplacing(stmt *ir.InspectStatement, writer io.Writer) error {
	var code string

	switch stmt.Mode {
	case "ALL":
		// Replace all occurrences
		code = fmt.Sprintf("\t%s = strings.ReplaceAll(%s, %q, %q)\n",
			stmt.Identifier, stmt.Identifier, stmt.Pattern, stmt.Replacement)

	case "LEADING":
		// Replace leading occurrences
		code = fmt.Sprintf(`	for strings.HasPrefix(%s, %q) {
		%s = %q + %s[len(%q):]
	}
`,
			stmt.Identifier, stmt.Pattern,
			stmt.Identifier, stmt.Replacement, stmt.Identifier, stmt.Pattern)

	case "FIRST":
		// Replace first occurrence
		code = fmt.Sprintf("\t%s = strings.Replace(%s, %q, %q, 1)\n",
			stmt.Identifier, stmt.Identifier, stmt.Pattern, stmt.Replacement)
	}

	_, err := writer.Write([]byte(code))
	return err
}

// generateInspectConverting generates Go code for INSPECT CONVERTING
func (g *Generator) generateInspectConverting(stmt *ir.InspectStatement, writer io.Writer) error {
	// CONVERTING is character-by-character replacement
	// e.g., CONVERTING "ABC" TO "XYZ" means A→X, B→Y, C→Z
	code := fmt.Sprintf(`	// INSPECT CONVERTING %q TO %q
	{
		fromChars := %q
		toChars := %q
		var result strings.Builder
		for _, ch := range %s {
			if idx := strings.IndexRune(fromChars, ch); idx != -1 && idx < len(toChars) {
				result.WriteRune(rune(toChars[idx]))
			} else {
				result.WriteRune(ch)
			}
		}
		%s = result.String()
	}
`,
		stmt.Pattern, stmt.Replacement,
		stmt.Pattern, stmt.Replacement,
		stmt.Identifier,
		stmt.Identifier)

	_, err := writer.Write([]byte(code))
	return err
}

// generateStringStatement generates Go code for COBOL STRING statement
func (g *Generator) generateStringStatement(stmt *ir.StringStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\t// STRING statement\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t{\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t\tvar sb strings.Builder\n")
	if err != nil {
		return err
	}

	// Generate concatenation for each source
	for _, source := range stmt.Sources {
		if source.Delimiter == "SIZE" {
			// DELIMITED BY SIZE means use entire field
			_, err = fmt.Fprintf(writer, "\t\tsb.WriteString(%s)\n", source.Source)
		} else {
			// DELIMITED BY literal means stop at delimiter
			_, err = fmt.Fprintf(writer, "\t\tif idx := strings.Index(%s, %q); idx >= 0 {\n",
				source.Source, source.Delimiter)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t\tsb.WriteString(%s[:idx])\n", source.Source)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t} else {\n")
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t\tsb.WriteString(%s)\n", source.Source)
			if err != nil {
				return err
			}
			_, err = fmt.Fprintf(writer, "\t\t}\n")
		}
		if err != nil {
			return err
		}
	}

	// Assign to destination
	_, err = fmt.Fprintf(writer, "\t\t%s = sb.String()\n", stmt.Destination)
	if err != nil {
		return err
	}

	// Handle optional POINTER
	if stmt.Pointer != "" {
		_, err = fmt.Fprintf(writer, "\t\t%s = len(%s)\n", stmt.Pointer, stmt.Destination)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateUnstringStatement generates Go code for COBOL UNSTRING statement
func (g *Generator) generateUnstringStatement(stmt *ir.UnstringStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\t// UNSTRING statement\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t{\n")
	if err != nil {
		return err
	}

	// Build delimiter pattern (handle multiple delimiters)
	delimiters := strings.Join(stmt.Delimiters, "")
	_, err = fmt.Fprintf(writer, "\t\tfields := strings.FieldsFunc(%s, func(r rune) bool {\n",
		stmt.Source)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t\t\treturn strings.ContainsRune(%q, r)\n", delimiters)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t\t})\n")
	if err != nil {
		return err
	}

	// Assign to target fields
	for i, target := range stmt.Targets {
		_, err = fmt.Fprintf(writer, "\t\tif len(fields) > %d {\n", i)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t\t\t%s = fields[%d]\n", target, i)
		if err != nil {
			return err
		}
		_, err = fmt.Fprintf(writer, "\t\t}\n")
		if err != nil {
			return err
		}
	}

	// Handle optional TALLYING
	if stmt.Tallying != "" {
		_, err = fmt.Fprintf(writer, "\t\t%s = len(fields)\n", stmt.Tallying)
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateSearchStatement generates Go code for COBOL SEARCH statement
func (g *Generator) generateSearchStatement(stmt *ir.SearchStatement, writer io.Writer) error {
	if stmt.IsSearchAll {
		return g.generateSearchAll(stmt, writer)
	}
	return g.generateLinearSearch(stmt, writer)
}

// generateLinearSearch generates code for linear SEARCH
func (g *Generator) generateLinearSearch(stmt *ir.SearchStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\t// SEARCH (linear) %s\n", stmt.TableName)
	if err != nil {
		return err
	}

	// Generate loop
	indexVar := stmt.IndexName
	if indexVar == "" {
		indexVar = "searchIdx"
	}

	_, err = fmt.Fprintf(writer, "\tfor %s := 0; %s < len(%s); %s++ {\n",
		indexVar, indexVar, stmt.TableName, indexVar)
	if err != nil {
		return err
	}

	// Generate WHEN clauses
	for i, whenClause := range stmt.WhenClauses {
		if i == 0 {
			_, err = fmt.Fprintf(writer, "\t\tif true { // Condition placeholder\n")
		} else {
			_, err = fmt.Fprintf(writer, "\t\t} else if true { // Condition placeholder\n")
		}
		if err != nil {
			return err
		}

		if whenClause.Body != nil {
			if err := g.generateBlock(whenClause.Body, writer); err != nil {
				return err
			}
		}
		_, err = fmt.Fprintf(writer, "\t\t\tbreak\n")
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t\t}\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}

// generateSearchAll generates code for binary SEARCH ALL
func (g *Generator) generateSearchAll(stmt *ir.SearchStatement, writer io.Writer) error {
	_, err := fmt.Fprintf(writer, "\t// SEARCH ALL (binary) %s\n", stmt.TableName)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t// Binary search implementation\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tlow, high := 0, len(%s)-1\n", stmt.TableName)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\tfor low <= high {\n")
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(writer, "\t\tmid := (low + high) / 2\n")
	if err != nil {
		return err
	}

	// Generate WHEN clause (simplified)
	if len(stmt.WhenClauses) > 0 {
		_, err = fmt.Fprintf(writer, "\t\tif true { // Match condition\n")
		if err != nil {
			return err
		}

		if stmt.WhenClauses[0].Body != nil {
			if err := g.generateBlock(stmt.WhenClauses[0].Body, writer); err != nil {
				return err
			}
		}

		_, err = fmt.Fprintf(writer, "\t\t\tbreak\n")
		if err != nil {
			return err
		}

		_, err = fmt.Fprintf(writer, "\t\t}\n")
		if err != nil {
			return err
		}

		_, err = fmt.Fprintf(writer, "\t\t// Adjust binary search bounds\n")
		if err != nil {
			return err
		}

		_, err = fmt.Fprintf(writer, "\t\tlow = mid + 1 // Placeholder logic\n")
		if err != nil {
			return err
		}
	}

	_, err = fmt.Fprintf(writer, "\t}\n")
	return err
}
