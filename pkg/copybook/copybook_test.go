package copybook

import (
	"strings"
	"testing"
)

func TestConverter(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name: "simple record",
			input: `01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC 9(10).
   05 CUSTOMER-NAME   PIC X(50).
   05 CUSTOMER-BALANCE PIC 9(15)V99.`,
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record CustomerRecord {
    CustomerId int64
    CustomerName string(50)
    CustomerBalance decimal(17,2)
}

`,
		},
		{
			name: "record with occurs",
			input: `01 ORDER-RECORD.
   05 ORDER-ID       PIC 9(10).
   05 ORDER-ITEMS    PIC X(20) OCCURS 10.`,
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record OrderRecord {
    OrderId int64
    OrderItems string(20)[10]
}

`,
		},
		{
			name: "record with redefines",
			input: `01 PAYMENT-RECORD.
   05 PAYMENT-TYPE   PIC X(1).
   05 PAYMENT-AMOUNT PIC 9(10)V99.
   05 CASH-PAYMENT   REDEFINES PAYMENT-AMOUNT PIC 9(10)V99.`,
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record PaymentRecord {
    PaymentType string(1)
    PaymentAmount decimal(12,2)
    CashPayment decimal(12,2)
}

`,
		},
		{
			name: "nested record structure",
			input: `01 EMPLOYEE-RECORD.
   05 EMPLOYEE-ID     PIC 9(10).
   05 EMPLOYEE-NAME   PIC X(50).
   05 EMPLOYEE-ADDRESS.
      10 STREET       PIC X(30).
      10 CITY         PIC X(20).
      10 ZIP-CODE     PIC X(10).`,
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record EmployeeRecord {
    EmployeeId int64
    EmployeeName string(50)
    EmployeeAddress
        Street string(30)
        City string(20)
        ZipCode string(10)
}

`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			converter := NewConverter(nil)
			result, err := converter.Convert(strings.NewReader(tt.input))
			if err != nil {
				t.Fatalf("Convert failed: %v", err)
			}

			if result != tt.expected {
				t.Errorf("Convert() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestParsePIC(t *testing.T) {
	converter := NewConverter(nil)

	tests := []struct {
		name           string
		pic            string
		expectedType   string
		expectedLength int
		expectedPrec   int
		expectedScale  int
	}{
		{
			name:           "integer",
			pic:            "999",
			expectedType:   "int32",
			expectedLength: 3,
			expectedPrec:   0,
			expectedScale:  0,
		},
		{
			name:           "signed integer",
			pic:            "S999",
			expectedType:   "int32",
			expectedLength: 3,
			expectedPrec:   0,
			expectedScale:  0,
		},
		{
			name:           "decimal",
			pic:            "999V99",
			expectedType:   "decimal",
			expectedLength: 5,
			expectedPrec:   5,
			expectedScale:  2,
		},
		{
			name:           "signed decimal",
			pic:            "S999V99",
			expectedType:   "decimal",
			expectedLength: 5,
			expectedPrec:   5,
			expectedScale:  2,
		},
		{
			name:           "string",
			pic:            "X(20)",
			expectedType:   "string",
			expectedLength: 20,
			expectedPrec:   0,
			expectedScale:  0,
		},
		{
			name:           "alphabetic",
			pic:            "A(10)",
			expectedType:   "string",
			expectedLength: 10,
			expectedPrec:   0,
			expectedScale:  0,
		},
		{
			name:           "long integer",
			pic:            "9999999999",
			expectedType:   "int64",
			expectedLength: 10,
			expectedPrec:   0,
			expectedScale:  0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fieldType, length, precision, scale := converter.parsePIC(tt.pic)

			if fieldType != tt.expectedType {
				t.Errorf("parsePIC(%q) type = %q, want %q", tt.pic, fieldType, tt.expectedType)
			}
			if length != tt.expectedLength {
				t.Errorf("parsePIC(%q) length = %d, want %d", tt.pic, length, tt.expectedLength)
			}
			if precision != tt.expectedPrec {
				t.Errorf("parsePIC(%q) precision = %d, want %d", tt.pic, precision, tt.expectedPrec)
			}
			if scale != tt.expectedScale {
				t.Errorf("parsePIC(%q) scale = %d, want %d", tt.pic, scale, tt.expectedScale)
			}
		})
	}
}

func TestParseLine(t *testing.T) {
	converter := NewConverter(nil)

	tests := []struct {
		name     string
		line     string
		expected Field
	}{
		{
			name: "simple field",
			line: "05 CUSTOMER-ID PIC 9(10).",
			expected: Field{
				Level:  5,
				Name:   "CUSTOMER-ID",
				PIC:    "9(10)",
				Type:   "int64",
				Length: 10,
			},
		},
		{
			name: "field with occurs",
			line: "05 ORDER-ITEMS PIC X(20) OCCURS 10.",
			expected: Field{
				Level:  5,
				Name:   "ORDER-ITEMS",
				PIC:    "X(20)",
				Type:   "string",
				Length: 20,
				Occurs: 10,
			},
		},
		{
			name: "field with redefines",
			line: "05 CASH-PAYMENT REDEFINES PAYMENT-AMOUNT PIC 9(10)V99.",
			expected: Field{
				Level:     5,
				Name:      "CASH-PAYMENT",
				PIC:       "9(10)V99",
				Type:      "decimal",
				Length:    12,
				Precision: 12,
				Scale:     2,
				Redefines: "PAYMENT-AMOUNT",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := converter.parseLine(tt.line, 1)
			if err != nil {
				t.Fatalf("parseLine failed: %v", err)
			}

			if result.Level != tt.expected.Level {
				t.Errorf("parseLine() level = %d, want %d", result.Level, tt.expected.Level)
			}
			if result.Name != tt.expected.Name {
				t.Errorf("parseLine() name = %q, want %q", result.Name, tt.expected.Name)
			}
			if result.Type != tt.expected.Type {
				t.Errorf("parseLine() type = %q, want %q", result.Type, tt.expected.Type)
			}
			if result.Length != tt.expected.Length {
				t.Errorf("parseLine() length = %d, want %d", result.Length, tt.expected.Length)
			}
			if result.Occurs != tt.expected.Occurs {
				t.Errorf("parseLine() occurs = %d, want %d", result.Occurs, tt.expected.Occurs)
			}
			if result.Redefines != tt.expected.Redefines {
				t.Errorf("parseLine() redefines = %q, want %q", result.Redefines, tt.expected.Redefines)
			}
		})
	}
}

func TestFormatRecordName(t *testing.T) {
	tests := []struct {
		name     string
		options  *Options
		input    string
		expected string
	}{
		{
			name:     "no prefix or suffix",
			options:  &Options{},
			input:    "CUSTOMER-RECORD",
			expected: "CUSTOMER-RECORD",
		},
		{
			name: "with prefix",
			options: &Options{
				Prefix: "Tst",
			},
			input:    "CUSTOMER-RECORD",
			expected: "TstCUSTOMER-RECORD",
		},
		{
			name: "with suffix",
			options: &Options{
				Suffix: "Type",
			},
			input:    "CUSTOMER-RECORD",
			expected: "CUSTOMER-RECORDType",
		},
		{
			name: "with prefix and suffix",
			options: &Options{
				Prefix: "Tst",
				Suffix: "Type",
			},
			input:    "CUSTOMER-RECORD",
			expected: "TstCUSTOMER-RECORDType",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			converter := NewConverter(tt.options)
			result := converter.formatRecordName(tt.input)

			if result != tt.expected {
				t.Errorf("formatRecordName() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestFormatFieldType(t *testing.T) {
	converter := NewConverter(nil)

	tests := []struct {
		name     string
		field    Field
		expected string
	}{
		{
			name: "decimal with precision and scale",
			field: Field{
				Type:      "decimal",
				Precision: 15,
				Scale:     2,
			},
			expected: "decimal(15,2)",
		},
		{
			name: "string with length",
			field: Field{
				Type:   "string",
				Length: 50,
			},
			expected: "string(50)",
		},
		{
			name: "int32",
			field: Field{
				Type: "int32",
			},
			expected: "int32",
		},
		{
			name: "int64",
			field: Field{
				Type: "int64",
			},
			expected: "int64",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := converter.formatFieldType(tt.field)

			if result != tt.expected {
				t.Errorf("formatFieldType() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestConverterWithOptions(t *testing.T) {
	input := `01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC 9(10).
   05 CUSTOMER-NAME   PIC X(50).`

	tests := []struct {
		name     string
		options  *Options
		expected string
	}{
		{
			name: "default options",
			options: &Options{
				Format: "dsl",
			},
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record CustomerRecord {
    CustomerId int64
    CustomerName string(50)
}

`,
		},
		{
			name: "with prefix and suffix",
			options: &Options{
				Prefix: "Tst",
				Suffix: "Type",
				Format: "dsl",
			},
			expected: `// Generated from COBOL copybook
// Converted by CobGO copybook2dsl

record TstCustomerRecordType {
    CustomerId int64
    CustomerName string(50)
}

`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			converter := NewConverter(tt.options)
			result, err := converter.Convert(strings.NewReader(input))
			if err != nil {
				t.Fatalf("Convert failed: %v", err)
			}

			if result != tt.expected {
				t.Errorf("Convert() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestConverterErrorHandling(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "empty input",
			input: "",
		},
		{
			name:  "invalid line format",
			input: "invalid line format",
		},
		{
			name:  "invalid level",
			input: "XX CUSTOMER-ID PIC 9(10).",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			converter := NewConverter(nil)
			_, err := converter.Convert(strings.NewReader(tt.input))
			// Should not fail for these inputs as we skip invalid lines
			if err != nil {
				t.Errorf("Convert() should not fail for input: %q", tt.input)
			}
		})
	}
}
