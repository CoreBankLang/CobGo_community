package formatter

import (
	"testing"
)

func TestFormatter(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name: "simple job",
			input: `job TestJob {
var count int32 = 10
step Main {
display("Hello")
}
}`,
			expected: `job TestJob {
    var count int32 = 10

    step Main {
        display("Hello")
    }
}
`,
		},
		{
			name: "record definition",
			input: `record Customer {
id int32
name string(50)
balance decimal(15,2)
}`,
			expected: `record Customer {
    id int32
    name string(50)
    balance decimal(15,2)
}
`,
		},
		{
			name: "complex job with multiple steps",
			input: `job CustomerManagement {
var customer Customer
step Initialize {
customer.id = 1
customer.name = "John"
}
step Process {
display("Processing: " + customer.name)
}
}`,
			expected: `job CustomerManagement {
    var customer Customer

    step Initialize {
        customer.id = 1
        customer.name = "John"
    }

    step Process {
        display("Processing: " + customer.name)
    }
}
`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			formatter := New(nil)
			result, err := formatter.Format(tt.input)
			if err != nil {
				t.Fatalf("Format failed: %v", err)
			}

			if result != tt.expected {
				t.Errorf("Format() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestFormatterWithOptions(t *testing.T) {
	input := `job Test {
var count int32
step Main {
display("Hello")
}
}`

	tests := []struct {
		name     string
		options  *Options
		expected string
	}{
		{
			name: "default options",
			options: &Options{
				TabWidth: 4,
				UseTabs:  false,
			},
			expected: `job Test {
    var count int32

    step Main {
        display("Hello")
    }
}
`,
		},
		{
			name: "tabs instead of spaces",
			options: &Options{
				TabWidth: 4,
				UseTabs:  true,
			},
			expected: `job Test {
	var count int32

	step Main {
		display("Hello")
	}
}
`,
		},
		{
			name: "custom tab width",
			options: &Options{
				TabWidth: 2,
				UseTabs:  false,
			},
			expected: `job Test {
  var count int32

  step Main {
    display("Hello")
  }
}
`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			formatter := New(tt.options)
			result, err := formatter.Format(input)
			if err != nil {
				t.Fatalf("Format failed: %v", err)
			}

			if result != tt.expected {
				t.Errorf("Format() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestFormatterErrorHandling(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{
			name:  "invalid syntax",
			input: `job Test { invalid syntax here`,
		},
		{
			name:  "empty input",
			input: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			formatter := New(nil)
			_, err := formatter.Format(tt.input)
			if err == nil {
				t.Errorf("Format() should have failed for input: %q", tt.input)
			}
		})
	}
}

// TestFormatType is disabled - the formatType method does not exist
// Type formatting is tested through integration tests with formatTypeAnnotation
/*
func TestFormatType(t *testing.T) {
	formatter := New(nil)

	tests := []struct {
		name     string
		typeInfo *TypeInfo
		expected string
	}{
		{
			name: "int32",
			typeInfo: &TypeInfo{
				Type: TypeInt32,
			},
			expected: "int32",
		},
		{
			name: "string with length",
			typeInfo: &TypeInfo{
				Type:   TypeString,
				Length: 50,
			},
			expected: "string(50)",
		},
		{
			name: "decimal with precision and scale",
			typeInfo: &TypeInfo{
				Type:      TypeDecimal,
				Precision: 15,
				Scale:     2,
			},
			expected: "decimal(15,2)",
		},
		{
			name: "array of strings",
			typeInfo: &TypeInfo{
				Type:          TypeArray,
				ElementType:   TypeString,
				ElementLength: 20,
				Length:        10,
			},
			expected: "string(20)[10]",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := formatter.formatType(tt.typeInfo)
			if result != tt.expected {
				t.Errorf("formatType() = %q, want %q", result, tt.expected)
			}
		})
	}
}
*/

// Mock TypeInfo for testing
type TypeInfo struct {
	Type             Type
	Length           int
	Precision        int
	Scale            int
	ElementType      Type
	ElementLength    int
	ElementPrecision int
	ElementScale     int
}

type Type int

const (
	TypeInt32 Type = iota
	TypeInt64
	TypeString
	TypeDecimal
	TypeBool
	TypeArray
)
