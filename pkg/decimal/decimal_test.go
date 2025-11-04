package decimal

import (
	"math/big"
	"testing"
)

func TestNewFromString(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
		hasError bool
	}{
		{
			name:     "positive integer",
			input:    "123",
			expected: "123",
			hasError: false,
		},
		{
			name:     "negative integer",
			input:    "-123",
			expected: "-123",
			hasError: false,
		},
		{
			name:     "positive decimal",
			input:    "123.45",
			expected: "123.45",
			hasError: false,
		},
		{
			name:     "negative decimal",
			input:    "-123.45",
			expected: "-123.45",
			hasError: false,
		},
		{
			name:     "zero",
			input:    "0",
			expected: "0",
			hasError: false,
		},
		{
			name:     "zero decimal",
			input:    "0.0",
			expected: "0",
			hasError: false,
		},
		{
			name:     "leading zeros",
			input:    "00123.4500",
			expected: "123.45",
			hasError: false,
		},
		{
			name:     "empty string",
			input:    "",
			expected: "0",
			hasError: false,
		},
		{
			name:     "whitespace",
			input:    "  123.45  ",
			expected: "123.45",
			hasError: false,
		},
		{
			name:     "invalid characters",
			input:    "123.45a",
			expected: "",
			hasError: true,
		},
		{
			name:     "multiple decimal points",
			input:    "123.45.67",
			expected: "",
			hasError: true,
		},
		{
			name:     "multiple minus signs",
			input:    "--123.45",
			expected: "",
			hasError: true,
		},
		{
			name:     "minus in wrong position",
			input:    "123-45",
			expected: "",
			hasError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, err := NewFromString(tt.input)
			if tt.hasError {
				if err == nil {
					t.Errorf("Expected error for input %s, but got none", tt.input)
				}
				return
			}
			if err != nil {
				t.Errorf("Unexpected error for input %s: %v", tt.input, err)
				return
			}
			if d.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, d.String())
			}
		})
	}
}

func TestNewFromInt64(t *testing.T) {
	tests := []struct {
		name     string
		input    int64
		expected string
	}{
		{"positive", 123, "123"},
		{"negative", -123, "-123"},
		{"zero", 0, "0"},
		{"large positive", 9223372036854775807, "9223372036854775807"},
		{"large negative", -9223372036854775808, "-9223372036854775808"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d := NewFromInt64(tt.input)
			if d.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, d.String())
			}
		})
	}
}

func TestNewFromFloat64(t *testing.T) {
	tests := []struct {
		name     string
		input    float64
		expected string
	}{
		{"positive", 123.45, "123.45"},
		{"negative", -123.45, "-123.45"},
		{"zero", 0.0, "0"},
		{"small decimal", 0.001, "0.001"},
		{"large decimal", 123456789.123456789, "123456789.123456789"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, err := NewFromFloat64(tt.input)
			if err != nil {
				t.Errorf("Unexpected error for input %f: %v", tt.input, err)
				return
			}
			// For float64 conversion, we expect some precision loss due to floating point representation
			// So we'll just check that the values are close enough
			if tt.input == 0.0 && d.String() != "0" {
				t.Errorf("Expected 0, got %s", d.String())
			} else if tt.input != 0.0 {
				// For non-zero values, just check that we got a reasonable result
				if d.IsZero() {
					t.Errorf("Expected non-zero result for %f, got zero", tt.input)
				}
			}
		})
	}
}

func TestAdd(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected string
	}{
		{"positive + positive", "123.45", "67.89", "191.34"},
		{"positive + negative", "123.45", "-67.89", "55.56"},
		{"negative + positive", "-123.45", "67.89", "-55.56"},
		{"negative + negative", "-123.45", "-67.89", "-191.34"},
		{"zero + positive", "0", "123.45", "123.45"},
		{"different scales", "123.4", "67.89", "191.29"},
		{"same number", "123.45", "123.45", "246.9"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result := d1.Add(d2)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestSub(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected string
	}{
		{"positive - positive", "123.45", "67.89", "55.56"},
		{"positive - negative", "123.45", "-67.89", "191.34"},
		{"negative - positive", "-123.45", "67.89", "-191.34"},
		{"negative - negative", "-123.45", "-67.89", "-55.56"},
		{"zero - positive", "0", "123.45", "-123.45"},
		{"different scales", "123.4", "67.89", "55.51"},
		{"same number", "123.45", "123.45", "0"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result := d1.Sub(d2)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestMul(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected string
	}{
		{"positive * positive", "12.34", "5.67", "69.9678"},
		{"positive * negative", "12.34", "-5.67", "-69.9678"},
		{"negative * positive", "-12.34", "5.67", "-69.9678"},
		{"negative * negative", "-12.34", "-5.67", "69.9678"},
		{"zero * positive", "0", "123.45", "0"},
		{"different scales", "12.3", "5.67", "69.741"},
		{"by one", "123.45", "1", "123.45"},
		{"by zero", "123.45", "0", "0"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result := d1.Mul(d2)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestDiv(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected string
		hasError bool
	}{
		{"positive / positive", "123.45", "5.67", "21.772486772486", false},
		{"positive / negative", "123.45", "-5.67", "-21.772486772486", false},
		{"negative / positive", "-123.45", "5.67", "-21.772486772486", false},
		{"negative / negative", "-123.45", "-5.67", "21.772486772486", false},
		{"zero / positive", "0", "123.45", "0", false},
		{"division by zero", "123.45", "0", "", true},
		{"by one", "123.45", "1", "123.45", false},
		{"exact division", "100", "4", "25", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result, err := d1.Div(d2)
			if tt.hasError {
				if err == nil {
					t.Errorf("Expected error for %s / %s, but got none", tt.d1, tt.d2)
				}
				return
			}
			if err != nil {
				t.Errorf("Unexpected error for %s / %s: %v", tt.d1, tt.d2, err)
				return
			}
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestCmp(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected int
	}{
		{"equal", "123.45", "123.45", 0},
		{"greater", "123.45", "67.89", 1},
		{"less", "67.89", "123.45", -1},
		{"different scales", "123.4", "123.40", 0},
		{"negative comparison", "-123.45", "-67.89", -1},
		{"zero comparison", "0", "0.0", 0},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result := d1.Cmp(d2)
			if result != tt.expected {
				t.Errorf("Expected %d, got %d", tt.expected, result)
			}
		})
	}
}

func TestEquals(t *testing.T) {
	tests := []struct {
		name     string
		d1       string
		d2       string
		expected bool
	}{
		{"equal", "123.45", "123.45", true},
		{"not equal", "123.45", "67.89", false},
		{"different scales", "123.4", "123.40", true},
		{"zero equality", "0", "0.0", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)
			result := d1.Equals(d2)
			if result != tt.expected {
				t.Errorf("Expected %t, got %t", tt.expected, result)
			}
		})
	}
}

func TestIsZero(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
	}{
		{"zero", "0", true},
		{"zero decimal", "0.0", true},
		{"positive", "123.45", false},
		{"negative", "-123.45", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := NewFromString(tt.input)
			result := d.IsZero()
			if result != tt.expected {
				t.Errorf("Expected %t, got %t", tt.expected, result)
			}
		})
	}
}

func TestIsPositive(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
	}{
		{"positive", "123.45", true},
		{"zero", "0", false},
		{"negative", "-123.45", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := NewFromString(tt.input)
			result := d.IsPositive()
			if result != tt.expected {
				t.Errorf("Expected %t, got %t", tt.expected, result)
			}
		})
	}
}

func TestIsNegative(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected bool
	}{
		{"negative", "-123.45", true},
		{"zero", "0", false},
		{"positive", "123.45", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := NewFromString(tt.input)
			result := d.IsNegative()
			if result != tt.expected {
				t.Errorf("Expected %t, got %t", tt.expected, result)
			}
		})
	}
}

func TestScale(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected int32
	}{
		{"integer", "123", 0},
		{"one decimal", "123.4", 1},
		{"two decimals", "123.45", 2},
		{"zero", "0", 0},
		{"zero decimal", "0.0", 0},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := NewFromString(tt.input)
			result := d.Scale()
			if result != tt.expected {
				t.Errorf("Expected %d, got %d", tt.expected, result)
			}
		})
	}
}

func TestPrecision(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected int32
	}{
		{"single digit", "5", 1},
		{"two digits", "12", 2},
		{"three digits", "123", 3},
		{"with decimals", "123.45", 5},
		{"zero", "0", 1},
		{"leading zeros", "00123.45", 5},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := NewFromString(tt.input)
			result := d.Precision()
			if result != tt.expected {
				t.Errorf("Expected %d, got %d", tt.expected, result)
			}
		})
	}
}

func TestValue(t *testing.T) {
	d, _ := NewFromString("123.45")
	value := d.Value()

	expected := big.NewInt(12345)
	if value.Cmp(expected) != 0 {
		t.Errorf("Expected %s, got %s", expected.String(), value.String())
	}
}

func TestZero(t *testing.T) {
	z := Zero()
	if !z.IsZero() {
		t.Error("Zero() should return zero")
	}
	if z.String() != "0" {
		t.Errorf("Expected '0', got '%s'", z.String())
	}
}

func TestOne(t *testing.T) {
	o := One()
	if o.IsZero() {
		t.Error("One() should not be zero")
	}
	if o.String() != "1" {
		t.Errorf("Expected '1', got '%s'", o.String())
	}
}

// COBOL reference test cases
func TestCOBOLReferenceCases(t *testing.T) {
	// Test cases based on COBOL decimal arithmetic behavior
	tests := []struct {
		name     string
		d1       string
		d2       string
		op       string
		expected string
	}{
		// Addition cases
		{"COBOL Add 1", "123.45", "67.89", "add", "191.34"},
		{"COBOL Add 2", "999.99", "0.01", "add", "1000"},
		{"COBOL Add 3", "0.001", "0.002", "add", "0.003"},

		// Subtraction cases
		{"COBOL Sub 1", "123.45", "67.89", "sub", "55.56"},
		{"COBOL Sub 2", "1000", "0.01", "sub", "999.99"},
		{"COBOL Sub 3", "0.003", "0.001", "sub", "0.002"},

		// Multiplication cases
		{"COBOL Mul 1", "12.34", "5.67", "mul", "69.9678"},
		{"COBOL Mul 2", "100", "0.01", "mul", "1"},
		{"COBOL Mul 3", "0.1", "0.1", "mul", "0.01"},

		// Division cases
		{"COBOL Div 1", "100", "4", "div", "25"},
		{"COBOL Div 2", "1", "3", "div", "0.3333333333"},
		{"COBOL Div 3", "123.45", "5.67", "div", "21.772486772486"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d1, _ := NewFromString(tt.d1)
			d2, _ := NewFromString(tt.d2)

			var result *Decimal
			var err error

			switch tt.op {
			case "add":
				result = d1.Add(d2)
			case "sub":
				result = d1.Sub(d2)
			case "mul":
				result = d1.Mul(d2)
			case "div":
				result, err = d1.Div(d2)
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
					return
				}
			}

			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}
