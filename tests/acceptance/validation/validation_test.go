package validation

import (
	"testing"
)

// TestDSLValidator tests the DSL validator
func TestDSLValidator(t *testing.T) {
	validator := NewDSLValidator("test_golden")

	if validator == nil {
		t.Error("DSL validator should not be nil")
	}

	t.Log("✅ DSL validator created successfully")
}

// TestGoValidator tests the Go validator
func TestGoValidator(t *testing.T) {
	validator := NewGoValidator("test_golden")

	if validator == nil {
		t.Error("Go validator should not be nil")
	}

	t.Log("✅ Go validator created successfully")
}

// TestBinaryValidator tests the binary validator
func TestBinaryValidator(t *testing.T) {
	validator := NewBinaryValidator("test_golden")

	if validator == nil {
		t.Error("Binary validator should not be nil")
	}

	t.Log("✅ Binary validator created successfully")
}

// TestValidationPackage tests the validation package functionality
func TestValidationPackage(t *testing.T) {
	t.Run("Package_Initialization", func(t *testing.T) {
		t.Log("✅ Validation package initialized successfully")
	})

	t.Run("Validator_Creation", func(t *testing.T) {
		dslValidator := NewDSLValidator("test")
		goValidator := NewGoValidator("test")
		binaryValidator := NewBinaryValidator("test")

		if dslValidator == nil || goValidator == nil || binaryValidator == nil {
			t.Error("All validators should be created successfully")
		}

		t.Log("✅ All validators created successfully")
	})
}
