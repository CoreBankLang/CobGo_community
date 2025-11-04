package runtime

import (
	"testing"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

func TestOccursRedefinesHandler(t *testing.T) {
	// Create a test record definition with array fields
	recordDef := &ir.Record{
		Name: "TestRecord",
		Fields: []*ir.Field{
			{
				Name: "ID",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "Scores",
				Type: &ir.TypeInfo{
					Type:      "integer",
					Size:      intPtr(4),
					IsArray:   true,
					ArraySize: intPtr(5),
				},
			},
			{
				Name: "Names",
				Type: &ir.TypeInfo{
					Type:      "string",
					Size:      intPtr(20),
					IsArray:   true,
					ArraySize: intPtr(3),
				},
			},
		},
	}

	// Create handler
	handler := NewOccursRedefinesHandler(recordDef)

	// Test array operations
	t.Run("ArrayOperations", func(t *testing.T) {
		// Test setting array values
		err := handler.SetArrayValue("Scores", 0, 100)
		if err != nil {
			t.Fatalf("Failed to set array value: %v", err)
		}

		err = handler.SetArrayValue("Scores", 1, 200)
		if err != nil {
			t.Fatalf("Failed to set array value: %v", err)
		}

		err = handler.SetArrayValue("Scores", 2, 300)
		if err != nil {
			t.Fatalf("Failed to set array value: %v", err)
		}

		// Test getting array values
		value, err := handler.GetArrayValue("Scores", 0)
		if err != nil {
			t.Fatalf("Failed to get array value: %v", err)
		}
		if value != 100 {
			t.Errorf("Expected Scores[0]=100, got %v", value)
		}

		value, err = handler.GetArrayValue("Scores", 1)
		if err != nil {
			t.Fatalf("Failed to get array value: %v", err)
		}
		if value != 200 {
			t.Errorf("Expected Scores[1]=200, got %v", value)
		}

		// Test array size
		size, err := handler.GetArraySize("Scores")
		if err != nil {
			t.Fatalf("Failed to get array size: %v", err)
		}
		if size != 5 {
			t.Errorf("Expected array size 5, got %d", size)
		}

		// Test string array
		err = handler.SetArrayValue("Names", 0, "Alice")
		if err != nil {
			t.Fatalf("Failed to set string array value: %v", err)
		}

		err = handler.SetArrayValue("Names", 1, "Bob")
		if err != nil {
			t.Fatalf("Failed to set string array value: %v", err)
		}

		value, err = handler.GetArrayValue("Names", 0)
		if err != nil {
			t.Fatalf("Failed to get string array value: %v", err)
		}
		if value != "Alice" {
			t.Errorf("Expected Names[0]='Alice', got '%v'", value)
		}
	})

	// Test error cases
	t.Run("ErrorCases", func(t *testing.T) {
		// Test out of bounds access
		_, err := handler.GetArrayValue("Scores", 10)
		if err == nil {
			t.Error("Expected error for out of bounds access")
		}

		err = handler.SetArrayValue("Scores", 10, 999)
		if err == nil {
			t.Error("Expected error for out of bounds access")
		}

		// Test non-array field
		_, err = handler.GetArrayValue("ID", 0)
		if err == nil {
			t.Error("Expected error for non-array field")
		}

		err = handler.SetArrayValue("ID", 0, 999)
		if err == nil {
			t.Error("Expected error for non-array field")
		}

		// Test invalid field
		_, err = handler.GetArrayValue("NonExistent", 0)
		if err == nil {
			t.Error("Expected error for non-existent field")
		}
	})

	// Test field type validation
	t.Run("TypeValidation", func(t *testing.T) {
		// Test valid type
		err := handler.SetArrayValue("Scores", 0, 42)
		if err != nil {
			t.Errorf("Expected no error for valid integer type, got %v", err)
		}

		// Test invalid type
		err = handler.SetArrayValue("Scores", 0, "not a number")
		if err == nil {
			t.Error("Expected error for invalid type")
		}
	})

	// Test field checks
	t.Run("FieldChecks", func(t *testing.T) {
		if !handler.IsArrayField("Scores") {
			t.Error("Expected Scores to be an array field")
		}

		if handler.IsArrayField("ID") {
			t.Error("Expected ID to not be an array field")
		}

		if handler.IsArrayField("NonExistent") {
			t.Error("Expected NonExistent to not be an array field")
		}
	})
}

func TestOccursRedefinesWithNestedRecords(t *testing.T) {
	// Create nested record definition
	nestedRecord := &ir.Record{
		Name: "Address",
		Fields: []*ir.Field{
			{
				Name: "Street",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(30),
				},
			},
			{
				Name: "City",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(20),
				},
			},
		},
	}

	// Create main record with array of nested records
	recordDef := &ir.Record{
		Name: "PersonRecord",
		Fields: []*ir.Field{
			{
				Name: "ID",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "Addresses",
				Type: &ir.TypeInfo{
					Type:      "record",
					Record:    nestedRecord,
					IsArray:   true,
					ArraySize: intPtr(3),
				},
			},
		},
	}

	// Create handler
	handler := NewOccursRedefinesHandler(recordDef)

	// Test array of records
	t.Run("ArrayOfRecords", func(t *testing.T) {
		// Set address records
		address1 := map[string]interface{}{
			"Street": "123 Main St",
			"City":   "New York",
		}

		address2 := map[string]interface{}{
			"Street": "456 Oak Ave",
			"City":   "Los Angeles",
		}

		err := handler.SetArrayValue("Addresses", 0, address1)
		if err != nil {
			t.Fatalf("Failed to set address record: %v", err)
		}

		err = handler.SetArrayValue("Addresses", 1, address2)
		if err != nil {
			t.Fatalf("Failed to set address record: %v", err)
		}

		// Get address records
		value, err := handler.GetArrayValue("Addresses", 0)
		if err != nil {
			t.Fatalf("Failed to get address record: %v", err)
		}

		addr, ok := value.(map[string]interface{})
		if !ok {
			t.Fatalf("Expected address to be map[string]interface{}, got %T", value)
		}

		if addr["Street"] != "123 Main St" {
			t.Errorf("Expected Street='123 Main St', got '%v'", addr["Street"])
		}

		if addr["City"] != "New York" {
			t.Errorf("Expected City='New York', got '%v'", addr["City"])
		}
	})
}

func TestOccursRedefinesProcessRecord(t *testing.T) {
	// Create a test record definition with array fields
	recordDef := &ir.Record{
		Name: "ProcessTestRecord",
		Fields: []*ir.Field{
			{
				Name: "ID",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "Scores",
				Type: &ir.TypeInfo{
					Type:      "integer",
					Size:      intPtr(4),
					IsArray:   true,
					ArraySize: intPtr(3),
				},
			},
		},
	}

	// Create handler
	handler := NewOccursRedefinesHandler(recordDef)

	// Test processing record with array data
	t.Run("ProcessRecordWithArrays", func(t *testing.T) {
		inputRecord := map[string]interface{}{
			"ID":     123,
			"Scores": []interface{}{100, 200, 300},
		}

		processedRecord, err := handler.ProcessRecordWithOccursRedefines(inputRecord)
		if err != nil {
			t.Fatalf("Failed to process record: %v", err)
		}

		// Verify ID field
		if processedRecord["ID"] != 123 {
			t.Errorf("Expected ID=123, got %v", processedRecord["ID"])
		}

		// Verify Scores array
		scores, ok := processedRecord["Scores"].([]interface{})
		if !ok {
			t.Errorf("Expected Scores to be []interface{}, got %T", processedRecord["Scores"])
		} else {
			if len(scores) != 3 {
				t.Errorf("Expected Scores length 3, got %d", len(scores))
			}

			if scores[0] != 100 {
				t.Errorf("Expected Scores[0]=100, got %v", scores[0])
			}

			if scores[1] != 200 {
				t.Errorf("Expected Scores[1]=200, got %v", scores[1])
			}

			if scores[2] != 300 {
				t.Errorf("Expected Scores[2]=300, got %v", scores[2])
			}
		}
	})

	// Test processing record with single value for array
	t.Run("ProcessRecordWithSingleValue", func(t *testing.T) {
		inputRecord := map[string]interface{}{
			"ID":     456,
			"Scores": 999, // Single value instead of array
		}

		processedRecord, err := handler.ProcessRecordWithOccursRedefines(inputRecord)
		if err != nil {
			t.Fatalf("Failed to process record: %v", err)
		}

		// Verify ID field
		if processedRecord["ID"] != 456 {
			t.Errorf("Expected ID=456, got %v", processedRecord["ID"])
		}

		// Verify Scores array (should have single value in first position)
		scores, ok := processedRecord["Scores"].([]interface{})
		if !ok {
			t.Errorf("Expected Scores to be []interface{}, got %T", processedRecord["Scores"])
		} else {
			if len(scores) != 3 {
				t.Errorf("Expected Scores length 3, got %d", len(scores))
			}

			if scores[0] != 999 {
				t.Errorf("Expected Scores[0]=999, got %v", scores[0])
			}

			// Other positions should be nil
			if scores[1] != nil {
				t.Errorf("Expected Scores[1]=nil, got %v", scores[1])
			}

			if scores[2] != nil {
				t.Errorf("Expected Scores[2]=nil, got %v", scores[2])
			}
		}
	})
}

func TestOccursRedefinesFieldInfo(t *testing.T) {
	// Create a test record definition
	recordDef := &ir.Record{
		Name: "FieldInfoTestRecord",
		Fields: []*ir.Field{
			{
				Name: "ID",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "Scores",
				Type: &ir.TypeInfo{
					Type:      "integer",
					Size:      intPtr(4),
					IsArray:   true,
					ArraySize: intPtr(5),
				},
			},
		},
	}

	// Create handler
	handler := NewOccursRedefinesHandler(recordDef)

	// Test getting array field info
	t.Run("GetArrayFieldInfo", func(t *testing.T) {
		arrayField, err := handler.GetArrayFieldInfo("Scores")
		if err != nil {
			t.Fatalf("Failed to get array field info: %v", err)
		}

		if arrayField.Field.Name != "Scores" {
			t.Errorf("Expected field name 'Scores', got '%s'", arrayField.Field.Name)
		}

		if arrayField.OccursInfo.MaxOccurs != 5 {
			t.Errorf("Expected max occurs 5, got %d", arrayField.OccursInfo.MaxOccurs)
		}

		if len(arrayField.Values) != 5 {
			t.Errorf("Expected values length 5, got %d", len(arrayField.Values))
		}
	})

	// Test getting field offset and size
	t.Run("GetFieldOffsetAndSize", func(t *testing.T) {
		offset, err := handler.GetFieldOffset("ID")
		if err != nil {
			t.Fatalf("Failed to get field offset: %v", err)
		}
		if offset != 0 {
			t.Errorf("Expected ID offset 0, got %d", offset)
		}

		offset, err = handler.GetFieldOffset("Scores")
		if err != nil {
			t.Fatalf("Failed to get field offset: %v", err)
		}
		if offset != 4 { // ID field size
			t.Errorf("Expected Scores offset 4, got %d", offset)
		}

		size, err := handler.GetFieldSize("ID")
		if err != nil {
			t.Fatalf("Failed to get field size: %v", err)
		}
		if size != 4 {
			t.Errorf("Expected ID size 4, got %d", size)
		}

		size, err = handler.GetFieldSize("Scores")
		if err != nil {
			t.Fatalf("Failed to get field size: %v", err)
		}
		if size != 4 {
			t.Errorf("Expected Scores size 4, got %d", size)
		}
	})
}
