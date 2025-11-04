package runtime

import (
	"fmt"
	"reflect"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

// OccursInfo represents information about an OCCURS clause
type OccursInfo struct {
	MinOccurs int
	MaxOccurs int
	Step      int
	IndexedBy string
}

// RedefinesInfo represents information about a REDEFINES clause
type RedefinesInfo struct {
	RedefinesField string
	Offset         int
	Size           int
}

// ArrayField represents a field with OCCURS clause
type ArrayField struct {
	Field      *ir.Field
	OccursInfo *OccursInfo
	Values     []interface{}
}

// RedefinesField represents a field with REDEFINES clause
type RedefinesField struct {
	Field         *ir.Field
	RedefinesInfo *RedefinesInfo
	Value         interface{}
}

// OccursRedefinesHandler handles OCCURS and REDEFINES operations
type OccursRedefinesHandler struct {
	recordDef *ir.Record
	arrays    map[string]*ArrayField
	redefines map[string]*RedefinesField
}

// NewOccursRedefinesHandler creates a new handler for OCCURS and REDEFINES
func NewOccursRedefinesHandler(recordDef *ir.Record) *OccursRedefinesHandler {
	handler := &OccursRedefinesHandler{
		recordDef: recordDef,
		arrays:    make(map[string]*ArrayField),
		redefines: make(map[string]*RedefinesField),
	}

	// Initialize arrays and redefines from record definition
	handler.initializeFromRecord(recordDef, 0)

	return handler
}

// initializeFromRecord recursively initializes arrays and redefines from record definition
func (orh *OccursRedefinesHandler) initializeFromRecord(recordDef *ir.Record, baseOffset int) {
	for _, field := range recordDef.Fields {
		// Check for OCCURS clause
		if field.Type.IsArray && field.Type.ArraySize != nil {
			occursInfo := &OccursInfo{
				MinOccurs: 1,
				MaxOccurs: *field.Type.ArraySize,
				Step:      1,
			}

			arrayField := &ArrayField{
				Field:      field,
				OccursInfo: occursInfo,
				Values:     make([]interface{}, *field.Type.ArraySize),
			}

			orh.arrays[field.Name] = arrayField
		}

		// Check for REDEFINES clause (would need to be added to IR)
		// For now, we'll handle it through field positioning

		// Handle nested records
		if field.Type.Type == "record" && field.Type.Record != nil {
			orh.initializeFromRecord(field.Type.Record, baseOffset)
		}

		baseOffset += getFieldSize(field.Type)
	}
}

// GetArrayValue gets a value from an array field
func (orh *OccursRedefinesHandler) GetArrayValue(fieldName string, index int) (interface{}, error) {
	arrayField, exists := orh.arrays[fieldName]
	if !exists {
		return nil, fmt.Errorf("field %s is not an array", fieldName)
	}

	if index < 0 || index >= len(arrayField.Values) {
		return nil, fmt.Errorf("array index %d out of bounds for field %s (size: %d)", index, fieldName, len(arrayField.Values))
	}

	return arrayField.Values[index], nil
}

// SetArrayValue sets a value in an array field
func (orh *OccursRedefinesHandler) SetArrayValue(fieldName string, index int, value interface{}) error {
	arrayField, exists := orh.arrays[fieldName]
	if !exists {
		return fmt.Errorf("field %s is not an array", fieldName)
	}

	if index < 0 || index >= len(arrayField.Values) {
		return fmt.Errorf("array index %d out of bounds for field %s (size: %d)", index, fieldName, len(arrayField.Values))
	}

	// Validate value type
	if err := orh.validateValueType(value, arrayField.Field.Type); err != nil {
		return fmt.Errorf("invalid value type for array field %s: %w", fieldName, err)
	}

	arrayField.Values[index] = value
	return nil
}

// GetRedefinesValue gets a value from a redefines field
func (orh *OccursRedefinesHandler) GetRedefinesValue(fieldName string) (interface{}, error) {
	redefinesField, exists := orh.redefines[fieldName]
	if !exists {
		return nil, fmt.Errorf("field %s is not a redefines field", fieldName)
	}

	return redefinesField.Value, nil
}

// SetRedefinesValue sets a value in a redefines field
func (orh *OccursRedefinesHandler) SetRedefinesValue(fieldName string, value interface{}) error {
	redefinesField, exists := orh.redefines[fieldName]
	if !exists {
		return fmt.Errorf("field %s is not a redefines field", fieldName)
	}

	// Validate value type
	if err := orh.validateValueType(value, redefinesField.Field.Type); err != nil {
		return fmt.Errorf("invalid value type for redefines field %s: %w", fieldName, err)
	}

	redefinesField.Value = value
	return nil
}

// GetArraySize returns the size of an array field
func (orh *OccursRedefinesHandler) GetArraySize(fieldName string) (int, error) {
	arrayField, exists := orh.arrays[fieldName]
	if !exists {
		return 0, fmt.Errorf("field %s is not an array", fieldName)
	}

	return len(arrayField.Values), nil
}

// IsArrayField checks if a field is an array
func (orh *OccursRedefinesHandler) IsArrayField(fieldName string) bool {
	_, exists := orh.arrays[fieldName]
	return exists
}

// IsRedefinesField checks if a field is a redefines field
func (orh *OccursRedefinesHandler) IsRedefinesField(fieldName string) bool {
	_, exists := orh.redefines[fieldName]
	return exists
}

// GetArrayFieldInfo returns information about an array field
func (orh *OccursRedefinesHandler) GetArrayFieldInfo(fieldName string) (*ArrayField, error) {
	arrayField, exists := orh.arrays[fieldName]
	if !exists {
		return nil, fmt.Errorf("field %s is not an array", fieldName)
	}

	return arrayField, nil
}

// GetRedefinesFieldInfo returns information about a redefines field
func (orh *OccursRedefinesHandler) GetRedefinesFieldInfo(fieldName string) (*RedefinesField, error) {
	redefinesField, exists := orh.redefines[fieldName]
	if !exists {
		return nil, fmt.Errorf("field %s is not a redefines field", fieldName)
	}

	return redefinesField, nil
}

// validateValueType validates that a value matches the expected field type
func (orh *OccursRedefinesHandler) validateValueType(value interface{}, typeInfo *ir.TypeInfo) error {
	if value == nil {
		return nil // nil values are allowed
	}

	switch typeInfo.Type {
	case "string":
		if _, ok := value.(string); !ok {
			return fmt.Errorf("expected string, got %T", value)
		}
	case "number", "integer":
		switch value.(type) {
		case int, int32, int64, float32, float64:
			return nil
		default:
			return fmt.Errorf("expected number, got %T", value)
		}
	case "decimal":
		// Check if it's a decimal type (would need to import decimal package)
		if reflect.TypeOf(value).String() == "*decimal.Decimal" {
			return nil
		}
		return fmt.Errorf("expected decimal, got %T", value)
	case "boolean":
		if _, ok := value.(bool); !ok {
			return fmt.Errorf("expected boolean, got %T", value)
		}
	case "record":
		if _, ok := value.(map[string]interface{}); !ok {
			return fmt.Errorf("expected record, got %T", value)
		}
	default:
		// For unknown types, allow any value
		return nil
	}

	return nil
}

// ProcessRecordWithOccursRedefines processes a record with OCCURS and REDEFINES handling
func (orh *OccursRedefinesHandler) ProcessRecordWithOccursRedefines(record map[string]interface{}) (map[string]interface{}, error) {
	processedRecord := make(map[string]interface{})

	// Copy non-array, non-redefines fields
	for fieldName, value := range record {
		if !orh.IsArrayField(fieldName) && !orh.IsRedefinesField(fieldName) {
			processedRecord[fieldName] = value
		}
	}

	// Process array fields
	for fieldName, arrayField := range orh.arrays {
		if values, exists := record[fieldName]; exists {
			if array, ok := values.([]interface{}); ok {
				// Copy array values
				for i, value := range array {
					if i < len(arrayField.Values) {
						arrayField.Values[i] = value
					}
				}
			} else {
				// Single value - put it in the first position and clear others
				arrayField.Values[0] = values
				for i := 1; i < len(arrayField.Values); i++ {
					arrayField.Values[i] = nil
				}
			}
		}
		processedRecord[fieldName] = arrayField.Values
	}

	// Process redefines fields
	for fieldName, redefinesField := range orh.redefines {
		if value, exists := record[fieldName]; exists {
			redefinesField.Value = value
			processedRecord[fieldName] = value
		}
	}

	return processedRecord, nil
}

// GetFieldOffset calculates the offset of a field within a record
func (orh *OccursRedefinesHandler) GetFieldOffset(fieldName string) (int, error) {
	offset := 0

	for _, field := range orh.recordDef.Fields {
		if field.Name == fieldName {
			return offset, nil
		}

		fieldSize := getFieldSize(field.Type)
		offset += fieldSize
	}

	return 0, fmt.Errorf("field %s not found in record", fieldName)
}

// GetFieldSize returns the size of a field
func (orh *OccursRedefinesHandler) GetFieldSize(fieldName string) (int, error) {
	for _, field := range orh.recordDef.Fields {
		if field.Name == fieldName {
			return getFieldSize(field.Type), nil
		}
	}

	return 0, fmt.Errorf("field %s not found in record", fieldName)
}
