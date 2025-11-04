package runtime

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/decimal"
	"github.com/cobgo/cobgo-community/pkg/ir"
)

// RecordReader provides functionality to read COBOL-style records
type RecordReader struct {
	reader     io.Reader
	recordDef  *ir.Record
	buffer     []byte
	recordSize int
}

// RecordWriter provides functionality to write COBOL-style records
type RecordWriter struct {
	writer     io.Writer
	recordDef  *ir.Record
	buffer     []byte
	recordSize int
}

// FieldValue represents a field value with its type information
type FieldValue struct {
	Name     string
	Value    interface{}
	TypeInfo *ir.TypeInfo
	Position int
	Size     int
}

// NewRecordReader creates a new record reader
func NewRecordReader(reader io.Reader, recordDef *ir.Record) (*RecordReader, error) {
	recordSize, err := calculateRecordSize(recordDef)
	if err != nil {
		return nil, fmt.Errorf("failed to calculate record size: %w", err)
	}

	return &RecordReader{
		reader:     reader,
		recordDef:  recordDef,
		buffer:     make([]byte, recordSize),
		recordSize: recordSize,
	}, nil
}

// NewRecordWriter creates a new record writer
func NewRecordWriter(writer io.Writer, recordDef *ir.Record) (*RecordWriter, error) {
	recordSize, err := calculateRecordSize(recordDef)
	if err != nil {
		return nil, fmt.Errorf("failed to calculate record size: %w", err)
	}

	return &RecordWriter{
		writer:     writer,
		recordDef:  recordDef,
		buffer:     make([]byte, recordSize),
		recordSize: recordSize,
	}, nil
}

// ReadRecord reads a single record from the input stream
func (rr *RecordReader) ReadRecord() (map[string]interface{}, error) {
	// Read the record buffer
	n, err := io.ReadFull(rr.reader, rr.buffer)
	if err != nil {
		if err == io.EOF {
			return nil, io.EOF
		}
		return nil, fmt.Errorf("failed to read record: %w", err)
	}

	if n != rr.recordSize {
		return nil, fmt.Errorf("incomplete record read: expected %d bytes, got %d", rr.recordSize, n)
	}

	// Parse the record into field values
	record := make(map[string]interface{})
	offset := 0

	for _, field := range rr.recordDef.Fields {
		fieldSize := getFieldSize(field.Type)
		if offset+fieldSize > len(rr.buffer) {
			return nil, fmt.Errorf("field %s exceeds record boundary", field.Name)
		}

		fieldData := rr.buffer[offset : offset+fieldSize]
		value, err := parseFieldValue(fieldData, field.Type)
		if err != nil {
			return nil, fmt.Errorf("failed to parse field %s: %w", field.Name, err)
		}

		record[field.Name] = value
		offset += fieldSize
	}

	return record, nil
}

// WriteRecord writes a single record to the output stream
func (rw *RecordWriter) WriteRecord(record map[string]interface{}) error {
	// Clear the buffer
	for i := range rw.buffer {
		rw.buffer[i] = 0
	}

	offset := 0
	for _, field := range rw.recordDef.Fields {
		fieldSize := getFieldSize(field.Type)
		if offset+fieldSize > len(rw.buffer) {
			return fmt.Errorf("field %s exceeds record boundary", field.Name)
		}

		value, exists := record[field.Name]
		if !exists {
			// Fill with spaces for missing fields
			for i := 0; i < fieldSize; i++ {
				rw.buffer[offset+i] = ' '
			}
		} else {
			fieldData, err := formatFieldValue(value, field.Type, fieldSize)
			if err != nil {
				return fmt.Errorf("failed to format field %s: %w", field.Name, err)
			}

			copy(rw.buffer[offset:offset+fieldSize], fieldData)
		}

		offset += fieldSize
	}

	// Write the record buffer
	_, err := rw.writer.Write(rw.buffer)
	if err != nil {
		return fmt.Errorf("failed to write record: %w", err)
	}

	return nil
}

// calculateRecordSize calculates the total size of a record in bytes
func calculateRecordSize(recordDef *ir.Record) (int, error) {
	totalSize := 0

	for _, field := range recordDef.Fields {
		fieldSize := getFieldSize(field.Type)
		totalSize += fieldSize
	}

	return totalSize, nil
}

// getFieldSize returns the size in bytes for a field type
func getFieldSize(typeInfo *ir.TypeInfo) int {
	switch typeInfo.Type {
	case "string":
		if typeInfo.Size != nil {
			return *typeInfo.Size
		}
		return 1 // Default size
	case "number":
		if typeInfo.Size != nil {
			return *typeInfo.Size
		}
		return 4 // Default size for numbers
	case "decimal":
		if typeInfo.Size != nil {
			return *typeInfo.Size
		}
		return 8 // Default size for decimals
	case "integer":
		if typeInfo.Size != nil {
			return *typeInfo.Size
		}
		return 4 // Default size for integers
	case "boolean":
		return 1
	case "record":
		if typeInfo.Record != nil {
			size, _ := calculateRecordSize(typeInfo.Record)
			return size
		}
		return 0
	default:
		return 1 // Default size
	}
}

// parseFieldValue parses a field value from raw bytes based on its type
func parseFieldValue(data []byte, typeInfo *ir.TypeInfo) (interface{}, error) {
	// Handle array fields
	if typeInfo.IsArray {
		// Calculate element size
		elementSize := len(data) / *typeInfo.ArraySize
		if elementSize == 0 {
			elementSize = 1
		}

		array := make([]interface{}, *typeInfo.ArraySize)
		offset := 0

		for i := 0; i < *typeInfo.ArraySize; i++ {
			if offset+elementSize > len(data) {
				break
			}

			elementData := data[offset : offset+elementSize]
			value, err := parseFieldValue(elementData, &ir.TypeInfo{
				Type: typeInfo.Type,
				Size: &elementSize,
			})
			if err != nil {
				return nil, fmt.Errorf("failed to parse array element %d: %w", i, err)
			}

			array[i] = value
			offset += elementSize
		}

		return array, nil
	}

	switch typeInfo.Type {
	case "string":
		// Remove trailing spaces and null bytes
		str := strings.TrimRight(string(data), " \x00")
		return str, nil

	case "number", "integer":
		// Parse as integer
		str := strings.TrimSpace(string(data))
		if str == "" {
			return 0, nil
		}
		return strconv.Atoi(str)

	case "decimal":
		// Parse as decimal
		str := strings.TrimSpace(string(data))
		if str == "" {
			return decimal.Zero(), nil
		}
		return decimal.NewFromString(str)

	case "boolean":
		// Parse as boolean (1 = true, 0 = false)
		if len(data) > 0 && data[0] == '1' {
			return true, nil
		}
		return false, nil

	case "record":
		// Parse as nested record
		if typeInfo.Record != nil {
			record := make(map[string]interface{})
			offset := 0

			for _, field := range typeInfo.Record.Fields {
				fieldSize := getFieldSize(field.Type)
				if offset+fieldSize > len(data) {
					return nil, fmt.Errorf("nested record field %s exceeds boundary", field.Name)
				}

				fieldData := data[offset : offset+fieldSize]
				value, err := parseFieldValue(fieldData, field.Type)
				if err != nil {
					return nil, fmt.Errorf("failed to parse nested field %s: %w", field.Name, err)
				}

				record[field.Name] = value
				offset += fieldSize
			}

			return record, nil
		}
		return nil, fmt.Errorf("record type without record definition")

	default:
		// Return as string by default
		return strings.TrimRight(string(data), " \x00"), nil
	}
}

// formatFieldValue formats a field value into bytes based on its type
func formatFieldValue(value interface{}, typeInfo *ir.TypeInfo, fieldSize int) ([]byte, error) {
	data := make([]byte, fieldSize)

	// Handle array fields
	if typeInfo.IsArray {
		// For arrays, we need to format each element
		array, ok := value.([]interface{})
		if !ok {
			return nil, fmt.Errorf("invalid array type: %T", value)
		}

		// Calculate element size
		elementSize := fieldSize / *typeInfo.ArraySize
		if elementSize == 0 {
			elementSize = 1
		}

		offset := 0
		for i, element := range array {
			if i >= *typeInfo.ArraySize {
				break
			}

			elementData, err := formatFieldValue(element, &ir.TypeInfo{
				Type: typeInfo.Type,
				Size: &elementSize,
			}, elementSize)
			if err != nil {
				return nil, fmt.Errorf("failed to format array element %d: %w", i, err)
			}

			copy(data[offset:offset+elementSize], elementData)
			offset += elementSize
		}

		// Fill remaining space with spaces
		for i := offset; i < fieldSize; i++ {
			data[i] = ' '
		}

		return data, nil
	}

	switch typeInfo.Type {
	case "string":
		str := fmt.Sprintf("%v", value)
		if len(str) > fieldSize {
			str = str[:fieldSize]
		}
		copy(data, str)
		// Pad with spaces
		for i := len(str); i < fieldSize; i++ {
			data[i] = ' '
		}

	case "number", "integer":
		var num int
		switch v := value.(type) {
		case int:
			num = v
		case int64:
			num = int(v)
		case float64:
			num = int(v)
		default:
			return nil, fmt.Errorf("invalid number type: %T", value)
		}
		str := strconv.Itoa(num)
		if len(str) > fieldSize {
			str = str[:fieldSize]
		}
		copy(data, str)
		// Pad with spaces
		for i := len(str); i < fieldSize; i++ {
			data[i] = ' '
		}

	case "decimal":
		var dec *decimal.Decimal
		switch v := value.(type) {
		case *decimal.Decimal:
			dec = v
		case decimal.Decimal:
			dec = &v
		default:
			return nil, fmt.Errorf("invalid decimal type: %T", value)
		}
		str := dec.String()
		if len(str) > fieldSize {
			str = str[:fieldSize]
		}
		copy(data, str)
		// Pad with spaces
		for i := len(str); i < fieldSize; i++ {
			data[i] = ' '
		}

	case "boolean":
		var b bool
		switch v := value.(type) {
		case bool:
			b = v
		default:
			return nil, fmt.Errorf("invalid boolean type: %T", value)
		}
		if b {
			data[0] = '1'
		} else {
			data[0] = '0'
		}
		// Pad with spaces
		for i := 1; i < fieldSize; i++ {
			data[i] = ' '
		}

	case "record":
		// Format as nested record
		if typeInfo.Record != nil {
			record, ok := value.(map[string]interface{})
			if !ok {
				return nil, fmt.Errorf("invalid record type: %T", value)
			}

			offset := 0
			for _, field := range typeInfo.Record.Fields {
				fieldSize := getFieldSize(field.Type)
				if offset+fieldSize > len(data) {
					return nil, fmt.Errorf("nested record field %s exceeds boundary", field.Name)
				}

				fieldValue, exists := record[field.Name]
				if !exists {
					// Fill with spaces for missing fields
					for i := 0; i < fieldSize; i++ {
						data[offset+i] = ' '
					}
				} else {
					fieldData, err := formatFieldValue(fieldValue, field.Type, fieldSize)
					if err != nil {
						return nil, fmt.Errorf("failed to format nested field %s: %w", field.Name, err)
					}
					copy(data[offset:offset+fieldSize], fieldData)
				}

				offset += fieldSize
			}
		}

	default:
		// Format as string by default
		str := fmt.Sprintf("%v", value)
		if len(str) > fieldSize {
			str = str[:fieldSize]
		}
		copy(data, str)
		// Pad with spaces
		for i := len(str); i < fieldSize; i++ {
			data[i] = ' '
		}
	}

	return data, nil
}

// GetRecordSize returns the calculated record size
func (rr *RecordReader) GetRecordSize() int {
	return rr.recordSize
}

// GetRecordSize returns the calculated record size
func (rw *RecordWriter) GetRecordSize() int {
	return rw.recordSize
}

// GetRecordDefinition returns the record definition
func (rr *RecordReader) GetRecordDefinition() *ir.Record {
	return rr.recordDef
}

// GetRecordDefinition returns the record definition
func (rw *RecordWriter) GetRecordDefinition() *ir.Record {
	return rw.recordDef
}
