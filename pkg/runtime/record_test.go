package runtime

import (
	"bytes"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/decimal"
	"github.com/cobgo/cobgo-community/pkg/ir"
)

func TestRecordReaderWriter(t *testing.T) {
	// Create a test record definition
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
				Name: "Name",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(20),
				},
			},
			{
				Name: "Amount",
				Type: &ir.TypeInfo{
					Type: "decimal",
					Size: intPtr(10),
				},
			},
			{
				Name: "Active",
				Type: &ir.TypeInfo{
					Type: "boolean",
				},
			},
		},
	}

	// Test data
	amount, _ := decimal.NewFromString("123.45")
	testRecord := map[string]interface{}{
		"ID":     123,
		"Name":   "John Doe",
		"Amount": amount,
		"Active": true,
	}

	// Test writing
	var buf bytes.Buffer
	writer, err := NewRecordWriter(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	err = writer.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record: %v", err)
	}

	// Test reading
	reader, err := NewRecordReader(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record reader: %v", err)
	}

	readRecord, err := reader.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record: %v", err)
	}

	// Verify the data
	if readRecord["ID"] != 123 {
		t.Errorf("Expected ID=123, got %v", readRecord["ID"])
	}

	if readRecord["Name"] != "John Doe" {
		t.Errorf("Expected Name='John Doe', got '%v'", readRecord["Name"])
	}

	// Check decimal amount
	amount, ok := readRecord["Amount"].(*decimal.Decimal)
	if !ok {
		t.Errorf("Expected Amount to be *decimal.Decimal, got %T", readRecord["Amount"])
	} else {
		expectedAmount, _ := decimal.NewFromString("123.45")
		if !amount.Equals(expectedAmount) {
			t.Errorf("Expected Amount=123.45, got %s", amount.String())
		}
	}

	if readRecord["Active"] != true {
		t.Errorf("Expected Active=true, got %v", readRecord["Active"])
	}
}

func TestRecordWithNestedRecord(t *testing.T) {
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

	// Create main record with nested record
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
				Name: "Address",
				Type: &ir.TypeInfo{
					Type:   "record",
					Record: nestedRecord,
				},
			},
		},
	}

	// Test data
	testRecord := map[string]interface{}{
		"ID": 456,
		"Address": map[string]interface{}{
			"Street": "123 Main St",
			"City":   "New York",
		},
	}

	// Test writing
	var buf bytes.Buffer
	writer, err := NewRecordWriter(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	err = writer.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record: %v", err)
	}

	// Test reading
	reader, err := NewRecordReader(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record reader: %v", err)
	}

	readRecord, err := reader.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record: %v", err)
	}

	// Verify the data
	if readRecord["ID"] != 456 {
		t.Errorf("Expected ID=456, got %v", readRecord["ID"])
	}

	address, ok := readRecord["Address"].(map[string]interface{})
	if !ok {
		t.Errorf("Expected Address to be map[string]interface{}, got %T", readRecord["Address"])
	} else {
		if address["Street"] != "123 Main St" {
			t.Errorf("Expected Street='123 Main St', got '%v'", address["Street"])
		}
		if address["City"] != "New York" {
			t.Errorf("Expected City='New York', got '%v'", address["City"])
		}
	}
}

func TestRecordSizeCalculation(t *testing.T) {
	recordDef := &ir.Record{
		Name: "SizeTestRecord",
		Fields: []*ir.Field{
			{
				Name: "Field1",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(10),
				},
			},
			{
				Name: "Field2",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "Field3",
				Type: &ir.TypeInfo{
					Type: "boolean",
				},
			},
		},
	}

	expectedSize := 10 + 4 + 1 // 15 bytes total

	writer, err := NewRecordWriter(&bytes.Buffer{}, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	actualSize := writer.GetRecordSize()
	if actualSize != expectedSize {
		t.Errorf("Expected record size %d, got %d", expectedSize, actualSize)
	}
}

func TestRecordWithMissingFields(t *testing.T) {
	recordDef := &ir.Record{
		Name: "MissingFieldsRecord",
		Fields: []*ir.Field{
			{
				Name: "Field1",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(10),
				},
			},
			{
				Name: "Field2",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(10),
				},
			},
		},
	}

	// Test data with missing field
	testRecord := map[string]interface{}{
		"Field1": "Value1",
		// Field2 is missing
	}

	// Test writing
	var buf bytes.Buffer
	writer, err := NewRecordWriter(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	err = writer.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record: %v", err)
	}

	// Test reading
	reader, err := NewRecordReader(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record reader: %v", err)
	}

	readRecord, err := reader.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record: %v", err)
	}

	// Verify the data
	if readRecord["Field1"] != "Value1" {
		t.Errorf("Expected Field1='Value1', got '%v'", readRecord["Field1"])
	}

	// Field2 should be empty string (spaces)
	if readRecord["Field2"] != "" {
		t.Errorf("Expected Field2='', got '%v'", readRecord["Field2"])
	}
}

func TestRecordWithEmptyString(t *testing.T) {
	recordDef := &ir.Record{
		Name: "EmptyStringRecord",
		Fields: []*ir.Field{
			{
				Name: "EmptyField",
				Type: &ir.TypeInfo{
					Type: "string",
					Size: intPtr(10),
				},
			},
		},
	}

	// Test data with empty string
	testRecord := map[string]interface{}{
		"EmptyField": "",
	}

	// Test writing
	var buf bytes.Buffer
	writer, err := NewRecordWriter(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	err = writer.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record: %v", err)
	}

	// Test reading
	reader, err := NewRecordReader(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record reader: %v", err)
	}

	readRecord, err := reader.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record: %v", err)
	}

	// Verify the data
	if readRecord["EmptyField"] != "" {
		t.Errorf("Expected EmptyField='', got '%v'", readRecord["EmptyField"])
	}
}

func TestRecordWithZeroValues(t *testing.T) {
	recordDef := &ir.Record{
		Name: "ZeroValuesRecord",
		Fields: []*ir.Field{
			{
				Name: "ZeroInt",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
			{
				Name: "ZeroDecimal",
				Type: &ir.TypeInfo{
					Type: "decimal",
					Size: intPtr(10),
				},
			},
			{
				Name: "FalseBool",
				Type: &ir.TypeInfo{
					Type: "boolean",
				},
			},
		},
	}

	// Test data with zero values
	testRecord := map[string]interface{}{
		"ZeroInt":     0,
		"ZeroDecimal": decimal.Zero(),
		"FalseBool":   false,
	}

	// Test writing
	var buf bytes.Buffer
	writer, err := NewRecordWriter(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record writer: %v", err)
	}

	err = writer.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record: %v", err)
	}

	// Test reading
	reader, err := NewRecordReader(&buf, recordDef)
	if err != nil {
		t.Fatalf("Failed to create record reader: %v", err)
	}

	readRecord, err := reader.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record: %v", err)
	}

	// Verify the data
	if readRecord["ZeroInt"] != 0 {
		t.Errorf("Expected ZeroInt=0, got %v", readRecord["ZeroInt"])
	}

	zeroDecimal, ok := readRecord["ZeroDecimal"].(*decimal.Decimal)
	if !ok {
		t.Errorf("Expected ZeroDecimal to be *decimal.Decimal, got %T", readRecord["ZeroDecimal"])
	} else {
		if !zeroDecimal.IsZero() {
			t.Errorf("Expected ZeroDecimal to be zero, got %s", zeroDecimal.String())
		}
	}

	if readRecord["FalseBool"] != false {
		t.Errorf("Expected FalseBool=false, got %v", readRecord["FalseBool"])
	}
}
