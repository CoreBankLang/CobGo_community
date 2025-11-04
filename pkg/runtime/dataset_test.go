package runtime

import (
	"os"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/decimal"
	"github.com/cobgo/cobgo-community/pkg/ir"
)

func TestDatasetManager(t *testing.T) {
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
		},
	}

	// Create dataset manager
	dm := NewDatasetManager()

	// Test opening a dataset
	t.Run("OpenDataset", func(t *testing.T) {
		// Create a temporary file
		tmpFile, err := os.CreateTemp("", "test_dataset_*.dat")
		if err != nil {
			t.Fatalf("Failed to create temp file: %v", err)
		}
		defer os.Remove(tmpFile.Name())
		tmpFile.Close()

		// Open dataset for writing
		dataset, err := dm.OpenDataset("test", SequentialDataset, WriteOnly, recordDef, tmpFile.Name())
		if err != nil {
			t.Fatalf("Failed to open dataset: %v", err)
		}

		if dataset == nil {
			t.Fatal("Expected dataset, got nil")
		}

		if dataset.Name != "test" {
			t.Errorf("Expected dataset name 'test', got '%s'", dataset.Name)
		}

		if dataset.Type != SequentialDataset {
			t.Errorf("Expected SequentialDataset, got %v", dataset.Type)
		}

		if dataset.AccessMode != WriteOnly {
			t.Errorf("Expected WriteOnly, got %v", dataset.AccessMode)
		}

		if !dataset.IsOpen {
			t.Error("Expected dataset to be open")
		}

		// Close dataset
		err = dataset.Close()
		if err != nil {
			t.Fatalf("Failed to close dataset: %v", err)
		}

		if dataset.IsOpen {
			t.Error("Expected dataset to be closed")
		}
	})

	// Test dataset operations
	t.Run("DatasetOperations", func(t *testing.T) {
		// Create a temporary file
		tmpFile, err := os.CreateTemp("", "test_dataset_ops_*.dat")
		if err != nil {
			t.Fatalf("Failed to create temp file: %v", err)
		}
		defer os.Remove(tmpFile.Name())
		tmpFile.Close()

		// Open dataset for read/write
		dataset, err := dm.OpenDataset("test_ops", SequentialDataset, ReadWrite, recordDef, tmpFile.Name())
		if err != nil {
			t.Fatalf("Failed to open dataset: %v", err)
		}
		defer dataset.Close()

		// Test writing records
		amount1, _ := decimal.NewFromString("100.50")
		amount2, _ := decimal.NewFromString("200.75")
		amount3, _ := decimal.NewFromString("300.25")

		testRecords := []map[string]interface{}{
			{
				"ID":     1,
				"Name":   "Alice",
				"Amount": amount1,
			},
			{
				"ID":     2,
				"Name":   "Bob",
				"Amount": amount2,
			},
			{
				"ID":     3,
				"Name":   "Charlie",
				"Amount": amount3,
			},
		}

		for _, record := range testRecords {
			err = dataset.WriteRecord(record)
			if err != nil {
				t.Fatalf("Failed to write record: %v", err)
			}
		}

		// Test reading records
		err = dataset.SeekToBeginning()
		if err != nil {
			t.Fatalf("Failed to seek to beginning: %v", err)
		}

		for i, expectedRecord := range testRecords {
			record, err := dataset.ReadRecord()
			if err != nil {
				t.Fatalf("Failed to read record %d: %v", i, err)
			}

			// Verify ID
			if record["ID"] != expectedRecord["ID"] {
				t.Errorf("Record %d: Expected ID=%v, got %v", i, expectedRecord["ID"], record["ID"])
			}

			// Verify Name
			if record["Name"] != expectedRecord["Name"] {
				t.Errorf("Record %d: Expected Name='%v', got '%v'", i, expectedRecord["Name"], record["Name"])
			}

			// Verify Amount
			expectedAmount := expectedRecord["Amount"].(*decimal.Decimal)
			actualAmount, ok := record["Amount"].(*decimal.Decimal)
			if !ok {
				t.Errorf("Record %d: Expected Amount to be *decimal.Decimal, got %T", i, record["Amount"])
			} else if !actualAmount.Equals(expectedAmount) {
				t.Errorf("Record %d: Expected Amount=%s, got %s", i, expectedAmount.String(), actualAmount.String())
			}
		}

		// Test EOF
		_, err = dataset.ReadRecord()
		if err == nil {
			t.Error("Expected EOF error")
		}

		// Test record count
		count := dataset.GetRecordCount()
		if count != 3 {
			t.Errorf("Expected record count 3, got %d", count)
		}
	})

	// Test indexed dataset
	t.Run("IndexedDataset", func(t *testing.T) {
		// Create a temporary file
		tmpFile, err := os.CreateTemp("", "test_indexed_*.dat")
		if err != nil {
			t.Fatalf("Failed to create temp file: %v", err)
		}
		defer os.Remove(tmpFile.Name())
		tmpFile.Close()

		// Open indexed dataset
		dataset, err := dm.OpenDataset("test_indexed", IndexedDataset, ReadWrite, recordDef, tmpFile.Name())
		if err != nil {
			t.Fatalf("Failed to open indexed dataset: %v", err)
		}
		defer dataset.Close()

		// Write records with keys
		amount1, _ := decimal.NewFromString("100.50")
		amount2, _ := decimal.NewFromString("200.75")
		amount3, _ := decimal.NewFromString("300.25")

		testRecords := map[string]map[string]interface{}{
			"001": {
				"ID":     1,
				"Name":   "Alice",
				"Amount": amount1,
			},
			"002": {
				"ID":     2,
				"Name":   "Bob",
				"Amount": amount2,
			},
			"003": {
				"ID":     3,
				"Name":   "Charlie",
				"Amount": amount3,
			},
		}

		for key, record := range testRecords {
			err = dataset.WriteRecordWithKey(key, record)
			if err != nil {
				t.Fatalf("Failed to write record with key %s: %v", key, err)
			}
		}

		// Test reading records by key
		for key, expectedRecord := range testRecords {
			record, err := dataset.ReadRecordByKey(key)
			if err != nil {
				t.Fatalf("Failed to read record with key %s: %v", key, err)
			}

			// Verify ID
			if record["ID"] != expectedRecord["ID"] {
				t.Errorf("Key %s: Expected ID=%v, got %v", key, expectedRecord["ID"], record["ID"])
			}

			// Verify Name
			if record["Name"] != expectedRecord["Name"] {
				t.Errorf("Key %s: Expected Name='%v', got '%v'", key, expectedRecord["Name"], record["Name"])
			}
		}

		// Test key operations
		keys := dataset.GetKeys()
		if len(keys) != 3 {
			t.Errorf("Expected 3 keys, got %d", len(keys))
		}

		if !dataset.HasKey("001") {
			t.Error("Expected key '001' to exist")
		}

		if dataset.HasKey("999") {
			t.Error("Expected key '999' to not exist")
		}

		// Test reading non-existent key
		_, err = dataset.ReadRecordByKey("999")
		if err == nil {
			t.Error("Expected error for non-existent key")
		}
	})

	// Test dataset manager operations
	t.Run("DatasetManagerOperations", func(t *testing.T) {
		// Create a temporary file
		tmpFile, err := os.CreateTemp("", "test_manager_*.dat")
		if err != nil {
			t.Fatalf("Failed to create temp file: %v", err)
		}
		defer os.Remove(tmpFile.Name())
		tmpFile.Close()

		// Open dataset
		dataset, err := dm.OpenDataset("test_manager", SequentialDataset, ReadWrite, recordDef, tmpFile.Name())
		if err != nil {
			t.Fatalf("Failed to open dataset: %v", err)
		}

		// Test getting dataset
		retrievedDataset, err := dm.GetDataset("test_manager")
		if err != nil {
			t.Fatalf("Failed to get dataset: %v", err)
		}

		if retrievedDataset != dataset {
			t.Error("Expected retrieved dataset to be the same as original")
		}

		// Test getting non-existent dataset
		_, err = dm.GetDataset("non_existent")
		if err == nil {
			t.Error("Expected error for non-existent dataset")
		}

		// Test closing dataset through manager
		err = dm.CloseDataset("test_manager")
		if err != nil {
			t.Fatalf("Failed to close dataset: %v", err)
		}

		// Test getting closed dataset
		_, err = dm.GetDataset("test_manager")
		if err == nil {
			t.Error("Expected error for closed dataset")
		}
	})
}

func TestDatasetWithArrays(t *testing.T) {
	// Create a test record definition with array fields
	recordDef := &ir.Record{
		Name: "ArrayRecord",
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
					Size:      intPtr(12), // 3 elements * 4 bytes each
					IsArray:   true,
					ArraySize: intPtr(3),
				},
			},
		},
	}

	// Create dataset manager
	dm := NewDatasetManager()

	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "test_array_*.dat")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpFile.Name())
	tmpFile.Close()

	// Open dataset
	dataset, err := dm.OpenDataset("test_array", SequentialDataset, ReadWrite, recordDef, tmpFile.Name())
	if err != nil {
		t.Fatalf("Failed to open dataset: %v", err)
	}
	defer dataset.Close()

	// Test writing record with array
	testRecord := map[string]interface{}{
		"ID":     1,
		"Scores": []interface{}{100, 200, 300},
	}

	err = dataset.WriteRecord(testRecord)
	if err != nil {
		t.Fatalf("Failed to write record with array: %v", err)
	}

	// Test reading record with array
	err = dataset.SeekToBeginning()
	if err != nil {
		t.Fatalf("Failed to seek to beginning: %v", err)
	}

	record, err := dataset.ReadRecord()
	if err != nil {
		t.Fatalf("Failed to read record with array: %v", err)
	}

	// Verify ID
	if record["ID"] != 1 {
		t.Errorf("Expected ID=1, got %v", record["ID"])
	}

	// Verify Scores array
	scores, ok := record["Scores"].([]interface{})
	if !ok {
		t.Errorf("Expected Scores to be []interface{}, got %T", record["Scores"])
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
}

func TestDatasetInfo(t *testing.T) {
	// Create a test record definition
	recordDef := &ir.Record{
		Name: "InfoTestRecord",
		Fields: []*ir.Field{
			{
				Name: "ID",
				Type: &ir.TypeInfo{
					Type: "integer",
					Size: intPtr(4),
				},
			},
		},
	}

	// Create dataset manager
	dm := NewDatasetManager()

	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "test_info_*.dat")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpFile.Name())
	tmpFile.Close()

	// Open dataset
	dataset, err := dm.OpenDataset("test_info", SequentialDataset, ReadWrite, recordDef, tmpFile.Name())
	if err != nil {
		t.Fatalf("Failed to open dataset: %v", err)
	}
	defer dataset.Close()

	// Test dataset info
	info := dataset.GetDatasetInfo()

	if info["name"] != "test_info" {
		t.Errorf("Expected name 'test_info', got '%v'", info["name"])
	}

	if info["type"] != SequentialDataset {
		t.Errorf("Expected type SequentialDataset, got %v", info["type"])
	}

	if info["access_mode"] != ReadWrite {
		t.Errorf("Expected access_mode ReadWrite, got %v", info["access_mode"])
	}

	if info["is_open"] != true {
		t.Errorf("Expected is_open true, got %v", info["is_open"])
	}

	if info["current_pos"] != int64(0) {
		t.Errorf("Expected current_pos 0, got %v (type: %T)", info["current_pos"], info["current_pos"])
	}

	if info["record_size"] != 4 {
		t.Errorf("Expected record_size 4, got %v", info["record_size"])
	}
}
