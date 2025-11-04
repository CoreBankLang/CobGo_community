package runtime

import (
	"fmt"
	"io"
	"os"
	"sort"

	"github.com/cobgo/cobgo-community/pkg/ir"
)

// DatasetType represents the type of dataset
type DatasetType int

const (
	SequentialDataset DatasetType = iota
	IndexedDataset
	RelativeDataset
)

// AccessMode represents the access mode for a dataset
type AccessMode int

const (
	ReadOnly AccessMode = iota
	WriteOnly
	ReadWrite
	Append
)

// Dataset represents a dataset for I/O operations
type Dataset struct {
	Name       string
	Type       DatasetType
	AccessMode AccessMode
	RecordDef  *ir.Record
	File       *os.File
	Reader     *RecordReader
	Writer     *RecordWriter
	Handler    *OccursRedefinesHandler
	Index      map[string]int64 // For indexed datasets
	CurrentPos int64
	IsOpen     bool
}

// IndexEntry represents an entry in the index
type IndexEntry struct {
	Key    string
	Offset int64
	Length int64
}

// DatasetManager manages multiple datasets
type DatasetManager struct {
	datasets map[string]*Dataset
}

// NewDatasetManager creates a new dataset manager
func NewDatasetManager() *DatasetManager {
	return &DatasetManager{
		datasets: make(map[string]*Dataset),
	}
}

// OpenDataset opens a dataset for I/O operations
func (dm *DatasetManager) OpenDataset(name string, datasetType DatasetType, accessMode AccessMode, recordDef *ir.Record, filename string) (*Dataset, error) {
	// Check if dataset is already open
	if dataset, exists := dm.datasets[name]; exists && dataset.IsOpen {
		return nil, fmt.Errorf("dataset %s is already open", name)
	}

	// Open the file
	var file *os.File
	var err error

	switch accessMode {
	case ReadOnly:
		file, err = os.Open(filename)
	case WriteOnly:
		file, err = os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	case ReadWrite:
		file, err = os.OpenFile(filename, os.O_RDWR|os.O_CREATE, 0644)
	case Append:
		file, err = os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0644)
	default:
		return nil, fmt.Errorf("invalid access mode: %d", accessMode)
	}

	if err != nil {
		return nil, fmt.Errorf("failed to open file %s: %w", filename, err)
	}

	// Create record reader/writer
	var reader *RecordReader
	var writer *RecordWriter

	if accessMode == ReadOnly || accessMode == ReadWrite {
		reader, err = NewRecordReader(file, recordDef)
		if err != nil {
			file.Close()
			return nil, fmt.Errorf("failed to create record reader: %w", err)
		}
	}

	if accessMode == WriteOnly || accessMode == ReadWrite || accessMode == Append {
		writer, err = NewRecordWriter(file, recordDef)
		if err != nil {
			file.Close()
			return nil, fmt.Errorf("failed to create record writer: %w", err)
		}
	}

	// Create occurs/redefines handler
	handler := NewOccursRedefinesHandler(recordDef)

	// Create dataset
	dataset := &Dataset{
		Name:       name,
		Type:       datasetType,
		AccessMode: accessMode,
		RecordDef:  recordDef,
		File:       file,
		Reader:     reader,
		Writer:     writer,
		Handler:    handler,
		Index:      make(map[string]int64),
		CurrentPos: 0,
		IsOpen:     true,
	}

	// Build index for indexed datasets (only if file has content)
	if datasetType == IndexedDataset {
		// Check if file has content before building index
		fileInfo, err := file.Stat()
		if err != nil {
			dataset.Close()
			return nil, fmt.Errorf("failed to get file info: %w", err)
		}

		if fileInfo.Size() > 0 {
			err = dataset.buildIndex()
			if err != nil {
				dataset.Close()
				return nil, fmt.Errorf("failed to build index: %w", err)
			}
		}
	}

	dm.datasets[name] = dataset
	return dataset, nil
}

// CloseDataset closes a dataset
func (dm *DatasetManager) CloseDataset(name string) error {
	dataset, exists := dm.datasets[name]
	if !exists {
		return fmt.Errorf("dataset %s not found", name)
	}

	if !dataset.IsOpen {
		return fmt.Errorf("dataset %s is not open", name)
	}

	err := dataset.Close()
	if err != nil {
		return fmt.Errorf("failed to close dataset %s: %w", name, err)
	}

	delete(dm.datasets, name)
	return nil
}

// GetDataset returns a dataset by name
func (dm *DatasetManager) GetDataset(name string) (*Dataset, error) {
	dataset, exists := dm.datasets[name]
	if !exists {
		return nil, fmt.Errorf("dataset %s not found", name)
	}

	if !dataset.IsOpen {
		return nil, fmt.Errorf("dataset %s is not open", name)
	}

	return dataset, nil
}

// Close closes the dataset
func (d *Dataset) Close() error {
	if !d.IsOpen {
		return fmt.Errorf("dataset %s is not open", d.Name)
	}

	err := d.File.Close()
	if err != nil {
		return fmt.Errorf("failed to close file: %w", err)
	}

	d.IsOpen = false
	return nil
}

// ReadRecord reads a record from the dataset
func (d *Dataset) ReadRecord() (map[string]interface{}, error) {
	if d.Reader == nil {
		return nil, fmt.Errorf("dataset %s is not open for reading", d.Name)
	}

	record, err := d.Reader.ReadRecord()
	if err != nil {
		return nil, fmt.Errorf("failed to read record: %w", err)
	}

	// Process with occurs/redefines
	processedRecord, err := d.Handler.ProcessRecordWithOccursRedefines(record)
	if err != nil {
		return nil, fmt.Errorf("failed to process record: %w", err)
	}

	d.CurrentPos++
	return processedRecord, nil
}

// WriteRecord writes a record to the dataset
func (d *Dataset) WriteRecord(record map[string]interface{}) error {
	if d.Writer == nil {
		return fmt.Errorf("dataset %s is not open for writing", d.Name)
	}

	// Process with occurs/redefines
	processedRecord, err := d.Handler.ProcessRecordWithOccursRedefines(record)
	if err != nil {
		return fmt.Errorf("failed to process record: %w", err)
	}

	err = d.Writer.WriteRecord(processedRecord)
	if err != nil {
		return fmt.Errorf("failed to write record: %w", err)
	}

	d.CurrentPos++
	return nil
}

// ReadRecordByKey reads a record by key (for indexed datasets)
func (d *Dataset) ReadRecordByKey(key string) (map[string]interface{}, error) {
	if d.Type != IndexedDataset {
		return nil, fmt.Errorf("dataset %s is not indexed", d.Name)
	}

	offset, exists := d.Index[key]
	if !exists {
		return nil, fmt.Errorf("key %s not found in index", key)
	}

	// Seek to the record position
	_, err := d.File.Seek(offset, 0)
	if err != nil {
		return nil, fmt.Errorf("failed to seek to record: %w", err)
	}

	// Read the record
	record, err := d.ReadRecord()
	if err != nil {
		return nil, fmt.Errorf("failed to read record at key %s: %w", key, err)
	}

	return record, nil
}

// WriteRecordWithKey writes a record with a key (for indexed datasets)
func (d *Dataset) WriteRecordWithKey(key string, record map[string]interface{}) error {
	if d.Type != IndexedDataset {
		return fmt.Errorf("dataset %s is not indexed", d.Name)
	}

	// Get current position
	pos, err := d.File.Seek(0, 1) // Get current position
	if err != nil {
		return fmt.Errorf("failed to get current position: %w", err)
	}

	// Write the record
	err = d.WriteRecord(record)
	if err != nil {
		return fmt.Errorf("failed to write record: %w", err)
	}

	// Update index
	d.Index[key] = pos
	return nil
}

// buildIndex builds an index for indexed datasets
func (d *Dataset) buildIndex() error {
	if d.Type != IndexedDataset {
		return fmt.Errorf("dataset %s is not indexed", d.Name)
	}

	// For simplicity, we'll assume the first field is the key
	// In a real implementation, this would be configurable
	if len(d.RecordDef.Fields) == 0 {
		return fmt.Errorf("record definition has no fields for indexing")
	}

	keyField := d.RecordDef.Fields[0].Name
	offset := int64(0)

	// Read all records and build index
	for {
		// Seek to current position
		_, err := d.File.Seek(offset, 0)
		if err != nil {
			break
		}

		// Read record
		record, err := d.ReadRecord()
		if err != nil {
			if err == io.EOF {
				break
			}
			return fmt.Errorf("failed to read record for indexing: %w", err)
		}

		// Extract key
		keyValue, exists := record[keyField]
		if !exists {
			return fmt.Errorf("key field %s not found in record", keyField)
		}

		key := fmt.Sprintf("%v", keyValue)
		d.Index[key] = offset

		// Move to next record
		offset += int64(d.Reader.GetRecordSize())
	}

	return nil
}

// GetKeys returns all keys in the index
func (d *Dataset) GetKeys() []string {
	keys := make([]string, 0, len(d.Index))
	for key := range d.Index {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	return keys
}

// HasKey checks if a key exists in the index
func (d *Dataset) HasKey(key string) bool {
	_, exists := d.Index[key]
	return exists
}

// GetRecordCount returns the number of records in the dataset
func (d *Dataset) GetRecordCount() int {
	if d.Type == IndexedDataset {
		return len(d.Index)
	}
	return int(d.CurrentPos)
}

// SeekToBeginning seeks to the beginning of the dataset
func (d *Dataset) SeekToBeginning() error {
	_, err := d.File.Seek(0, 0)
	if err != nil {
		return fmt.Errorf("failed to seek to beginning: %w", err)
	}
	d.CurrentPos = 0
	return nil
}

// SeekToEnd seeks to the end of the dataset
func (d *Dataset) SeekToEnd() error {
	_, err := d.File.Seek(0, 2) // Seek to end
	if err != nil {
		return fmt.Errorf("failed to seek to end: %w", err)
	}
	return nil
}

// IsEOF checks if we're at the end of the dataset
func (d *Dataset) IsEOF() bool {
	// Try to read one byte to check if we're at EOF
	pos, _ := d.File.Seek(0, 1) // Get current position
	fileInfo, err := d.File.Stat()
	if err != nil {
		return true
	}
	return pos >= fileInfo.Size()
}

// GetCurrentPosition returns the current position in the dataset
func (d *Dataset) GetCurrentPosition() int64 {
	return d.CurrentPos
}

// GetDatasetInfo returns information about the dataset
func (d *Dataset) GetDatasetInfo() map[string]interface{} {
	info := map[string]interface{}{
		"name":        d.Name,
		"type":        d.Type,
		"access_mode": d.AccessMode,
		"is_open":     d.IsOpen,
		"current_pos": d.CurrentPos,
		"record_size": d.Reader.GetRecordSize(),
	}

	if d.Type == IndexedDataset {
		info["index_size"] = len(d.Index)
		info["keys"] = d.GetKeys()
	}

	return info
}
