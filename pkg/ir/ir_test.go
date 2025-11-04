package ir

import (
	"testing"
)

func TestNewProgram(t *testing.T) {
	program := NewProgram("test")
	if program.Name != "test" {
		t.Errorf("Expected program name 'test', got '%s'", program.Name)
	}
	if len(program.Jobs) != 0 {
		t.Errorf("Expected empty jobs slice, got %d jobs", len(program.Jobs))
	}
	if len(program.Records) != 0 {
		t.Errorf("Expected empty records slice, got %d records", len(program.Records))
	}
}

func TestAddJob(t *testing.T) {
	program := NewProgram("test")
	job := &Job{Name: "TestJob"}

	program.AddJob(job)

	if len(program.Jobs) != 1 {
		t.Errorf("Expected 1 job, got %d", len(program.Jobs))
	}
	if program.Jobs[0].Name != "TestJob" {
		t.Errorf("Expected job name 'TestJob', got '%s'", program.Jobs[0].Name)
	}
}

func TestAddRecord(t *testing.T) {
	program := NewProgram("test")
	record := &Record{Name: "TestRecord"}

	program.AddRecord(record)

	if len(program.Records) != 1 {
		t.Errorf("Expected 1 record, got %d", len(program.Records))
	}
	if program.Records[0].Name != "TestRecord" {
		t.Errorf("Expected record name 'TestRecord', got '%s'", program.Records[0].Name)
	}
}

func TestFindJob(t *testing.T) {
	program := NewProgram("test")
	job1 := &Job{Name: "Job1"}
	job2 := &Job{Name: "Job2"}

	program.AddJob(job1)
	program.AddJob(job2)

	found := program.FindJob("Job1")
	if found == nil {
		t.Error("Expected to find Job1, got nil")
	}
	if found.Name != "Job1" {
		t.Errorf("Expected job name 'Job1', got '%s'", found.Name)
	}

	notFound := program.FindJob("NonExistent")
	if notFound != nil {
		t.Error("Expected not to find NonExistent job, got non-nil")
	}
}

func TestFindRecord(t *testing.T) {
	program := NewProgram("test")
	record1 := &Record{Name: "Record1"}
	record2 := &Record{Name: "Record2"}

	program.AddRecord(record1)
	program.AddRecord(record2)

	found := program.FindRecord("Record1")
	if found == nil {
		t.Error("Expected to find Record1, got nil")
	}
	if found.Name != "Record1" {
		t.Errorf("Expected record name 'Record1', got '%s'", found.Name)
	}

	notFound := program.FindRecord("NonExistent")
	if notFound != nil {
		t.Error("Expected not to find NonExistent record, got non-nil")
	}
}

func TestJobFindStep(t *testing.T) {
	job := &Job{Name: "TestJob"}
	step1 := &Step{Name: "Step1"}
	step2 := &Step{Name: "Step2"}

	job.Steps = append(job.Steps, step1, step2)

	found := job.FindStep("Step1")
	if found == nil {
		t.Error("Expected to find Step1, got nil")
	}
	if found.Name != "Step1" {
		t.Errorf("Expected step name 'Step1', got '%s'", found.Name)
	}

	notFound := job.FindStep("NonExistent")
	if notFound != nil {
		t.Error("Expected not to find NonExistent step, got non-nil")
	}
}

func TestJobFindVariable(t *testing.T) {
	job := &Job{Name: "TestJob"}
	var1 := &Variable{Name: "var1"}
	var2 := &Variable{Name: "var2"}

	job.Variables = append(job.Variables, var1, var2)

	found := job.FindVariable("var1")
	if found == nil {
		t.Error("Expected to find var1, got nil")
	}
	if found.Name != "var1" {
		t.Errorf("Expected variable name 'var1', got '%s'", found.Name)
	}

	notFound := job.FindVariable("NonExistent")
	if notFound != nil {
		t.Error("Expected not to find NonExistent variable, got non-nil")
	}
}

func TestRecordFindField(t *testing.T) {
	record := &Record{Name: "TestRecord"}
	field1 := &Field{Name: "field1"}
	field2 := &Field{Name: "field2"}

	record.Fields = append(record.Fields, field1, field2)

	found := record.FindField("field1")
	if found == nil {
		t.Error("Expected to find field1, got nil")
	}
	if found.Name != "field1" {
		t.Errorf("Expected field name 'field1', got '%s'", found.Name)
	}

	notFound := record.FindField("NonExistent")
	if notFound != nil {
		t.Error("Expected not to find NonExistent field, got non-nil")
	}
}

func TestToJSON(t *testing.T) {
	program := NewProgram("test")
	job := &Job{Name: "TestJob", Line: 1, Column: 1}
	program.AddJob(job)

	json, err := program.ToJSON()
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	if len(json) == 0 {
		t.Error("Expected non-empty JSON, got empty")
	}

	// Check that JSON contains expected fields
	jsonStr := string(json)
	if !contains(jsonStr, "test") {
		t.Error("Expected JSON to contain program name 'test'")
	}
	if !contains(jsonStr, "TestJob") {
		t.Error("Expected JSON to contain job name 'TestJob'")
	}
}

func TestString(t *testing.T) {
	program := NewProgram("test")
	job := &Job{Name: "TestJob", Line: 1, Column: 1}
	program.AddJob(job)

	str := program.String()
	if len(str) == 0 {
		t.Error("Expected non-empty string, got empty")
	}

	// String should be JSON formatted
	if !contains(str, "{") {
		t.Error("Expected string to contain JSON formatting")
	}
}

// Helper function to check if a string contains a substring
func contains(s, substr string) bool {
	return len(s) >= len(substr) && s[:len(substr)] == substr ||
		len(s) > len(substr) && contains(s[1:], substr)
}
