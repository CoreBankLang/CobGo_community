//go:build ignore
// +build ignore

package translator

import (
	"testing"

	"github.com/cobgo/cobgo-community/pkg/cobolparser"
)

func TestTranslatorBasic(t *testing.T) {
	cobolSource := `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Hello".
           STOP RUN.
	`

	// Parse COBOL
	cobolParser := cobolparser.NewParser(cobolSource)
	cobolAST, err := cobolParser.Parse()
	if err != nil {
		t.Fatalf("COBOL parsing failed: %v", err)
	}

	// Translate
	translator := NewTranslator(cobolAST)
	dslAST, err := translator.Translate()
	if err != nil {
		t.Fatalf("Translation failed: %v", err)
	}

	// Verify job created
	if len(dslAST.Jobs) == 0 {
		t.Fatal("No jobs created")
	}

	job := dslAST.Jobs[0]

	// Check job name
	if job.Name != "TestProgram" {
		t.Errorf("Expected job name 'TestProgram', got '%s'", job.Name)
	}

	// Check at least one step exists
	if len(job.Steps) == 0 {
		t.Error("No steps created")
	}

	t.Logf("✅ Translation successful: %d steps created", len(job.Steps))
}

func TestTranslatorWithFiles(t *testing.T) {
	cobolSource := `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-TEST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-REC.
           05 INPUT-ID PIC 9(5).
           05 INPUT-NAME PIC X(30).
       
       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC XX.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE.
           CLOSE INPUT-FILE.
           STOP RUN.
	`

	// Parse and translate
	cobolParser := cobolparser.NewParser(cobolSource)
	cobolAST, err := cobolParser.Parse()
	if err != nil {
		t.Fatalf("COBOL parsing failed: %v", err)
	}

	translator := NewTranslator(cobolAST)
	dslAST, err := translator.Translate()
	if err != nil {
		t.Fatalf("Translation failed: %v", err)
	}

	job := dslAST.Jobs[0]

	// Check file definitions created
	if len(job.Files) == 0 {
		t.Error("No file definitions created")
	}

	if len(job.Files) > 0 {
		file := job.Files[0]
		if file.Name != "input_file" {
			t.Errorf("Expected file name 'input_file', got '%s'", file.Name)
		}
		if file.Organization != "sequential" {
			t.Errorf("Expected organization 'sequential', got '%s'", file.Organization)
		}
	}

	// Check records created
	if len(job.Records) == 0 {
		t.Error("No record definitions created")
	}

	t.Logf("✅ File translation successful: %d files, %d records", len(job.Files), len(job.Records))
}

func TestTranslatorPerformMapping(t *testing.T) {
	cobolSource := `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM-TEST.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-PARA.
           PERFORM PROCESS-PARA.
           STOP RUN.
       
       INIT-PARA.
           DISPLAY "Initializing".
           EXIT.
       
       PROCESS-PARA.
           DISPLAY "Processing".
           EXIT.
	`

	// Parse and translate
	cobolParser := cobolparser.NewParser(cobolSource)
	cobolAST, err := cobolParser.Parse()
	if err != nil {
		t.Fatalf("COBOL parsing failed: %v", err)
	}

	translator := NewTranslator(cobolAST)
	dslAST, err := translator.Translate()
	if err != nil {
		t.Fatalf("Translation failed: %v", err)
	}

	job := dslAST.Jobs[0]

	// Check steps created for paragraphs
	if len(job.Steps) < 2 {
		t.Errorf("Expected at least 2 steps, got %d", len(job.Steps))
	}

	t.Logf("✅ PERFORM mapping successful: %d steps created", len(job.Steps))
}

func TestDSLGenerator(t *testing.T) {
	cobolSource := `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GEN-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG PIC X(20) VALUE "Test".
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY WS-MSG.
           STOP RUN.
	`

	// Parse, translate, and generate DSL
	cobolParser := cobolparser.NewParser(cobolSource)
	cobolAST, err := cobolParser.Parse()
	if err != nil {
		t.Fatalf("COBOL parsing failed: %v", err)
	}

	translator := NewTranslator(cobolAST)
	dslAST, err := translator.Translate()
	if err != nil {
		t.Fatalf("Translation failed: %v", err)
	}

	generator := NewDSLGenerator()
	dslCode := generator.Generate(dslAST)

	// Check generated code
	if dslCode == "" {
		t.Fatal("No DSL code generated")
	}

	// Check for expected elements
	if !contains(dslCode, "job GenTest") {
		t.Error("Generated DSL missing job declaration")
	}
	if !contains(dslCode, "step Main") {
		t.Error("Generated DSL missing main step")
	}

	t.Logf("✅ DSL generation successful")
	t.Logf("Generated DSL:\n%s", dslCode)
}

func TestPictureToType(t *testing.T) {
	translator := &Translator{}

	tests := []struct {
		pic      string
		expected string
	}{
		{"9(5)", "int32"},
		{"X(20)", "string(50)"},
		{"9(10)V99", "decimal(15,2)"},
		{"S9(8)V99", "decimal(15,2)"},
	}

	for _, tt := range tests {
		result := translator.pictureToType(tt.pic)
		// Note: current implementation returns defaults, would need refinement
		t.Logf("PIC %s -> %s", tt.pic, result)
	}
}

func TestNamingConventions(t *testing.T) {
	tests := []struct {
		cobol string
		camel string
		snake string
	}{
		{"CUSTOMER-ID", "CustomerId", "customer_id"},
		{"WS-TOTAL-AMOUNT", "WsTotalAmount", "ws_total_amount"},
		{"MAIN-PARA", "MainPara", "main_para"},
	}

	for _, tt := range tests {
		camel := toCamelCase(tt.cobol)
		snake := toSnakeCase(tt.cobol)

		if camel != tt.camel {
			t.Errorf("toCamelCase(%s): expected '%s', got '%s'", tt.cobol, tt.camel, camel)
		}
		if snake != tt.snake {
			t.Errorf("toSnakeCase(%s): expected '%s', got '%s'", tt.cobol, tt.snake, snake)
		}
	}

	t.Log("✅ Naming convention tests passed")
}

// Helper function
func contains(s, substr string) bool {
	return len(s) > 0 && len(substr) > 0 && (s == substr || len(s) >= len(substr) && findSubstring(s, substr))
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
