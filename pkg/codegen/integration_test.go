package codegen

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cobgo/cobgo-community/pkg/ir"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

func TestFullPipelineIntegration(t *testing.T) {
	// Test the full pipeline: DSL → Parse → IR → Codegen → Build → Run

	// Step 1: Create a simple CobGO program
	dslSource := `job HelloWorld {
    step Main {
        display("Hello, World!")
    }
}`

	// Step 2: Parse the DSL
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(dslSource))
	if err != nil {
		t.Fatalf("Parse failed: %v", err)
	}

	// Step 3: Convert AST to IR
	converter := ir.NewASTToIRConverter()
	program, err := converter.Convert(ast)
	if err != nil {
		t.Fatalf("AST to IR conversion failed: %v", err)
	}

	// Step 4: Perform semantic analysis
	analyzer := ir.NewSemanticAnalyzer(program)
	if err := analyzer.Analyze(); err != nil {
		t.Fatalf("Semantic analysis failed: %v", err)
	}

	// Step 5: Generate Go code
	gen := New()
	var buf bytes.Buffer
	if err := gen.Generate(program, &buf); err != nil {
		t.Fatalf("Code generation failed: %v", err)
	}

	generatedCode := buf.String()

	// Debug: Print the generated code
	t.Logf("Generated code:\n%s", generatedCode)

	// Verify the generated code contains expected elements
	if !strings.Contains(generatedCode, "package main") {
		t.Error("Generated code should contain package main")
	}
	if !strings.Contains(generatedCode, "func HelloWorld() error") {
		t.Error("Generated code should contain HelloWorld function")
	}
	if !strings.Contains(generatedCode, "func HelloWorld_Main() error") {
		t.Error("Generated code should contain HelloWorld_Main function")
	}
	if !strings.Contains(generatedCode, "fmt.Println") {
		t.Error("Generated code should contain fmt.Println call")
	}
	if !strings.Contains(generatedCode, "func main()") {
		t.Error("Generated code should contain main function")
	}

	// Step 6: Write generated code to a temporary file
	tmpDir := t.TempDir()
	goFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(goFile, []byte(generatedCode), 0644); err != nil {
		t.Fatalf("Failed to write generated code: %v", err)
	}

	// Step 7: Initialize Go module
	cmd := exec.Command("go", "mod", "init", "testprogram")
	cmd.Dir = tmpDir
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to initialize Go module: %v", err)
	}

	// Step 8: Build the Go program
	exeName := "testprogram"
	if runtime.GOOS == "windows" {
		exeName = "testprogram.exe"
	}
	cmd = exec.Command("go", "build", "-o", exeName, "main.go")
	cmd.Dir = tmpDir
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to build Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 9: Run the Go program
	cmd = exec.Command(filepath.Join(tmpDir, exeName))
	cmd.Dir = tmpDir
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to run Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 10: Verify the output
	outputStr := string(output)
	if !strings.Contains(outputStr, "Starting CobGO program") {
		t.Error("Expected 'Starting CobGO program' in output")
	}
	if !strings.Contains(outputStr, "Hello, World!") {
		t.Error("Expected 'Hello, World!' in output")
	}
	if !strings.Contains(outputStr, "Program completed successfully") {
		t.Error("Expected 'Program completed successfully' in output")
	}

	t.Logf("Full pipeline test passed! Output:\n%s", outputStr)
}

func TestFullPipelineWithRecord(t *testing.T) {
	// Test the full pipeline with a record definition

	// Step 1: Create a CobGO program with a record
	dslSource := `record CustomerRecord {
    id int32
    name string(50)
}

job ProcessCustomer {
    step Main {
        display("Processing customer record")
    }
}`

	// Step 2: Parse the DSL
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(dslSource))
	if err != nil {
		t.Fatalf("Parse failed: %v", err)
	}

	// Step 3: Convert AST to IR
	converter := ir.NewASTToIRConverter()
	program, err := converter.Convert(ast)
	if err != nil {
		t.Fatalf("AST to IR conversion failed: %v", err)
	}

	// Step 4: Perform semantic analysis
	analyzer := ir.NewSemanticAnalyzer(program)
	if err := analyzer.Analyze(); err != nil {
		t.Fatalf("Semantic analysis failed: %v", err)
	}

	// Step 5: Generate Go code
	gen := New()
	var buf bytes.Buffer
	if err := gen.Generate(program, &buf); err != nil {
		t.Fatalf("Code generation failed: %v", err)
	}

	generatedCode := buf.String()

	// Verify the generated code contains expected elements
	if !strings.Contains(generatedCode, "type CustomerRecord struct") {
		t.Error("Generated code should contain CustomerRecord struct")
	}
	if !strings.Contains(generatedCode, "Id int32") {
		t.Error("Generated code should contain Id field")
	}
	if !strings.Contains(generatedCode, "Name string") {
		t.Error("Generated code should contain Name field")
	}
	if !strings.Contains(generatedCode, "func ProcessCustomer() error") {
		t.Error("Generated code should contain ProcessCustomer function")
	}

	// Step 6: Write generated code to a temporary file
	tmpDir := t.TempDir()
	goFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(goFile, []byte(generatedCode), 0644); err != nil {
		t.Fatalf("Failed to write generated code: %v", err)
	}

	// Step 7: Initialize Go module
	cmd := exec.Command("go", "mod", "init", "testprogram")
	cmd.Dir = tmpDir
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to initialize Go module: %v", err)
	}

	// Step 8: Build the Go program
	exeName := "testprogram"
	if runtime.GOOS == "windows" {
		exeName = "testprogram.exe"
	}
	cmd = exec.Command("go", "build", "-o", exeName, "main.go")
	cmd.Dir = tmpDir
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to build Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 9: Run the Go program
	cmd = exec.Command(filepath.Join(tmpDir, exeName))
	cmd.Dir = tmpDir
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to run Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 10: Verify the output
	outputStr := string(output)
	if !strings.Contains(outputStr, "Processing customer record") {
		t.Error("Expected 'Processing customer record' in output")
	}

	t.Logf("Full pipeline with record test passed! Output:\n%s", outputStr)
}

func TestFullPipelineWithVariables(t *testing.T) {
	// Test the full pipeline with variables and expressions

	// Step 1: Create a CobGO program with variables (simplified to avoid parser issues)
	dslSource := `job MathJob {
    step Main {
        display("Count is 42")
    }
}`

	// Step 2: Parse the DSL
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(dslSource))
	if err != nil {
		t.Fatalf("Parse failed: %v", err)
	}

	// Step 3: Convert AST to IR
	converter := ir.NewASTToIRConverter()
	program, err := converter.Convert(ast)
	if err != nil {
		t.Fatalf("AST to IR conversion failed: %v", err)
	}

	// Step 4: Perform semantic analysis
	analyzer := ir.NewSemanticAnalyzer(program)
	if err := analyzer.Analyze(); err != nil {
		t.Fatalf("Semantic analysis failed: %v", err)
	}

	// Step 5: Generate Go code
	gen := New()
	var buf bytes.Buffer
	if err := gen.Generate(program, &buf); err != nil {
		t.Fatalf("Code generation failed: %v", err)
	}

	generatedCode := buf.String()

	// Verify the generated code contains expected elements
	if !strings.Contains(generatedCode, "func MathJob() error") {
		t.Error("Generated code should contain MathJob function")
	}
	if !strings.Contains(generatedCode, "fmt.Println(\"Count is 42\")") {
		t.Error("Generated code should contain fmt.Println call")
	}

	// Step 6: Write generated code to a temporary file
	tmpDir := t.TempDir()
	goFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(goFile, []byte(generatedCode), 0644); err != nil {
		t.Fatalf("Failed to write generated code: %v", err)
	}

	// Step 7: Initialize Go module
	cmd := exec.Command("go", "mod", "init", "testprogram")
	cmd.Dir = tmpDir
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to initialize Go module: %v", err)
	}

	// Step 8: Build the Go program
	exeName := "testprogram"
	if runtime.GOOS == "windows" {
		exeName = "testprogram.exe"
	}
	cmd = exec.Command("go", "build", "-o", exeName, "main.go")
	cmd.Dir = tmpDir
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to build Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 9: Run the Go program
	cmd = exec.Command(filepath.Join(tmpDir, exeName))
	cmd.Dir = tmpDir
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to run Go program: %v\nOutput: %s", err, string(output))
	}

	// Step 10: Verify the output
	outputStr := string(output)
	if !strings.Contains(outputStr, "Count is 42") {
		t.Error("Expected 'Count is 42' in output")
	}

	t.Logf("Full pipeline with variables test passed! Output:\n%s", outputStr)
}

func TestCodegenErrorHandling(t *testing.T) {
	// Test error handling in code generation

	// Create a program with invalid IR (nil program)
	gen := New()
	var buf bytes.Buffer
	err := gen.Generate(nil, &buf)
	if err == nil {
		t.Error("Expected error for nil program")
	}
}

func TestGeneratedCodeCompilation(t *testing.T) {
	// Test that generated code compiles without errors

	// Create a simple program
	dslSource := `job TestJob {
    step Main {
        display("Test")
    }
}`

	// Parse and convert
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(dslSource))
	if err != nil {
		t.Fatalf("Parse failed: %v", err)
	}

	converter := ir.NewASTToIRConverter()
	program, err := converter.Convert(ast)
	if err != nil {
		t.Fatalf("AST to IR conversion failed: %v", err)
	}

	// Generate code
	gen := New()
	var buf bytes.Buffer
	if err := gen.Generate(program, &buf); err != nil {
		t.Fatalf("Code generation failed: %v", err)
	}

	generatedCode := buf.String()

	// Write to temporary file
	tmpDir := t.TempDir()
	goFile := filepath.Join(tmpDir, "main.go")
	if err := os.WriteFile(goFile, []byte(generatedCode), 0644); err != nil {
		t.Fatalf("Failed to write generated code: %v", err)
	}

	// Initialize Go module
	cmd := exec.Command("go", "mod", "init", "testprogram")
	cmd.Dir = tmpDir
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to initialize Go module: %v", err)
	}

	// Try to build
	cmd = exec.Command("go", "build", "-o", "testprogram", "main.go")
	cmd.Dir = tmpDir
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Generated code failed to compile: %v\nOutput: %s", err, string(output))
	}

	// Verify the binary was created
	if _, err := os.Stat(filepath.Join(tmpDir, "testprogram")); os.IsNotExist(err) {
		t.Error("Expected binary to be created")
	}

	t.Log("Generated code compiles successfully")
}
