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
)

func TestSimpleCodegen(t *testing.T) {
	// Test basic code generation without parsing

	// Create a simple IR program directly
	program := &ir.Program{
		Name: "TestProgram",
		Jobs: []*ir.Job{
			{
				Name: "HelloJob",
				Steps: []*ir.Step{
					{
						Name: "Main",
						Body: &ir.Block{
							Statements: []ir.Statement{
								&ir.DisplayStatement{
									Args: []ir.Expression{
										&ir.LiteralExpression{
											Value:  "Hello, World!",
											Type:   "string",
											Line:   1,
											Column: 1,
										},
									},
									Line:   1,
									Column: 1,
								},
							},
						},
						Line:   1,
						Column: 1,
					},
				},
				Line:   1,
				Column: 1,
			},
		},
	}

	// Generate Go code
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
	if !strings.Contains(generatedCode, "func HelloJob() error") {
		t.Error("Generated code should contain HelloJob function")
	}
	if !strings.Contains(generatedCode, "fmt.Println(\"Hello, World!\")") {
		t.Error("Generated code should contain fmt.Println call with Hello, World!")
	}
	if !strings.Contains(generatedCode, "func main()") {
		t.Error("Generated code should contain main function")
	}

	// Write generated code to a temporary file
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

	// Build the Go program
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

	// Check if the executable was created
	exePath := filepath.Join(tmpDir, exeName)
	if _, err := os.Stat(exePath); os.IsNotExist(err) {
		t.Fatalf("Executable was not created at %s", exePath)
	}

	// List files in tmpDir for debugging
	files, _ := os.ReadDir(tmpDir)
	t.Logf("Files in tmpDir: %v", files)

	// Run the Go program
	cmd = exec.Command(exePath)
	output, err = cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("Failed to run Go program: %v\nOutput: %s", err, string(output))
	}

	// Verify the output
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

	t.Logf("Simple codegen test passed! Output:\n%s", outputStr)
}
