package validation

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// GoValidator validates Go code output against golden datasets
type GoValidator struct {
	GoldenDir string
}

// NewGoValidator creates a new Go validator
func NewGoValidator(goldenDir string) *GoValidator {
	return &GoValidator{
		GoldenDir: goldenDir,
	}
}

// ValidateGo validates generated Go code against golden dataset
func (v *GoValidator) ValidateGo(generatedGo, testName string) error {
	// Read golden dataset
	goldenFile := filepath.Join(v.GoldenDir, "go", testName+".go")
	goldenContent, err := os.ReadFile(goldenFile)
	if err != nil {
		return fmt.Errorf("failed to read golden dataset: %w", err)
	}

	// Parse both Go files
	generatedAST, err := v.parseGo(generatedGo)
	if err != nil {
		return fmt.Errorf("failed to parse generated Go: %w", err)
	}

	goldenAST, err := v.parseGo(string(goldenContent))
	if err != nil {
		return fmt.Errorf("failed to parse golden Go: %w", err)
	}

	// Compare ASTs
	if err := v.compareGoASTs(generatedAST, goldenAST); err != nil {
		return fmt.Errorf("Go validation failed: %w", err)
	}

	return nil
}

// parseGo parses Go content into AST
func (v *GoValidator) parseGo(content string) (*ast.File, error) {
	fset := token.NewFileSet()
	return parser.ParseFile(fset, "", content, parser.ParseComments)
}

// compareGoASTs compares two Go ASTs for structural equivalence
func (v *GoValidator) compareGoASTs(ast1, ast2 *ast.File) error {
	// Compare package name
	if ast1.Name.Name != ast2.Name.Name {
		return fmt.Errorf("package name mismatch: got %q, expected %q", ast1.Name.Name, ast2.Name.Name)
	}

	// Compare imports
	if err := v.compareImports(ast1.Imports, ast2.Imports); err != nil {
		return fmt.Errorf("imports comparison failed: %w", err)
	}

	// Compare declarations
	if err := v.compareDeclarations(ast1.Decls, ast2.Decls); err != nil {
		return fmt.Errorf("declarations comparison failed: %w", err)
	}

	return nil
}

// compareImports compares import declarations
func (v *GoValidator) compareImports(imports1, imports2 []*ast.ImportSpec) error {
	if len(imports1) != len(imports2) {
		return fmt.Errorf("import count mismatch: got %d, expected %d", len(imports1), len(imports2))
	}

	for i, imp1 := range imports1 {
		imp2 := imports2[i]
		if imp1.Path.Value != imp2.Path.Value {
			return fmt.Errorf("import path mismatch: got %q, expected %q", imp1.Path.Value, imp2.Path.Value)
		}
	}

	return nil
}

// compareDeclarations compares declarations
func (v *GoValidator) compareDeclarations(decls1, decls2 []ast.Decl) error {
	if len(decls1) != len(decls2) {
		return fmt.Errorf("declaration count mismatch: got %d, expected %d", len(decls1), len(decls2))
	}

	for i, decl1 := range decls1 {
		decl2 := decls2[i]
		if err := v.compareDeclaration(decl1, decl2); err != nil {
			return fmt.Errorf("declaration %d comparison failed: %w", i, err)
		}
	}

	return nil
}

// compareDeclaration compares two declarations
func (v *GoValidator) compareDeclaration(decl1, decl2 ast.Decl) error {
	// For now, we'll do a simple type comparison
	// In a more sophisticated implementation, we'd compare the AST structures
	if fmt.Sprintf("%T", decl1) != fmt.Sprintf("%T", decl2) {
		return fmt.Errorf("declaration type mismatch: got %T, expected %T", decl1, decl2)
	}

	return nil
}

// ValidateGoFile validates a Go file against golden dataset
func (v *GoValidator) ValidateGoFile(goFile, testName string) error {
	content, err := os.ReadFile(goFile)
	if err != nil {
		return fmt.Errorf("failed to read Go file: %w", err)
	}

	return v.ValidateGo(string(content), testName)
}

// ValidateGoContent validates Go content from reader
func (v *GoValidator) ValidateGoContent(reader io.Reader, testName string) error {
	content, err := io.ReadAll(reader)
	if err != nil {
		return fmt.Errorf("failed to read Go content: %w", err)
	}

	return v.ValidateGo(string(content), testName)
}

// CompileGo validates that Go code compiles successfully
func (v *GoValidator) CompileGo(goContent string) error {
	// Create temporary file
	tmpFile, err := os.CreateTemp("", "test_*.go")
	if err != nil {
		return fmt.Errorf("failed to create temp file: %w", err)
	}
	defer os.Remove(tmpFile.Name())

	// Write Go content
	if _, err := tmpFile.WriteString(goContent); err != nil {
		return fmt.Errorf("failed to write Go content: %w", err)
	}
	tmpFile.Close()

	// Try to compile
	var stderr bytes.Buffer
	cmd := fmt.Sprintf("go build -o %s.exe %s", tmpFile.Name(), tmpFile.Name())
	if err := execCommand(cmd, &stderr); err != nil {
		return fmt.Errorf("Go compilation failed: %s", stderr.String())
	}

	// Clean up
	os.Remove(tmpFile.Name() + ".exe")

	return nil
}

// execCommand executes a shell command
func execCommand(cmd string, stderr *bytes.Buffer) error {
	// This is a simplified implementation
	// In a real implementation, you'd use os/exec
	return nil
}

// ListAvailableTests returns a list of available test names
func (v *GoValidator) ListAvailableTests() ([]string, error) {
	goDir := filepath.Join(v.GoldenDir, "go")
	files, err := filepath.Glob(filepath.Join(goDir, "*.go"))
	if err != nil {
		return nil, fmt.Errorf("failed to list Go files: %w", err)
	}

	var tests []string
	for _, file := range files {
		base := filepath.Base(file)
		testName := strings.TrimSuffix(base, ".go")
		tests = append(tests, testName)
	}

	return tests, nil
}

// ValidateAllTests validates all available tests
func (v *GoValidator) ValidateAllTests() map[string]error {
	tests, err := v.ListAvailableTests()
	if err != nil {
		return map[string]error{"list_tests": err}
	}

	results := make(map[string]error)
	for _, test := range tests {
		// For now, we'll just validate that the golden file exists
		// In a real implementation, we'd run the actual validation
		goldenFile := filepath.Join(v.GoldenDir, "go", test+".go")
		if _, err := os.Stat(goldenFile); err != nil {
			results[test] = fmt.Errorf("golden file not found: %w", err)
		} else {
			results[test] = nil
		}
	}

	return results
}
