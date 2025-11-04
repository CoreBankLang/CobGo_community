package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/cobolparser"
	"github.com/cobgo/cobgo-community/pkg/codegen"
	"github.com/cobgo/cobgo-community/pkg/ir"
	"github.com/cobgo/cobgo-community/pkg/parser"
)

var (
	version   = "dev"
	buildTime = "unknown"
	gitCommit = "unknown"
)

func main() {
	var (
		inputFile   = flag.String("i", "", "Input COBOL file")
		outputFile  = flag.String("o", "", "Output Go file (default: input.go)")
		showVersion = flag.Bool("version", false, "Show version information")
		verbose     = flag.Bool("v", false, "Verbose output")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("CobGO DSL Compiler v%s\n", version)
		fmt.Printf("Build time: %s\n", buildTime)
		fmt.Printf("Git commit: %s\n", gitCommit)
		return
	}

	if *inputFile == "" {
		fmt.Fprintf(os.Stderr, "Error: Input file required\n")
		fmt.Fprintf(os.Stderr, "Usage: %s -i <input.cob> [-o <output.go>]\n", os.Args[0])
		os.Exit(1)
	}

	// Generate output filename if not provided
	if *outputFile == "" {
		ext := filepath.Ext(*inputFile)
		*outputFile = strings.TrimSuffix(*inputFile, ext) + ".go"
	}

	if *verbose {
		fmt.Printf("Compiling %s -> %s\n", *inputFile, *outputFile)
	}

	// Implement actual compilation pipeline
	err := compileFile(*inputFile, *outputFile, *verbose)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Compilation successful: %s\n", *outputFile)
}

func compileFile(inputFile, outputFile string, verbose bool) error {
	// Read input file
	input, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("failed to read input file: %w", err)
	}

	// Detect file type
	ext := strings.ToLower(filepath.Ext(inputFile))
	isCOBOL := ext == ".cob" || ext == ".cbl" || ext == ".cobol"

	var dslSource string
	if isCOBOL {
		// Parse COBOL and convert to DSL
		if verbose {
			fmt.Printf("Detected COBOL file, converting to DSL...\n")
		}

		cobolParser := cobolparser.NewParser(string(input))
		cobolAST, err := cobolParser.Parse()
		if err != nil {
			return fmt.Errorf("COBOL parsing failed: %w", err)
		}

		if verbose {
			fmt.Printf("Parsed COBOL AST successfully\n")
		}

		// Convert COBOL AST to DSL
		dslSource, err = cobolparser.ConvertToDSL(cobolAST)
		if err != nil {
			return fmt.Errorf("COBOL to DSL conversion failed: %w", err)
		}

		if verbose {
			fmt.Printf("Converted COBOL to DSL successfully\n")
			fmt.Printf("Generated DSL:\n%s\n", dslSource)
		}
	} else {
		// Direct DSL source
		dslSource = string(input)
	}

	// Parse the DSL code
	p := parser.New()
	ast, err := p.Parse(strings.NewReader(dslSource))
	if err != nil {
		return fmt.Errorf("DSL parsing failed: %w", err)
	}

	if verbose {
		fmt.Printf("Parsed DSL AST successfully\n")
	}

	// Convert AST to IR
	converter := ir.NewASTToIRConverter()
	irProgram, err := converter.Convert(ast)
	if err != nil {
		return fmt.Errorf("IR conversion failed: %w", err)
	}

	if verbose {
		fmt.Printf("Converted to IR successfully\n")
	}

	// Generate Go code
	generator := codegen.New()
	output, err := os.Create(outputFile)
	if err != nil {
		return fmt.Errorf("failed to create output file: %w", err)
	}
	defer output.Close()

	err = generator.Generate(irProgram, output)
	if err != nil {
		return fmt.Errorf("code generation failed: %w", err)
	}

	if verbose {
		fmt.Printf("Generated Go code successfully\n")
	}

	return nil
}
