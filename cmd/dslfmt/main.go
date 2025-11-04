package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/cobgo/cobgo-community/pkg/formatter"
)

var (
	version   = "dev"
	buildTime = "unknown"
	gitCommit = "unknown"
)

func main() {
	var (
		inputFile   = flag.String("i", "", "Input DSL file")
		outputFile  = flag.String("o", "", "Output file (default: overwrite input)")
		showVersion = flag.Bool("version", false, "Show version information")
		verbose     = flag.Bool("v", false, "Verbose output")
		checkOnly   = flag.Bool("check", false, "Check formatting without modifying files")
		tabWidth    = flag.Int("tabwidth", 4, "Tab width for indentation")
		useTabs     = flag.Bool("tabs", false, "Use tabs instead of spaces")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("CobGO DSL Formatter v%s\n", version)
		fmt.Printf("Build time: %s\n", buildTime)
		fmt.Printf("Git commit: %s\n", gitCommit)
		return
	}

	if *inputFile == "" {
		fmt.Fprintf(os.Stderr, "Error: Input file required\n")
		fmt.Fprintf(os.Stderr, "Usage: %s -i <input.cobgo> [-o <output.cobgo>] [options]\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	// Generate output filename if not provided
	if *outputFile == "" {
		*outputFile = *inputFile
	}

	if *verbose {
		fmt.Printf("Formatting %s", *inputFile)
		if *outputFile != *inputFile {
			fmt.Printf(" -> %s", *outputFile)
		}
		fmt.Println()
	}

	// Read input file
	input, err := os.ReadFile(*inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	// Create formatter with options
	formatter := formatter.New(&formatter.Options{
		TabWidth: *tabWidth,
		UseTabs:  *useTabs,
	})

	// Format the code
	formatted, err := formatter.Format(string(input))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error formatting code: %v\n", err)
		os.Exit(1)
	}

	// Check if formatting is needed
	if *checkOnly {
		if string(input) != formatted {
			fmt.Printf("File %s is not properly formatted\n", *inputFile)
			os.Exit(1)
		}
		fmt.Printf("File %s is properly formatted\n", *inputFile)
		return
	}

	// Write formatted output
	err = os.WriteFile(*outputFile, []byte(formatted), 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing output file: %v\n", err)
		os.Exit(1)
	}

	if *verbose {
		fmt.Printf("Formatting complete: %s\n", *outputFile)
	}
}
