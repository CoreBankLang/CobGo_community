package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/copybook"
)

var (
	version   = "dev"
	buildTime = "unknown"
	gitCommit = "unknown"
)

func main() {
	var (
		inputFile   = flag.String("i", "", "Input copybook file")
		outputFile  = flag.String("o", "", "Output DSL file (default: input.cobgo)")
		showVersion = flag.Bool("version", false, "Show version information")
		verbose     = flag.Bool("v", false, "Verbose output")
		format      = flag.String("format", "dsl", "Output format (dsl, json)")
		prefix      = flag.String("prefix", "", "Prefix for generated record names")
		suffix      = flag.String("suffix", "", "Suffix for generated record names")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("CobGO Copybook to DSL Converter v%s\n", version)
		fmt.Printf("Build time: %s\n", buildTime)
		fmt.Printf("Git commit: %s\n", gitCommit)
		return
	}

	if *inputFile == "" {
		fmt.Fprintf(os.Stderr, "Error: Input file required\n")
		fmt.Fprintf(os.Stderr, "Usage: %s -i <input.cpy> [-o <output.cobgo>] [options]\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	// Generate output filename if not provided
	if *outputFile == "" {
		ext := filepath.Ext(*inputFile)
		*outputFile = strings.TrimSuffix(*inputFile, ext) + ".cobgo"
	}

	if *verbose {
		fmt.Printf("Converting %s -> %s\n", *inputFile, *outputFile)
	}

	// Create converter with options
	converterOptions := &copybook.Options{
		Prefix: *prefix,
		Suffix: *suffix,
		Format: *format,
	}

	converter := copybook.NewConverter(converterOptions)

	// Convert copybook to DSL
	result, err := converter.ConvertFile(*inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error converting copybook: %v\n", err)
		os.Exit(1)
	}

	// Write output
	err = os.WriteFile(*outputFile, []byte(result), 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing output file: %v\n", err)
		os.Exit(1)
	}

	if *verbose {
		fmt.Printf("Conversion successful: %s\n", *outputFile)
	}
}
