package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/cobgo/cobgo-community/pkg/linter"
)

var (
	version   = "dev"
	buildTime = "unknown"
	gitCommit = "unknown"
)

func main() {
	var (
		inputFile   = flag.String("i", "", "Input DSL file")
		showVersion = flag.Bool("version", false, "Show version information")
		verbose     = flag.Bool("v", false, "Verbose output")
		format      = flag.String("format", "text", "Output format (text, json)")
		enableAll   = flag.Bool("enable-all", false, "Enable all linter rules")
		disable     = flag.String("disable", "", "Comma-separated list of rules to disable")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("CobGO DSL Linter v%s\n", version)
		fmt.Printf("Build time: %s\n", buildTime)
		fmt.Printf("Git commit: %s\n", gitCommit)
		return
	}

	if *inputFile == "" {
		fmt.Fprintf(os.Stderr, "Error: Input file required\n")
		fmt.Fprintf(os.Stderr, "Usage: %s -i <input.cobgo> [options]\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	if *verbose {
		fmt.Printf("Linting %s\n", *inputFile)
	}

	// Create linter with options
	linterOptions := &linter.Options{
		EnableAll:     *enableAll,
		DisabledRules: strings.Split(*disable, ","),
		Format:        *format,
	}

	l := linter.New(linterOptions)

	// Run linter
	results, err := l.LintFile(*inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error linting file: %v\n", err)
		os.Exit(1)
	}

	// Output results
	if err := linter.OutputResults(results, *format, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "Error outputting results: %v\n", err)
		os.Exit(1)
	}

	// Exit with error code if there are issues
	if len(results.Issues) > 0 {
		os.Exit(1)
	}
}
