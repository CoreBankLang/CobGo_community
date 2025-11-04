# CobGO API Documentation

## Overview

This document describes the public APIs provided by the CobGO system for COBOL modernization.

## Compiler API

### Command Line Interface

```bash
dslc [options] <input-file>
```

#### Options

- `-i, -input`: Input COBOL file (required)
- `-o, -output`: Output Go file (optional, defaults to input.go)
- `-v, -verbose`: Verbose output
- `-version`: Show version information

#### Examples

```bash
# Basic compilation
dslc -i program.cob

# Specify output file
dslc -i program.cob -o generated_program.go

# Verbose output
dslc -i program.cob -v
```

## Parser API

### Package: `github.com/cobgo/cobgo/pkg/parser`

#### Parser

```go
type Parser struct {
    // Internal state
}

// New creates a new COBOL parser
func New() *Parser

// Parse parses COBOL source code and returns an AST
func (p *Parser) Parse(reader io.Reader) (*AST, error)
```

#### AST (Abstract Syntax Tree)

```go
type AST struct {
    // AST nodes
}
```

#### Usage Example

```go
package main

import (
    "os"
    "github.com/cobgo/cobgo/pkg/parser"
)

func main() {
    p := parser.New()
    
    file, err := os.Open("program.cob")
    if err != nil {
        panic(err)
    }
    defer file.Close()
    
    ast, err := p.Parse(file)
    if err != nil {
        panic(err)
    }
    
    // Process AST...
}
```

## Intermediate Representation API

### Package: `github.com/cobgo/cobgo/pkg/ir`

#### Program

```go
type Program struct {
    Name            string
    DataDivision    *DataDivision
    ProcedureDivision *ProcedureDivision
}

// NewProgram creates a new IR program
func NewProgram(name string) *Program
```

#### Data Division

```go
type DataDivision struct {
    WorkingStorage []*Variable
    FileSection   []*FileDescription
}

type Variable struct {
    Name    string
    Type    DataType
    Size    int
    Picture string
    Value   interface{}
}
```

#### Data Types

```go
type DataType int

const (
    TypeNumeric DataType = iota
    TypeAlphabetic
    TypeAlphanumeric
    TypeGroup
)

func (dt DataType) String() string
```

#### Usage Example

```go
package main

import (
    "github.com/cobgo/cobgo/pkg/ir"
)

func main() {
    program := ir.NewProgram("HELLO")
    
    // Add variables
    program.DataDivision.WorkingStorage = append(
        program.DataDivision.WorkingStorage,
        &ir.Variable{
            Name: "WS-NAME",
            Type: ir.TypeAlphanumeric,
            Size: 20,
        },
    )
    
    // Process program...
}
```

## Code Generator API

### Package: `github.com/cobgo/cobgo/pkg/codegen`

#### Generator

```go
type Generator struct {
    // Internal state
}

// New creates a new code generator
func New() *Generator

// Generate generates Go code from IR
func (g *Generator) Generate(ir interface{}, writer io.Writer) error

// GenerateFromTemplate generates code using Go templates
func (g *Generator) GenerateFromTemplate(data TemplateData, writer io.Writer) error
```

#### Template Data

```go
type TemplateData struct {
    PackageName string
    Imports     []string
    Functions   []Function
    Variables   []Variable
}

type Function struct {
    Name       string
    Parameters []Parameter
    ReturnType string
    Body       string
}

type Parameter struct {
    Name string
    Type string
}

type Variable struct {
    Name  string
    Type  string
    Value string
}
```

#### Usage Example

```go
package main

import (
    "os"
    "github.com/cobgo/cobgo/pkg/codegen"
)

func main() {
    generator := codegen.New()
    
    data := codegen.TemplateData{
        PackageName: "main",
        Imports: []string{"fmt"},
        Functions: []codegen.Function{
            {
                Name: "main",
                Body: "fmt.Println(\"Hello, World!\")",
            },
        },
    }
    
    file, err := os.Create("output.go")
    if err != nil {
        panic(err)
    }
    defer file.Close()
    
    err = generator.GenerateFromTemplate(data, file)
    if err != nil {
        panic(err)
    }
}
```

## Runtime API

### Package: `github.com/cobgo/cobgo/runtime`

#### CobolRuntime

```go
type CobolRuntime struct {
    // Internal state
}

// New creates a new COBOL runtime
func New() *CobolRuntime
```

#### I/O Operations

```go
// Display prints output (COBOL DISPLAY statement)
func (r *CobolRuntime) Display(args ...interface{})

// DisplayLine prints output with newline
func (r *CobolRuntime) DisplayLine(args ...interface{})

// Accept reads input (COBOL ACCEPT statement)
func (r *CobolRuntime) Accept(prompt string) string
```

#### Data Operations

```go
// Move copies data (COBOL MOVE statement)
func (r *CobolRuntime) Move(src, dest interface{})

// Add performs addition (COBOL ADD statement)
func (r *CobolRuntime) Add(a, b interface{}) interface{}

// Subtract performs subtraction (COBOL SUBTRACT statement)
func (r *CobolRuntime) Subtract(a, b interface{}) interface{}

// Multiply performs multiplication (COBOL MULTIPLY statement)
func (r *CobolRuntime) Multiply(a, b interface{}) interface{}

// Divide performs division (COBOL DIVIDE statement)
func (r *CobolRuntime) Divide(a, b interface{}) interface{}
```

#### String Operations

```go
// StringLength returns the length of a string
func (r *CobolRuntime) StringLength(s string) int

// StringTrim trims spaces from a string
func (r *CobolRuntime) StringTrim(s string) string

// StringUpper converts string to uppercase
func (r *CobolRuntime) StringUpper(s string) string

// StringLower converts string to lowercase
func (r *CobolRuntime) StringLower(s string) string
```

#### Control Flow

```go
// IfThenElse implements COBOL IF-THEN-ELSE logic
func (r *CobolRuntime) IfThenElse(condition bool, thenFunc, elseFunc func())

// PerformTimes implements COBOL PERFORM TIMES
func (r *CobolRuntime) PerformTimes(count int, fn func())

// PerformUntil implements COBOL PERFORM UNTIL
func (r *CobolRuntime) PerformUntil(condition func() bool, fn func())

// PerformWhile implements COBOL PERFORM WHILE
func (r *CobolRuntime) PerformWhile(condition func() bool, fn func())
```

#### Usage Example

```go
package main

import (
    "github.com/cobgo/cobgo/runtime"
)

func main() {
    rt := runtime.New()
    
    // Display output
    rt.DisplayLine("Hello, World!")
    
    // Read input
    name := rt.Accept("Enter your name: ")
    rt.DisplayLine("Hello, ", name)
    
    // Arithmetic operations
    result := rt.Add(10, 20)
    rt.DisplayLine("10 + 20 = ", result)
    
    // String operations
    upper := rt.StringUpper("hello")
    rt.DisplayLine("Uppercase: ", upper)
}
```

## Error Handling

All APIs return errors that implement the standard Go `error` interface. Common error types include:

- `ParseError`: Parser-related errors
- `TypeError`: Type checking errors
- `GenerationError`: Code generation errors
- `RuntimeError`: Runtime execution errors

## Version Information

The compiler provides version information through the command line:

```bash
dslc -version
```

This displays:
- Version number
- Build timestamp
- Git commit hash