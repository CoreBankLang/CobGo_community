# Contributing to CobGO

Thank you for your interest in contributing to CobGO! This document provides guidelines and information for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Contributing Process](#contributing-process)
- [Code Standards](#code-standards)
- [Testing Guidelines](#testing-guidelines)
- [Documentation Guidelines](#documentation-guidelines)
- [Release Process](#release-process)
- [Community Guidelines](#community-guidelines)

## Code of Conduct

This project adheres to a code of conduct that ensures a welcoming environment for all contributors. By participating, you agree to uphold this code.

### Our Standards

- **Be Respectful**: Treat everyone with respect and kindness
- **Be Inclusive**: Welcome contributors from all backgrounds
- **Be Constructive**: Provide helpful feedback and suggestions
- **Be Professional**: Maintain a professional tone in all interactions

### Unacceptable Behavior

- Harassment, discrimination, or offensive language
- Personal attacks or trolling
- Spam or off-topic discussions
- Publishing private information without permission

## Getting Started

### Prerequisites

- Go 1.21 or later
- Git
- Make (optional, PowerShell scripts provided for Windows)
- Basic understanding of COBOL and Go

### Development Setup

1. **Fork the Repository**
   ```bash
   # Fork on GitHub, then clone your fork
   git clone https://github.com/your-username/cobgo.git
   cd cobgo
   ```

2. **Set Up Development Environment**
   ```bash
   # Install dependencies
   make deps
   
   # Build the project
   make build
   
   # Run tests
   make test
   ```

3. **Create a Feature Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

## Contributing Process

### 1. Choose an Issue

- Browse [GitHub Issues](https://github.com/cobgo/cobgo/issues)
- Look for issues labeled `good first issue` for beginners
- Comment on the issue to indicate you're working on it

### 2. Make Your Changes

- Write clean, well-documented code
- Follow the coding standards outlined below
- Add tests for new functionality
- Update documentation as needed

### 3. Test Your Changes

```bash
# Run all tests
make test

# Run specific test packages
go test ./pkg/parser/...

# Run with coverage
go test -cover ./...

# Run acceptance tests
make acceptance-tests
```

### 4. Submit a Pull Request

- Push your changes to your fork
- Create a pull request with a clear description
- Reference the issue number in your PR description
- Ensure all CI checks pass

### 5. Code Review

- Respond to feedback promptly
- Make requested changes
- Keep the PR focused and atomic
- Update documentation if needed

## Code Standards

### Go Code Style

- Follow [Effective Go](https://golang.org/doc/effective_go.html) guidelines
- Use `gofmt` for formatting
- Use `golint` for style checking
- Write clear, self-documenting code

### Naming Conventions

- **Packages**: lowercase, single word (e.g., `parser`, `codegen`)
- **Types**: PascalCase (e.g., `Parser`, `CodeGenerator`)
- **Functions**: PascalCase for public, camelCase for private
- **Variables**: camelCase (e.g., `inputFile`, `outputPath`)

### Code Organization

```go
// Package comment
package parser

// Imports (standard, third-party, local)
import (
    "fmt"
    "io"
    
    "github.com/some/package"
    
    "github.com/cobgo/cobgo/pkg/ir"
)

// Type definitions
type Parser struct {
    // fields
}

// Constructor
func New() *Parser {
    // implementation
}

// Public methods
func (p *Parser) Parse(reader io.Reader) (*ir.AST, error) {
    // implementation
}

// Private methods
func (p *Parser) parseStatement() (*Statement, error) {
    // implementation
}
```

### Error Handling

- Use Go's standard error handling patterns
- Return meaningful error messages
- Wrap errors with context when appropriate
- Use `errors.New()` for simple errors, `fmt.Errorf()` for formatted errors

```go
// Good
func (p *Parser) ParseFile(filename string) (*AST, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, fmt.Errorf("failed to open file %s: %w", filename, err)
    }
    defer file.Close()
    
    return p.Parse(file)
}

// Bad
func (p *Parser) ParseFile(filename string) (*AST, error) {
    file, _ := os.Open(filename) // Ignoring error
    return p.Parse(file)
}
```

## Testing Guidelines

### Unit Tests

- Write tests for all public functions
- Aim for >90% test coverage
- Use table-driven tests for multiple scenarios
- Test both success and error cases

```go
func TestParser_Parse(t *testing.T) {
    tests := []struct {
        name     string
        input    string
        expected *AST
        wantErr  bool
    }{
        {
            name:  "valid program",
            input: "PROGRAM-ID. HELLO.",
            expected: &AST{
                ProgramID: "HELLO",
            },
            wantErr: false,
        },
        {
            name:    "invalid syntax",
            input:   "INVALID SYNTAX",
            wantErr: true,
        },
    }
    
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            p := New()
            got, err := p.Parse(strings.NewReader(tt.input))
            
            if tt.wantErr {
                assert.Error(t, err)
                return
            }
            
            assert.NoError(t, err)
            assert.Equal(t, tt.expected, got)
        })
    }
}
```

### Integration Tests

- Test complete workflows
- Use real COBOL examples
- Verify generated Go code compiles and runs
- Test performance requirements

### Benchmark Tests

- Add benchmarks for performance-critical code
- Ensure performance requirements are met
- Use `go test -bench=.` to run benchmarks

```go
func BenchmarkParser_Parse(b *testing.B) {
    input := generateLargeCOBOLProgram()
    p := New()
    
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _, err := p.Parse(strings.NewReader(input))
        if err != nil {
            b.Fatal(err)
        }
    }
}
```

## Documentation Guidelines

### Code Documentation

- Document all public functions and types
- Use Go doc conventions
- Provide examples for complex functions
- Keep documentation up-to-date

```go
// Parser parses COBOL source code and generates an Abstract Syntax Tree.
// It supports various COBOL dialects and provides comprehensive error reporting.
type Parser struct {
    // lexer handles tokenization of COBOL source
    lexer *Lexer
    // options contains parser configuration
    options *Options
}

// Parse parses COBOL source code from the provided reader and returns an AST.
// It returns an error if the source code contains syntax errors.
//
// Example:
//   p := New()
//   ast, err := p.Parse(strings.NewReader("PROGRAM-ID. HELLO."))
//   if err != nil {
//       log.Fatal(err)
//   }
func (p *Parser) Parse(reader io.Reader) (*AST, error) {
    // implementation
}
```

### User Documentation

- Update README.md for user-facing changes
- Add examples for new features
- Update API documentation
- Keep setup guides current

### Commit Messages

Use clear, descriptive commit messages:

```
feat: add support for COBOL REDEFINES clause

- Implement REDEFINES parsing in parser package
- Add IR representation for redefined fields
- Update code generator to handle redefines
- Add comprehensive tests for redefines functionality

Fixes #123
```

## Release Process

### Version Numbering

We follow [Semantic Versioning](https://semver.org/):
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Checklist

- [ ] All tests pass
- [ ] Documentation is updated
- [ ] CHANGELOG.md is updated
- [ ] Version numbers are updated
- [ ] Release notes are written
- [ ] GitHub release is created

## Community Guidelines

### Getting Help

- Check existing documentation first
- Search GitHub issues for similar problems
- Ask questions in GitHub Discussions
- Join our community chat (if available)

### Providing Feedback

- Be specific about issues or suggestions
- Provide reproduction steps for bugs
- Include relevant system information
- Be patient with responses

### Recognition

- Contributors are recognized in release notes
- Significant contributors may be invited to join the core team
- Community members are acknowledged in project documentation

## License

By contributing to CobGO, you agree that your contributions will be licensed under the same license as the project (MIT License).

## Questions?

If you have questions about contributing, please:

1. Check this document first
2. Search existing GitHub issues
3. Create a new issue with the `question` label
4. Join our community discussions

Thank you for contributing to CobGO! 🚀