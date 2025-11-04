# CobGO Setup Guide

## Overview

CobGO is a production-grade COBOL modernization platform that translates legacy COBOL applications to modern Go code. This guide will help you set up the development environment and get started with CobGO.

## Prerequisites

### Required Software

1. **Go Programming Language (1.21 or later)**
   - Download from: https://golang.org/dl/
   - Install and add to PATH
   - Verify installation: `go version`

2. **Git**
   - Download from: https://git-scm.com/downloads
   - Verify installation: `git --version`

3. **Make (Optional but recommended)**
   - Windows: Install via Chocolatey or use `nmake` (Visual Studio)
   - Alternative: Use PowerShell scripts provided

### Platform-Specific Setup

#### Windows Setup

**Option 1: Using Chocolatey (Recommended)**

1. Install Chocolatey: https://chocolatey.org/install
2. Install required tools:
   ```powershell
   choco install golang git make
   ```

**Option 2: Manual Installation**

1. **Install Go**:
   - Download Go installer from https://golang.org/dl/
   - Run installer and follow prompts
   - Add `C:\Program Files\Go\bin` to PATH

2. **Install Git**:
   - Download from https://git-scm.com/downloads
   - Run installer with default settings

3. **Verify Installation**:
   ```powershell
   go version
   git --version
   ```

#### Linux/macOS Setup

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install golang-go git make

# macOS (with Homebrew)
brew install go git make

# Verify installation
go version
git --version
make --version
```

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/cobgo/cobgo.git
cd cobgo
```

### 2. Install Dependencies

```bash
# Using Make
make deps

# Or manually
go mod download
go mod tidy
```

### 3. Build CobGO

```bash
# Build all tools
make tools

# Or build individual components
make build
```

This will create the following executables in the `bin/` directory:
- `dslc` - Main CobGO compiler
- `dslfmt` - DSL code formatter
- `dsllint` - DSL linter
- `copybook2dsl` - COBOL copybook converter

### 4. Verify Installation

```bash
# Run tests
make test

# Check tool versions
./bin/dslc --version
./bin/dslfmt --help
./bin/dsllint --help
./bin/copybook2dsl --help
```

## Quick Start

### 1. Compile a COBOL Program

```bash
# Compile a sample COBOL program
./bin/dslc examples/hello.cob -o hello.go

# Run the generated Go program
go run hello.go
```

### 2. Format DSL Code

```bash
# Format a DSL file
./bin/dslfmt -i examples/hello.cobgo

# Check formatting without modifying
./bin/dslfmt -check examples/hello.cobgo
```

### 3. Lint DSL Code

```bash
# Lint a DSL file
./bin/dsllint examples/hello.cobgo

# Lint with JSON output
./bin/dsllint -format json examples/hello.cobgo
```

### 4. Convert COBOL Copybook

```bash
# Convert a copybook to DSL
./bin/copybook2dsl -i examples/customer.cpy -o customer.cobgo

# Convert with custom options
./bin/copybook2dsl -i examples/customer.cpy -o customer.cobgo -prefix "Tst" -suffix "Type"
```

## Project Structure

```
cobgo/
├── cmd/                    # Command-line tools
│   ├── dslc/              # Main compiler
│   ├── dslfmt/            # DSL formatter
│   ├── dsllint/           # DSL linter
│   └── copybook2dsl/      # Copybook converter
├── pkg/                   # Core packages
│   ├── parser/            # COBOL parsing
│   ├── ir/                # Intermediate representation
│   ├── codegen/           # Go code generation
│   ├── decimal/           # Decimal arithmetic
│   ├── formatter/         # Code formatting
│   ├── linter/            # Code analysis
│   ├── copybook/          # Copybook processing
│   └── runtime/           # Runtime libraries
├── runtime/               # Runtime implementation
├── tests/acceptance/      # Acceptance tests
├── docs/                  # Documentation
├── examples/              # Sample programs
├── scripts/               # Build scripts
├── Makefile               # Build automation
├── build.ps1              # Windows build script
└── go.mod                 # Go module configuration
```

## Development Workflow

### 1. Making Changes

```bash
# Create a feature branch
git checkout -b feature/your-feature-name

# Make your changes
# ... edit files ...

# Run tests
make test

# Format code
make format

# Lint code
make lint
```

### 2. Building and Testing

```bash
# Build all components
make build

# Run all tests
make test

# Run acceptance tests
make acceptance-tests

# Run performance benchmarks
make benchmarks
```

### 3. Code Quality

```bash
# Format all DSL files
make format

# Lint all DSL files
make lint-dsl

# Run linters
make lint
```

## Troubleshooting

### Common Issues

**Go Not Found Error**
```
go: The term 'go' is not recognized
```

**Solution:**
1. Install Go from https://golang.org/dl/
2. Add Go to PATH:
   - Windows: Add `C:\Program Files\Go\bin` to PATH
   - Linux/macOS: Add `export PATH=$PATH:/usr/local/go/bin` to your shell profile
3. Restart your terminal
4. Verify: `go version`

**Module Not Found Error**
```
go: cannot find module
```

**Solution:**
```bash
go mod init github.com/cobgo/cobgo
go mod tidy
```

**Build Errors**

**Solution:**
1. Check Go version: `go version` (should be 1.21+)
2. Clean and rebuild:
   ```bash
   go clean
   go mod download
   go build ./...
   ```

**Permission Errors (Windows)**

**Solution:**
- Run PowerShell as Administrator
- Or change to a directory with write permissions

### Getting Help

1. Check this setup guide
2. Review the [Architecture Documentation](ARCHITECTURE.md)
3. Check the [API Documentation](API.md)
4. Review the [Migration Guide](migration.md)
5. Check Go installation: https://golang.org/doc/install

## Docker Setup (Alternative)

If you prefer using Docker:

```dockerfile
# Dockerfile
FROM golang:1.21-alpine

WORKDIR /app
COPY . .
RUN go mod download
RUN make tools

CMD ["./bin/dslc", "--version"]
```

```bash
# Build and run
docker build -t cobgo .
docker run cobgo
```

## Next Steps

After successful setup:

1. **Explore Examples**: Check out the `examples/` directory
2. **Read Documentation**: Review the documentation in `docs/`
3. **Run Tests**: Ensure all tests pass with `make test`
4. **Try the Tools**: Experiment with the CLI tools
5. **Contribute**: See [CONTRIBUTING.md](../CONTRIBUTING.md) for contribution guidelines

## Support

For issues or questions:

1. Check this setup guide
2. Review the documentation in the `docs/` directory
3. Check the [GitHub Issues](https://github.com/cobgo/cobgo/issues)
4. Verify your Go installation: https://golang.org/doc/install
5. Ensure your project structure matches the expected layout

## License

This project is licensed under the MIT License - see the [LICENSE](../LICENSE) file for details.