# CobGO Community Edition - Quick Start Guide

Get up and running with CobGO in 5 minutes!

## Prerequisites

- Go 1.21 or later installed
- Git installed
- Basic familiarity with command line

## Installation

### Step 1: Clone the Repository

```bash
git clone https://github.com/cobgo/cobgo-community.git
cd cobgo-community
```

### Step 2: Install Dependencies

```bash
go mod download
```

### Step 3: Build the Tools

**On Linux/Mac:**
```bash
make tools
```

**On Windows:**
```powershell
.\build.ps1
```

This will create the following tools in the `bin/` directory:
- `dslc` - Main compiler
- `dslfmt` - Code formatter
- `dsllint` - Code linter
- `copybook2dsl` - Copybook converter

## Your First Program

### Option 1: Compile a COBOL Program

Create a file `hello.cob`:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, CobGO Community!"
           STOP RUN.
```

Compile it:

```bash
./bin/dslc hello.cob -o hello.go
go run hello.go
```

### Option 2: Write in CobGO DSL

Create a file `hello.cobgo`:

```cobgo
job HelloWorld {
    step Greet {
        var message string = "Hello, CobGO Community!"
        display(message)
    }
}
```

Compile it:

```bash
./bin/dslc hello.cobgo -o hello.go
go run hello.go
```

## Next Steps

1. **Read the Tutorials**: Check out [tutorials/](tutorials/) for step-by-step guides
2. **Explore Examples**: See [examples/](examples/) for sample programs
3. **Read Documentation**: See [docs/](docs/) for detailed documentation
4. **Join the Community**: Contribute, ask questions, and share your projects!

## Common Commands

```bash
# Compile COBOL to Go
./bin/dslc program.cob -o program.go

# Format DSL code
./bin/dslfmt -i program.cobgo

# Lint DSL code
./bin/dsllint program.cobgo

# Convert copybook
./bin/copybook2dsl -i customer.cpy -o customer.cobgo
```

## Getting Help

- **Documentation**: [docs/](docs/)
- **Issues**: [GitHub Issues](https://github.com/cobgo/cobgo-community/issues)
- **Discussions**: [GitHub Discussions](https://github.com/cobgo/cobgo-community/discussions)

## Troubleshooting

### Build Fails
- Ensure Go 1.21+ is installed: `go version`
- Check Go is in your PATH: `which go` (Linux/Mac) or `where go` (Windows)

### Import Errors
- Run `go mod download` to fetch dependencies
- Ensure you're in the project root directory

### Tool Not Found
- Ensure you ran `make tools` or `.\build.ps1`
- Check that `bin/` directory exists and contains executables

---

**Happy Modernizing!** 🚀

