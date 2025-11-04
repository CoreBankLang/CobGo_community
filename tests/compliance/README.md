# CobGO Compliance Test Suite

## Overview

This directory contains comprehensive compliance tests that ensure CobGO behaves exactly as COBOL in critical domains:

- **Decimal Arithmetic**: Fixed-point decimal operations with COBOL-compatible precision
- **File I/O**: Sequential, indexed, and relative file operations
- **Batch Processing**: Job orchestration and transaction processing
- **Data Handling**: OCCURS, REDEFINES, and group items

## Directory Structure

```
tests/compliance/
├── README.md                      # This file
├── arithmetic/                    # Arithmetic operation tests
│   ├── addition_test.cobgo
│   ├── subtraction_test.cobgo
│   ├── multiplication_test.cobgo
│   ├── division_test.cobgo
│   ├── compute_test.cobgo
│   └── rounding_test.cobgo
├── file_io/                       # File I/O operation tests
│   ├── sequential_test.cobgo
│   ├── indexed_test.cobgo
│   ├── relative_test.cobgo
│   └── file_status_test.cobgo
├── batch/                         # Batch processing tests
│   ├── job_control_test.cobgo
│   ├── transaction_test.cobgo
│   └── error_handling_test.cobgo
├── data_handling/                 # Data structure tests
│   ├── occurs_test.cobgo
│   ├── redefines_test.cobgo
│   ├── group_items_test.cobgo
│   └── picture_clause_test.cobgo
├── golden_outputs/                # Expected outputs for golden file testing
│   ├── arithmetic/
│   ├── file_io/
│   ├── batch/
│   └── data_handling/
└── compliance_test.go             # Main compliance test runner

```

## Test Philosophy

These tests are designed to:

1. **Match COBOL Behavior**: Ensure CobGO produces identical results to COBOL compilers
2. **Cover Edge Cases**: Test boundary conditions, overflow, underflow, and error handling
3. **Validate Precision**: Ensure decimal arithmetic maintains COBOL-compatible precision
4. **Test Standards**: Follow COBOL-85, COBOL-2002, and COBOL-2014 standards
5. **Be Reproducible**: All tests must produce deterministic, repeatable results

## Test Sources

These tests are original implementations inspired by:

- **ANSI COBOL Standards**: Official COBOL language specifications
- **IBM Enterprise COBOL**: Industry-standard COBOL behavior
- **Common COBOL Patterns**: Real-world COBOL programming practices
- **Financial Computing Standards**: IEEE 754-2008 decimal arithmetic

**Note**: These tests are original works created for CobGO and are licensed under MIT. They do not contain code from GPL-licensed projects like GNUCobol.

## Running Compliance Tests

### Run All Compliance Tests
```bash
go test ./tests/compliance/... -v
```

### Run Specific Test Categories
```bash
# Arithmetic tests only
go test ./tests/compliance/... -v -run TestArithmetic

# File I/O tests only
go test ./tests/compliance/... -v -run TestFileIO

# Batch processing tests only
go test ./tests/compliance/... -v -run TestBatch

# Data handling tests only
go test ./tests/compliance/... -v -run TestDataHandling
```

### Run with Golden File Updates
```bash
# Update golden files if behavior is correct
go test ./tests/compliance/... -update-golden
```

### Run with Coverage
```bash
go test ./tests/compliance/... -cover -coverprofile=coverage.out
go tool cover -html=coverage.out
```

## Adding New Compliance Tests

### 1. Create Test File

Create a new `.cobgo` file in the appropriate category:

```cobgo
// arithmetic/new_operation_test.cobgo
job ArithmeticTest {
    var num1 decimal(15,2) = 123.45
    var num2 decimal(15,2) = 67.89
    var result decimal(15,2)
    
    step Main {
        result = num1 + num2
        display("Result: ", result)
    }
}
```

### 2. Create Golden Output

Create corresponding golden output file:

```
golden_outputs/arithmetic/new_operation_expected.txt
```

### 3. Add Test Case

Add test case to `compliance_test.go`:

```go
func TestArithmeticNewOperation(t *testing.T) {
    runComplianceTest(t, "arithmetic/new_operation_test.cobgo", 
                      "arithmetic/new_operation_expected.txt")
}
```

### 4. Validate

Run the test and ensure it passes:

```bash
go test ./tests/compliance/... -v -run TestArithmeticNewOperation
```

## Compliance Requirements

### Decimal Arithmetic

- **Precision**: Maintain exact decimal precision (no floating-point errors)
- **Rounding**: Follow COBOL rounding rules (ROUNDED clause)
- **Overflow**: Handle overflow/underflow according to COBOL standards
- **Division**: Handle division by zero with proper error codes

### File I/O

- **File Status**: Return standard COBOL file status codes
- **Record Locking**: Support proper record locking for indexed files
- **EOF Handling**: Handle end-of-file conditions correctly
- **Error Recovery**: Support proper error recovery mechanisms

### Batch Processing

- **Job Steps**: Execute job steps in correct order
- **Transaction Boundaries**: Maintain transaction integrity
- **Rollback**: Support transaction rollback on errors
- **Logging**: Provide comprehensive audit trails

### Data Handling

- **OCCURS**: Support fixed and variable-length arrays
- **REDEFINES**: Support memory aliasing correctly
- **Group Items**: Handle nested group items properly
- **PICTURE**: Support all COBOL PICTURE clause formats

## Performance Benchmarks

All compliance tests must meet performance requirements:

- **Decimal Operations**: < 1ms per 1000 operations
- **Record I/O**: < 10ms per 1000 records
- **Job Orchestration**: < 100ms per job step
- **End-to-End Tests**: < 5 seconds for complete test suite

## CI/CD Integration

Compliance tests are automatically run in CI/CD:

- **Pull Requests**: All compliance tests must pass
- **Main Branch**: Performance benchmarks must meet requirements
- **Releases**: 100% compliance test coverage required

## Troubleshooting

### Test Failures

1. **Check Golden Files**: Ensure golden output files are up-to-date
2. **Verify DSL Syntax**: Ensure test DSL is valid
3. **Check Compiler Output**: Review generated Go code for issues
4. **Enable Verbose Mode**: Run with `-v` flag for detailed output

### Performance Issues

1. **Profile Tests**: Use Go's profiling tools to identify bottlenecks
2. **Check Test Data**: Ensure test data sizes are appropriate
3. **Review Algorithms**: Check for inefficient algorithms in generated code

## Contributing

When contributing compliance tests:

1. **Original Work Only**: Do not copy from GPL-licensed sources
2. **Document Sources**: Cite COBOL standards and specifications
3. **Add Documentation**: Explain what the test validates
4. **Include Golden Files**: Provide expected output files
5. **Test Coverage**: Ensure comprehensive edge case coverage

## References

- [ANSI COBOL Standards](https://www.iso.org/standard/51416.html)
- [IBM Enterprise COBOL Documentation](https://www.ibm.com/products/cobol-compiler-zos)
- [IEEE 754-2008 Decimal Arithmetic](https://ieeexplore.ieee.org/document/4610935)
- [COBOL Programming Best Practices](https://www.microfocus.com/documentation/enterprise-developer/)

---

**Last Updated**: $(date)
**Compliance Level**: COBOL-85, COBOL-2002, COBOL-2014
**License**: MIT

