# CobGO Acceptance Test Suite

## Overview

This directory contains comprehensive acceptance tests for the CobGO COBOL modernization platform. The tests validate the complete end-to-end pipeline: **COBOL → DSL → Go → Binary**.

## Test Structure

```
tests/acceptance/
├── README.md                    # This file
├── cobol_samples/              # Curated COBOL sample programs
│   ├── basic/                  # Basic COBOL constructs
│   ├── business/               # Business logic examples
│   ├── data_processing/        # Data processing programs
│   └── complex/                # Complex enterprise examples
├── golden_outputs/             # Expected outputs for validation
│   ├── dsl/                    # Expected DSL output
│   ├── go/                     # Expected Go output
│   └── binary/                 # Expected binary behavior
├── test_runner.go              # Main test runner
├── pipeline_test.go            # End-to-end pipeline tests
└── validation/                 # Validation utilities
    ├── dsl_validator.go        # DSL output validation
    ├── go_validator.go         # Go output validation
    └── binary_validator.go     # Binary execution validation
```

## Test Categories

### 1. Basic COBOL Constructs
- Variable declarations and assignments
- Arithmetic operations
- String manipulation
- Control structures (IF-THEN-ELSE, PERFORM)
- File I/O operations

### 2. Business Logic Examples
- Customer management systems
- Order processing
- Payment calculations
- Report generation
- Data validation

### 3. Data Processing Programs
- Batch processing
- Record transformations
- Data aggregation
- Sorting and merging
- Error handling

### 4. Complex Enterprise Examples
- Multi-step job processing
- Transaction management
- Database operations
- Web service integration
- Performance-critical operations

## Running Tests

### Run All Acceptance Tests
```bash
go test ./tests/acceptance/... -v
```

### Run Specific Test Categories
```bash
# Basic constructs only
go test ./tests/acceptance/... -v -run TestBasic

# Business logic only
go test ./tests/acceptance/... -v -run TestBusiness

# Data processing only
go test ./tests/acceptance/... -v -run TestDataProcessing

# Complex examples only
go test ./tests/acceptance/... -v -run TestComplex
```

### Run with Performance Benchmarks
```bash
go test ./tests/acceptance/... -v -bench=.
```

## Test Validation

Each test validates:
1. **COBOL Parsing**: Input COBOL parses without errors
2. **DSL Generation**: Generated DSL matches expected format
3. **Go Code Generation**: Generated Go code compiles successfully
4. **Binary Execution**: Generated binary produces expected output
5. **Performance**: Execution meets performance requirements

## Golden Dataset Management

Golden datasets are stored in `golden_outputs/` and serve as the source of truth for expected outputs. When updating the platform:

1. **Update Golden Datasets**: Modify expected outputs in `golden_outputs/`
2. **Validate Changes**: Run tests to ensure changes are intentional
3. **Version Control**: Commit both code changes and golden dataset updates

## Performance Requirements

- **Decimal Operations**: < 1ms per 1000 operations
- **Record I/O**: < 10ms per 1000 records
- **Job Orchestration**: < 100ms per job step
- **End-to-End Pipeline**: < 5 seconds for typical business programs

## Continuous Integration

Acceptance tests are automatically run in CI/CD pipeline:
- **Pull Requests**: Full test suite validation
- **Main Branch**: Performance benchmark validation
- **Releases**: Complete acceptance test validation

## Adding New Tests

1. **Create COBOL Sample**: Add to appropriate category in `cobol_samples/`
2. **Generate Golden Outputs**: Run pipeline and capture expected outputs
3. **Add Test Case**: Create test in `pipeline_test.go`
4. **Validate**: Ensure test passes and performance requirements are met
