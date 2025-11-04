# Changelog

All notable changes to CobGO Community Edition will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-11-04

### Added - Initial Community Release

#### Core Compiler
- COBOL parser supporting COBOL-74 standard
- DSL (Domain Specific Language) for modern COBOL syntax
- Intermediate Representation (IR) for language-agnostic processing
- Go code generation with proper error handling
- Comprehensive runtime library for COBOL semantics

#### Developer Tools
- `dslc` - Main compiler (COBOL/CobGO → Go)
- `dslfmt` - Code formatter for consistent style
- `dsllint` - Static analyzer and linter
- `copybook2dsl` - COBOL copybook converter

#### COBOL Language Support
- All data types (COMP, DISPLAY, PACKED-DECIMAL)
- All PIC clauses (9, X, A, S, V, decimal positions)
- OCCURS (fixed tables)
- REDEFINES (data overlays)
- Group items and elementary items
- Level numbers (01-49, 77, 88)
- VALUE clauses
- USAGE clauses

#### Control Structures
- IF-THEN-ELSE statements
- EVALUATE (CASE) statements
- PERFORM loops (UNTIL, VARYING, TIMES)
- GO TO (with warnings)

#### Arithmetic Operations
- ADD, SUBTRACT, MULTIPLY, DIVIDE
- COMPUTE (expressions)
- SIZE ERROR handling
- Decimal precision support

#### File I/O
- Sequential file I/O (OPEN, READ, WRITE, CLOSE)
- File status checking
- SELECT/ASSIGN statements
- FD (File Description) support

#### Program Structure
- IDENTIFICATION DIVISION
- ENVIRONMENT DIVISION
- DATA DIVISION (WORKING-STORAGE, FILE SECTION)
- PROCEDURE DIVISION

#### Documentation
- Complete setup and installation guide
- Architecture documentation
- API reference
- Language specification
- Migration guide
- Tutorials and examples

---

## [Unreleased]

### Planned Features
- Enhanced COBOL-85 support
- Performance optimizations
- Additional examples and tutorials
- Community contributions

---

## Version History

- **1.0.0** (2025-11-04) - Initial community release

