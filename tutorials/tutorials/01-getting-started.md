# Getting Started with CobGO DSL

## Overview

Welcome to CobGO! This tutorial will guide you through setting up CobGO, writing your first DSL program, and compiling it to Go.

**What You'll Learn:**
- Installing CobGO
- Understanding the DSL syntax
- Writing your first program
- Compiling and running DSL code
- Debugging common issues

**Prerequisites:**
- Go 1.21 or higher installed
- Basic understanding of programming concepts
- Text editor (VS Code, Vim, etc.)

---

## Installation

### Step 1: Clone the Repository

```bash
git clone https://github.com/cobgo/cobgo.git
cd cobgo
```

### Step 2: Build the Tools

**On Linux/Mac:**
```bash
make build-all
```

**On Windows (PowerShell):**
```powershell
.\build.ps1
```

This creates four executable tools:
- `dslc` - The DSL compiler
- `dslfmt` - Code formatter
- `dsllint` - Code linter
- `copybook2dsl` - COBOL copybook converter

### Step 3: Verify Installation

```bash
# Check compiler version
./bin/dslc --version

# Run tests to ensure everything works
go test ./...
```

---

## Your First CobGO Program

### Hello World

Create a file called `hello.cobgo`:

```go
// My first CobGO program
job HelloWorld {
    step Greet {
        var message string = "Hello, Banking World!"
        display(message)
    }
}
```

**Key Concepts:**
- **job**: Top-level container (like a COBOL program)
- **step**: A procedure or function (like a COBOL paragraph)
- **var**: Variable declaration with type
- **display**: Output to console (like COBOL DISPLAY)

### Compile and Run

```bash
# Compile to Go
./bin/dslc hello.cobgo -o hello_generated.go

# Run the generated code
go run hello_generated.go
```

**Expected Output:**
```
Hello, Banking World!
```

---

## Understanding DSL Syntax

### 1. Data Types

CobGO supports banking-appropriate data types:

```go
job DataTypesDemo {
    step ShowTypes {
        // Strings
        var customerName string = "John Doe"
        
        // Integers
        var accountNumber int32 = 123456
        var transactionID int64 = 9876543210
        
        // Decimals (for money)
        var balance decimal = 1000.50
        var interestRate decimal = 0.0425
        
        // Booleans
        var isActive bool = true
        
        // Dates
        var openDate date = "2025-01-15"
        
        display("Customer: " + customerName)
        display("Account: " + accountNumber)
        display("Balance: $" + balance)
    }
}
```

### 2. Arithmetic Operations

```go
job CalculatorDemo {
    step Calculate {
        var principal decimal = 1000.00
        var rate decimal = 0.05
        var years int32 = 3
        
        // Basic arithmetic
        var interest decimal = principal * rate * years
        var total decimal = principal + interest
        
        display("Principal: $" + principal)
        display("Interest: $" + interest)
        display("Total: $" + total)
    }
}
```

### 3. Control Flow

**If-Else Statements:**

```go
job ConditionalDemo {
    step CheckBalance {
        var balance decimal = 150.00
        var minimumBalance decimal = 100.00
        
        if balance >= minimumBalance {
            display("Account in good standing")
        } else {
            display("Warning: Below minimum balance")
        }
    }
}
```

**For Loops:**

```go
job LoopDemo {
    step CountTransactions {
        var count int32
        
        for count = 1; count <= 5; count = count + 1 {
            display("Processing transaction #" + count)
        }
    }
}
```

**While Loops:**

```go
job WhileDemo {
    step ProcessUntilDone {
        var remaining int32 = 100
        
        while remaining > 0 {
            display("Remaining: " + remaining)
            remaining = remaining - 10
        }
    }
}
```

### 4. Working with Records

```go
// Define a record structure
record Customer {
    id int32
    name string
    balance decimal
    status string
}

job RecordDemo {
    step CreateCustomer {
        var cust Customer
        cust.id = 12345
        cust.name = "Jane Smith"
        cust.balance = 5000.00
        cust.status = "ACTIVE"
        
        display("Customer ID: " + cust.id)
        display("Name: " + cust.name)
        display("Balance: $" + cust.balance)
    }
}
```

### 5. Multiple Steps

```go
job MultiStepDemo {
    step Initialize {
        var greeting string = "Welcome"
        display(greeting)
    }
    
    step Process {
        var count int32 = 0
        for count = 1; count <= 3; count = count + 1 {
            display("Processing step " + count)
        }
    }
    
    step Finalize {
        display("Processing complete!")
    }
}
```

---

## COBOL-85 Advanced Features

### INSPECT Statement

```go
job InspectDemo {
    step CountSpaces {
        var input string = "Hello World Banking"
        var spaceCount int32
        
        // Count all spaces
        inspect input tallying spaceCount for all " "
        
        display("Spaces found: " + spaceCount)
    }
    
    step MaskData {
        var ssn string = "123-45-6789"
        
        // Mask first 5 digits
        inspect ssn replacing first "123-45" by "XXX-XX"
        
        display("Masked SSN: " + ssn)
    }
}
```

### STRING Statement

```go
job StringDemo {
    step ConcatenateNames {
        var firstName string = "John"
        var lastName string = "Doe"
        var fullName string
        
        string firstName delimited by size
               " " delimited by size
               lastName delimited by size
               into fullName
        
        display("Full Name: " + fullName)
    }
}
```

### UNSTRING Statement

```go
job UnstringDemo {
    step ParseCSV {
        var csvRecord string = "Smith,John,555-1234,CA"
        var lastName string
        var firstName string
        var phone string
        var state string
        
        unstring csvRecord
            delimited by ","
            into lastName, firstName, phone, state
        
        display("Parsed Data:")
        display("  Last: " + lastName)
        display("  First: " + firstName)
        display("  Phone: " + phone)
        display("  State: " + state)
    }
}
```

### SEARCH Statement

```go
job SearchDemo {
    step FindAccount {
        var accounts []string = ["A100", "A200", "A300"]
        var target string = "A200"
        var found bool = false
        
        search accounts
            when accounts[i] == target {
                found = true
                display("Account found at position " + i)
            }
        
        if !found {
            display("Account not found")
        }
    }
}
```

---

## File Operations

### Reading Files

```go
job FileReadDemo {
    step ReadCustomerFile {
        var customerFile file
        var record string
        
        // Open file for input
        open customerFile
            file "customers.txt"
            mode input
        
        // Read first record
        read customerFile into record
        
        display("Record: " + record)
        
        // Close file
        close customerFile
    }
}
```

### Writing Files

```go
job FileWriteDemo {
    step WriteReport {
        var reportFile file
        var reportLine string = "Transaction Report - 2025-11-02"
        
        // Open file for output
        open reportFile
            file "report.txt"
            mode output
        
        // Write record
        write reportFile from reportLine
        
        // Close file
        close reportFile
        
        display("Report written successfully")
    }
}
```

---

## Using the DSL Tools

### 1. DSL Compiler (dslc)

**Basic Usage:**
```bash
./bin/dslc input.cobgo -o output.go
```

**With Options:**
```bash
# Verbose output
./bin/dslc -v input.cobgo -o output.go

# Specify output directory
./bin/dslc input.cobgo -o ./generated/output.go

# Compile multiple files
./bin/dslc file1.cobgo file2.cobgo -o combined.go
```

### 2. DSL Formatter (dslfmt)

**Format a File:**
```bash
./bin/dslfmt -w myprogram.cobgo
```

**Check Formatting (No Changes):**
```bash
./bin/dslfmt myprogram.cobgo
```

### 3. DSL Linter (dsllint)

**Check Code Quality:**
```bash
./bin/dsllint myprogram.cobgo
```

**Output Example:**
```
myprogram.cobgo:12:5: Unused variable: tempValue
myprogram.cobgo:25:1: Empty block in if statement
```

### 4. Copybook Converter

**Convert COBOL Copybook to DSL:**
```bash
./bin/copybook2dsl customer.cpy -o customer_record.cobgo
```

---

## Common Patterns

### 1. Banking Transaction Processing

```go
job ProcessTransaction {
    step ValidateAccount {
        var accountNum int32 = 123456
        var balance decimal = 1000.00
        var transactionAmt decimal = 50.00
        
        if balance >= transactionAmt {
            display("Transaction approved")
        } else {
            display("Insufficient funds")
        }
    }
}
```

### 2. Customer Data Validation

```go
job ValidateCustomer {
    step CheckData {
        var customerName string = "John Doe"
        var ssn string = "123-45-6789"
        var nameLength int32
        
        // Count characters in name
        inspect customerName tallying nameLength for characters
        
        if nameLength > 0 and nameLength < 100 {
            display("Name valid: " + customerName)
        } else {
            display("Invalid name length")
        }
    }
}
```

### 3. Report Generation

```go
job GenerateReport {
    step CreateHeader {
        var reportTitle string
        var reportDate string = "2025-11-02"
        
        string "Monthly Summary - " delimited by size
               reportDate delimited by size
               into reportTitle
        
        display(reportTitle)
        display("=" * 50)
    }
    
    step ShowSummary {
        var totalTransactions int32 = 1250
        var totalAmount decimal = 45780.50
        
        display("Total Transactions: " + totalTransactions)
        display("Total Amount: $" + totalAmount)
    }
}
```

---

## Debugging Tips

### 1. Enable Verbose Logging

```bash
./bin/dslc -v myprogram.cobgo -o output.go
```

### 2. Check Generated Go Code

After compilation, inspect the generated Go code to understand what's being produced:

```bash
cat output.go
```

### 3. Use Display Statements

Add display statements to trace program execution:

```go
display("DEBUG: Entering step ProcessData")
display("DEBUG: Balance = " + balance)
```

### 4. Common Errors

**Error: Undefined variable**
```
Error: line 10: undefined variable 'accountNum'
```
**Solution:** Make sure all variables are declared with `var` before use.

**Error: Type mismatch**
```
Error: line 15: cannot assign string to decimal
```
**Solution:** Ensure variable types match the assigned values.

**Error: Parse error**
```
Error: line 8: expected '{' after job name
```
**Solution:** Check syntax - all jobs/steps need braces `{...}`

---

## Next Steps

Now that you've learned the basics:

1. **Tutorial 2**: Learn how to run a banking pilot
2. **Tutorial 3**: Advanced features (DB2, CICS integration)
3. **Tutorial 4**: Migrating existing COBOL code
4. **Tutorial 5**: Performance optimization

**Additional Resources:**
- `docs/COBOL85_SUPPORT.md` - Complete language reference
- `examples/` - Sample programs
- `tests/compliance/` - Test examples

---

## Quick Reference

### Essential Commands

```bash
# Compile DSL to Go
./bin/dslc program.cobgo -o program.go

# Format code
./bin/dslfmt -w program.cobgo

# Lint code
./bin/dsllint program.cobgo

# Run generated code
go run program.go
```

### Basic Program Structure

```go
job JobName {
    step StepName {
        var variableName type = value
        // Your code here
        display("Output")
    }
}
```

### Data Types Cheat Sheet

| Type | Example | Use Case |
|------|---------|----------|
| `string` | `"John Doe"` | Names, addresses |
| `int32` | `12345` | Account numbers, counts |
| `int64` | `9876543210` | Large IDs, timestamps |
| `decimal` | `1000.50` | Money, interest rates |
| `bool` | `true/false` | Flags, status |
| `date` | `"2025-11-02"` | Dates |

---

**Congratulations! You're ready to start developing with CobGO!** 🎉

Next: [Tutorial 2 - Running a Banking Pilot](02-banking-pilot.md)

