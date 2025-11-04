# CobGO Language Specification

## Overview

CobGO is a Domain Specific Language (DSL) designed to modernize COBOL applications by providing a clean, Go-like syntax while preserving COBOL semantics and business logic integrity.

## Design Principles

1. **COBOL Compatibility**: Preserve COBOL data types, operations, and semantics
2. **Go Integration**: Generate idiomatic Go code with proper error handling
3. **Readability**: Clean, modern syntax that's easier to understand than COBOL
4. **Type Safety**: Strong typing with compile-time validation
5. **Performance**: Efficient compilation and runtime execution

## Core Data Types

### Primitive Types

#### Decimal
```cobgo
var amount decimal(15,2) = 1234.56
var rate decimal(5,4) = 0.1250
```
- **COBOL Mapping**: `PIC 9(n)V9(m)` or `PIC S9(n)V9(m)`
- **Go Mapping**: `decimal.Decimal` (using shopspring/decimal)
- **Semantics**: Fixed-point decimal arithmetic with specified precision

#### String
```cobgo
var name string(50) = "John Doe"
var code string(10)
```
- **COBOL Mapping**: `PIC X(n)`
- **Go Mapping**: `string` with length validation
- **Semantics**: Fixed-length character strings

#### Date
```cobgo
var birth_date date = "2024-01-15"
var current_date date = today()
```
- **COBOL Mapping**: `PIC 9(8)` (YYYYMMDD) or `PIC X(10)` (YYYY-MM-DD)
- **Go Mapping**: `time.Time`
- **Semantics**: Date values with various formats

#### Integer
```cobgo
var count int32 = 100
var id int64 = 123456789
```
- **COBOL Mapping**: `PIC 9(n)` or `PIC S9(n)`
- **Go Mapping**: `int32`, `int64`
- **Semantics**: Signed/unsigned integers

#### Boolean
```cobgo
var is_valid bool = true
var flag bool
```
- **COBOL Mapping**: `PIC X` with 'Y'/'N' or 88-level conditions
- **Go Mapping**: `bool`
- **Semantics**: True/false values

### Composite Types

#### Record
```cobgo
record Customer {
    id int32
    name string(50)
    email string(100)
    balance decimal(15,2)
    created_date date
    is_active bool
}
```
- **COBOL Mapping**: 01-level group items
- **Go Mapping**: `struct`
- **Semantics**: Structured data with named fields

#### Array
```cobgo
var items string(20)[10]  // Fixed-size array
var names string(50)[]    // Dynamic array
```
- **COBOL Mapping**: `OCCURS` clause
- **Go Mapping**: `[]T` or `[N]T`
- **Semantics**: Collections of elements

## Control Structures

### Job (Main Program)
```cobgo
job MainProgram {
    // Data declarations
    var customer Customer
    
    // Main logic
    step Initialize {
        customer.id = 1
        customer.name = "John Doe"
    }
    
    step Process {
        display("Processing customer: " + customer.name)
        // Business logic here
    }
}
```
- **COBOL Mapping**: `PROGRAM-ID` and main procedure division
- **Go Mapping**: `func main()`
- **Semantics**: Top-level program container

### Step (Procedure)
```cobgo
step CalculateTax(amount decimal(15,2)) decimal(15,2) {
    var tax_rate decimal(5,4) = 0.0875
    var tax_amount decimal(15,2) = amount * tax_rate
    return tax_amount
}
```
- **COBOL Mapping**: `PARAGRAPH` or `SECTION`
- **Go Mapping**: `func`
- **Semantics**: Reusable procedure with parameters and return values

### For Loop
```cobgo
for i := 1 to 10 {
    display("Iteration: " + string(i))
}

for item in items {
    display("Item: " + item)
}
```
- **COBOL Mapping**: `PERFORM VARYING`
- **Go Mapping**: `for` loop
- **Semantics**: Iteration over ranges or collections

### If-Then-Else
```cobgo
if customer.balance > 1000.00 {
    display("Premium customer")
} else if customer.balance > 100.00 {
    display("Standard customer")
} else {
    display("Basic customer")
}
```
- **COBOL Mapping**: `IF-THEN-ELSE`
- **Go Mapping**: `if-else`
- **Semantics**: Conditional execution

### Switch/Case
```cobgo
switch customer.status {
case "ACTIVE":
    display("Customer is active")
case "INACTIVE":
    display("Customer is inactive")
case "SUSPENDED":
    display("Customer is suspended")
default:
    display("Unknown status")
}
```
- **COBOL Mapping**: `EVALUATE`
- **Go Mapping**: `switch`
- **Semantics**: Multi-way conditional execution

## I/O Operations

### Display (Output)
```cobgo
display("Hello, World!")
display("Customer: " + customer.name)
display("Balance: " + string(customer.balance))
```
- **COBOL Mapping**: `DISPLAY`
- **Go Mapping**: `fmt.Print` or `fmt.Println`
- **Semantics**: Console output

### Accept (Input)
```cobgo
var input_name string(50)
accept("Enter name: ", input_name)

var input_amount decimal(15,2)
accept("Enter amount: ", input_amount)
```
- **COBOL Mapping**: `ACCEPT`
- **Go Mapping**: `fmt.Scan` or custom input functions
- **Semantics**: Console input

### File Operations
```cobgo
file customer_file {
    record Customer
    organization: sequential
    access: sequential
}

// Read from file
read customer_file into customer

// Write to file
write customer to customer_file
```
- **COBOL Mapping**: `SELECT` and `FD` with `READ`/`WRITE`
- **Go Mapping**: File I/O with proper error handling
- **Semantics**: File-based data persistence

## Arithmetic Operations

### Basic Operations
```cobgo
var result decimal(15,2)

// Addition
result = amount1 + amount2

// Subtraction
result = amount1 - amount2

// Multiplication
result = amount1 * amount2

// Division
result = amount1 / amount2

// Modulo
var remainder int32 = value1 % value2
```

### String Operations
```cobgo
var full_name string(100)

// Concatenation
full_name = first_name + " " + last_name

// Substring
var initials string(2) = full_name[0:2]

// Length
var name_length int32 = length(full_name)

// Trim
var clean_name string(50) = trim(input_name)
```

### Date Operations
```cobgo
var today date = current_date()
var tomorrow date = today + 1 day
var diff_days int32 = tomorrow - today

// Format date
var formatted string(10) = format_date(today, "YYYY-MM-DD")
```

## Error Handling

### Try-Catch
```cobgo
try {
    var result decimal(15,2) = amount / divisor
    display("Result: " + string(result))
} catch DivisionByZero {
    display("Error: Division by zero")
} catch OverflowError {
    display("Error: Arithmetic overflow")
}
```
- **COBOL Mapping**: `ON SIZE ERROR` and `ON OVERFLOW`
- **Go Mapping**: `panic`/`recover` or error handling
- **Semantics**: Exception handling for runtime errors

## Comments

```cobgo
// Single-line comment
var value int32 = 100  // Inline comment

/*
 * Multi-line comment
 * for complex explanations
 */
```

## COBOL PIC to DSL Type Mapping

| COBOL PIC | DSL Type | Go Type | Description |
|-----------|----------|---------|-------------|
| `PIC 9(n)` | `int32` | `int32` | Unsigned integer |
| `PIC S9(n)` | `int32` | `int32` | Signed integer |
| `PIC 9(n)V9(m)` | `decimal(n+m,m)` | `decimal.Decimal` | Fixed decimal |
| `PIC X(n)` | `string(n)` | `string` | Character string |
| `PIC 9(8)` | `date` | `time.Time` | Date (YYYYMMDD) |
| `PIC X(10)` | `date` | `time.Time` | Date (YYYY-MM-DD) |
| `PIC 9(6)` | `time` | `time.Time` | Time (HHMMSS) |

## Example: Complete Program

```cobgo
// Customer Management System
record Customer {
    id int32
    name string(50)
    email string(100)
    balance decimal(15,2)
    status string(10)
    created_date date
}

job CustomerManagement {
    var customer Customer
    var choice string(1)
    
    step Main {
        display("=== Customer Management System ===")
        
        loop {
            display("1. Add Customer")
            display("2. View Customer")
            display("3. Update Balance")
            display("4. Exit")
            accept("Choose option: ", choice)
            
            switch choice {
            case "1":
                step AddCustomer()
            case "2":
                step ViewCustomer()
            case "3":
                step UpdateBalance()
            case "4":
                display("Goodbye!")
                exit
            default:
                display("Invalid option")
            }
        }
    }
    
    step AddCustomer() {
        accept("Enter customer ID: ", customer.id)
        accept("Enter customer name: ", customer.name)
        accept("Enter email: ", customer.email)
        customer.balance = 0.00
        customer.status = "ACTIVE"
        customer.created_date = current_date()
        
        display("Customer added successfully!")
    }
    
    step ViewCustomer() {
        if customer.id > 0 {
            display("ID: " + string(customer.id))
            display("Name: " + customer.name)
            display("Email: " + customer.email)
            display("Balance: " + string(customer.balance))
            display("Status: " + customer.status)
        } else {
            display("No customer data")
        }
    }
    
    step UpdateBalance() {
        var amount decimal(15,2)
        accept("Enter amount to add: ", amount)
        
        customer.balance = customer.balance + amount
        display("New balance: " + string(customer.balance))
    }
}
```

## Compilation Process

1. **Lexical Analysis**: Tokenize the source code
2. **Syntax Analysis**: Parse tokens into Abstract Syntax Tree (AST)
3. **Semantic Analysis**: Type checking and validation
4. **Code Generation**: Transform AST to Go source code
5. **Optimization**: Apply Go-specific optimizations

## Future Extensions

- **Object-Oriented Features**: Classes, inheritance, polymorphism
- **Concurrency**: Goroutines and channels
- **Database Integration**: SQL operations
- **Web Services**: HTTP client/server capabilities
- **Testing Framework**: Built-in unit testing support
