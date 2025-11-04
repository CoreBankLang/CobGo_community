# CobGO Quick Start Guide

**Get up and running with CobGO in 15 minutes!**

---

## 1. Install (5 minutes)

```bash
# Clone repository
git clone https://github.com/cobgo/cobgo.git
cd cobgo

# Build tools (Linux/Mac)
make build-all

# Build tools (Windows)
.\build.ps1

# Verify installation
./bin/dslc --version
```

✅ **Done!** You now have `dslc`, `dslfmt`, `dsllint`, and `copybook2dsl`.

---

## 2. Write Your First Program (3 minutes)

Create `hello-banking.cobgo`:

```go
job HelloBanking {
    step Welcome {
        var bankName string = "First National Bank"
        var balance decimal = 1000.50
        
        display("Welcome to " + bankName)
        display("Your balance: $" + balance)
    }
}
```

---

## 3. Compile and Run (2 minutes)

```bash
# Compile to Go
./bin/dslc hello-banking.cobgo -o hello.go

# Run it
go run hello.go
```

**Output:**
```
Welcome to First National Bank
Your balance: $1000.50
```

---

## 4. Try COBOL-85 Features (5 minutes)

Create `banking-demo.cobgo`:

```go
job BankingDemo {
    step ProcessCustomer {
        // Customer data
        var customerName string = "John Doe"
        var ssn string = "123-45-6789"
        var balance decimal = 5000.00
        
        // Mask SSN (INSPECT statement)
        inspect ssn replacing first "123-45" by "XXX-XX"
        
        // Build greeting (STRING statement)
        var greeting string
        string "Dear " delimited by size
               customerName delimited by size
               into greeting
        
        // Display results
        display(greeting)
        display("SSN: " + ssn)
        display("Balance: $" + balance)
        
        // Process transactions
        if balance >= 100.00 {
            balance = balance - 100.00
            display("Withdrawal: $100.00")
            display("New Balance: $" + balance)
        }
    }
}
```

**Compile and run:**

```bash
./bin/dslc banking-demo.cobgo -o demo.go
go run demo.go
```

**Output:**
```
Dear John Doe
SSN: XXX-XX-6789
Balance: $5000.00
Withdrawal: $100.00
New Balance: $4900.00
```

---

## 5. Use the Tools

### Format your code:
```bash
./bin/dslfmt -w banking-demo.cobgo
```

### Lint for issues:
```bash
./bin/dsllint banking-demo.cobgo
```

### Convert COBOL copybooks:
```bash
./bin/copybook2dsl customer.cpy -o customer.cobgo
```

---

## Next Steps

**You're ready!** Choose your path:

### For Learning:
📖 [Tutorial 1: Getting Started](01-getting-started.md) - Comprehensive guide

### For Production:
🏦 [Tutorial 2: Banking Pilot](02-banking-pilot.md) - Full pilot guide

### For Reference:
📚 [COBOL85_SUPPORT.md](../docs/COBOL85_SUPPORT.md) - Language reference

---

## Cheat Sheet

### Basic Syntax

```go
// Program structure
job JobName {
    step StepName {
        var name type = value
        display("output")
    }
}

// Data types
var text string = "value"
var count int32 = 123
var amount decimal = 999.99
var flag bool = true

// Control flow
if condition {
    // code
}

for i = 1; i <= 10; i = i + 1 {
    // code
}

while condition {
    // code
}

// COBOL-85
inspect string tallying count for all "X"
string src into dest
unstring input delimited by "," into f1, f2
search table when condition { }
```

### Essential Commands

```bash
# Compile
./bin/dslc input.cobgo -o output.go

# Format
./bin/dslfmt -w input.cobgo

# Lint
./bin/dsllint input.cobgo

# Run
go run output.go
```

---

## Common Use Cases

### 1. Interest Calculation
```go
job InterestCalc {
    step Calculate {
        var principal decimal = 10000.00
        var rate decimal = 0.05
        var interest decimal = principal * rate
        display("Interest: $" + interest)
    }
}
```

### 2. Data Validation
```go
job ValidateData {
    step Check {
        var balance decimal = 500.00
        if balance >= 100.00 {
            display("Valid balance")
        } else {
            display("Below minimum")
        }
    }
}
```

### 3. String Processing
```go
job ParseData {
    step Parse {
        var csv string = "Doe,John,12345"
        var last, first, id string
        unstring csv delimited by "," into last, first, id
        display("Name: " + first + " " + last)
    }
}
```

---

## Troubleshooting

**Build fails?**
- Ensure Go 1.21+ is installed: `go version`
- Check path: `echo $PATH`

**Compilation errors?**
- Check syntax in your `.cobgo` file
- Run with verbose: `./bin/dslc -v file.cobgo`

**Output unexpected?**
- Use `display()` to debug
- Check data types match
- Review generated Go code

---

## Support

- **Docs:** `docs/` folder
- **Examples:** `examples/` folder
- **Tests:** `tests/` folder
- **Tutorials:** `tutorials/` folder

---

**🎉 You're ready to modernize banking applications with CobGO!**

**Time spent:** 15 minutes  
**Skills gained:** DSL basics, compilation, COBOL-85 features  
**Next:** Choose a tutorial above

---

**Happy coding!** 🚀

