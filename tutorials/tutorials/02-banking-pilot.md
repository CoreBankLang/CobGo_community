# Running a Banking Pilot with CobGO

## Overview

This tutorial guides you through setting up and running a complete banking pilot program using CobGO. You'll learn how to structure a pilot project, migrate sample banking code, and validate the results.

**What You'll Learn:**
- Planning a banking pilot
- Setting up the pilot environment
- Migrating COBOL banking programs
- Running validation tests
- Measuring success metrics

**Prerequisites:**
- Completed [Tutorial 1 - Getting Started](01-getting-started.md)
- Understanding of banking domain concepts
- Access to sample COBOL banking code (or use our examples)

**Estimated Time:** 2-4 hours

---

## Phase 1: Pilot Planning (30 minutes)

### Define Pilot Scope

**Recommended First Pilot Programs:**
1. **Customer Data Validation** (Low complexity)
2. **Simple Interest Calculator** (Medium complexity)
3. **Transaction Processing** (Medium-high complexity)

**Avoid for First Pilot:**
- ❌ Complex DB2 integration
- ❌ CICS online transactions
- ❌ Large batch processing (>1000 LOC)
- ❌ Programs with extensive external dependencies

### Success Criteria

Define measurable goals:

```markdown
## Pilot Success Criteria

### Functional
- [ ] All test cases pass (COBOL vs Go output match)
- [ ] Performance within 10% of COBOL
- [ ] No data loss or corruption

### Quality
- [ ] Code compiles without errors
- [ ] Linter reports no critical issues
- [ ] Generated code is readable and maintainable

### Process
- [ ] Migration completed in estimated time
- [ ] Documentation is comprehensive
- [ ] Team can understand and modify generated code
```

---

## Phase 2: Environment Setup (30 minutes)

### Directory Structure

Create a structured pilot workspace:

```bash
# Create pilot directory
mkdir banking-pilot
cd banking-pilot

# Create subdirectories
mkdir -p {cobol-source,dsl-source,generated-go,test-data,validation,docs}

# Directory structure:
# banking-pilot/
#   ├── cobol-source/      # Original COBOL programs
#   ├── dsl-source/        # Converted DSL programs
#   ├── generated-go/      # Generated Go code
#   ├── test-data/         # Input/output test files
#   ├── validation/        # Validation scripts and results
#   └── docs/              # Pilot documentation
```

### Sample Data Setup

Create test data files:

**test-data/customers.txt:**
```
00001|John Doe|1000.50|ACTIVE
00002|Jane Smith|2500.75|ACTIVE
00003|Bob Johnson|150.00|INACTIVE
00004|Alice Williams|5000.00|ACTIVE
```

**test-data/transactions.txt:**
```
TX001|00001|100.00|DEPOSIT
TX002|00002|50.00|WITHDRAWAL
TX003|00001|25.50|WITHDRAWAL
TX004|00004|1000.00|DEPOSIT
```

---

## Phase 3: Program Migration (1-2 hours)

### Example 1: Customer Validation Program

**Original COBOL (customer-validator.cob):**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTVAL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-RECORD.
           05  CUST-ID          PIC 9(5).
           05  CUST-NAME        PIC X(30).
           05  CUST-BALANCE     PIC 9(7)V99.
           05  CUST-STATUS      PIC X(10).
       
       01  WS-VALID-FLAG        PIC X VALUE 'N'.
       01  WS-NAME-LENGTH       PIC 99 VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 12345 TO CUST-ID.
           MOVE 'JOHN DOE' TO CUST-NAME.
           MOVE 1000.50 TO CUST-BALANCE.
           MOVE 'ACTIVE' TO CUST-STATUS.
           
           PERFORM VALIDATE-CUSTOMER.
           
           IF WS-VALID-FLAG = 'Y'
               DISPLAY 'CUSTOMER VALID: ' CUST-NAME
           ELSE
               DISPLAY 'CUSTOMER INVALID'
           END-IF.
           
           STOP RUN.
       
       VALIDATE-CUSTOMER.
           INSPECT CUST-NAME TALLYING WS-NAME-LENGTH 
               FOR CHARACTERS.
           
           IF WS-NAME-LENGTH > 0 AND WS-NAME-LENGTH < 31
              AND CUST-BALANCE >= 0
              AND (CUST-STATUS = 'ACTIVE' OR 
                   CUST-STATUS = 'INACTIVE')
               MOVE 'Y' TO WS-VALID-FLAG
           ELSE
               MOVE 'N' TO WS-VALID-FLAG
           END-IF.
```

**Converted DSL (customer-validator.cobgo):**

```go
// Customer Validation Program
// Converted from COBOL CUSTVAL

record CustomerRecord {
    custID int32
    custName string
    custBalance decimal
    custStatus string
}

job CustomerValidator {
    step MainLogic {
        // Initialize customer record
        var customer CustomerRecord
        customer.custID = 12345
        customer.custName = "JOHN DOE"
        customer.custBalance = 1000.50
        customer.custStatus = "ACTIVE"
        
        // Validate
        var validFlag string = "N"
        validFlag = ValidateCustomer(customer)
        
        if validFlag == "Y" {
            display("CUSTOMER VALID: " + customer.custName)
        } else {
            display("CUSTOMER INVALID")
        }
    }
    
    step ValidateCustomer(cust CustomerRecord) string {
        var nameLength int32
        var isValid string = "N"
        
        // Check name length
        inspect cust.custName tallying nameLength for characters
        
        // Validation logic
        if nameLength > 0 and nameLength < 31
           and cust.custBalance >= 0
           and (cust.custStatus == "ACTIVE" or cust.custStatus == "INACTIVE") {
            isValid = "Y"
        } else {
            isValid = "N"
        }
        
        return isValid
    }
}
```

### Example 2: Interest Calculator

**DSL Version (interest-calculator.cobgo):**

```go
// Banking Interest Calculator
// Calculates simple and compound interest

record AccountInfo {
    accountNum int32
    principal decimal
    rate decimal
    years int32
}

job InterestCalculator {
    step Main {
        var account AccountInfo
        account.accountNum = 12345
        account.principal = 10000.00
        account.rate = 0.05
        account.years = 3
        
        // Calculate simple interest
        var simpleInterest decimal = CalculateSimpleInterest(account)
        
        // Calculate compound interest
        var compoundInterest decimal = CalculateCompoundInterest(account)
        
        // Display results
        DisplayResults(account, simpleInterest, compoundInterest)
    }
    
    step CalculateSimpleInterest(acct AccountInfo) decimal {
        var interest decimal
        interest = acct.principal * acct.rate * acct.years
        return interest
    }
    
    step CalculateCompoundInterest(acct AccountInfo) decimal {
        var amount decimal = acct.principal
        var year int32
        
        // Compound for each year
        for year = 1; year <= acct.years; year = year + 1 {
            amount = amount * (1 + acct.rate)
        }
        
        var interest decimal = amount - acct.principal
        return interest
    }
    
    step DisplayResults(acct AccountInfo, simple decimal, compound decimal) {
        display("=" * 50)
        display("INTEREST CALCULATION REPORT")
        display("=" * 50)
        display("Account Number: " + acct.accountNum)
        display("Principal: $" + acct.principal)
        display("Rate: " + (acct.rate * 100) + "%")
        display("Years: " + acct.years)
        display("-" * 50)
        display("Simple Interest: $" + simple)
        display("Compound Interest: $" + compound)
        display("=" * 50)
    }
}
```

### Example 3: Transaction Processing

**DSL Version (transaction-processor.cobgo):**

```go
// Transaction Processing System
// Processes deposits and withdrawals

record Transaction {
    txnID string
    accountNum int32
    amount decimal
    type string
}

record Account {
    accountNum int32
    balance decimal
    status string
}

job TransactionProcessor {
    step Main {
        // Initialize account
        var account Account
        account.accountNum = 12345
        account.balance = 1000.00
        account.status = "ACTIVE"
        
        display("Initial Balance: $" + account.balance)
        display("")
        
        // Process multiple transactions
        ProcessTransaction(account, "TX001", 500.00, "DEPOSIT")
        ProcessTransaction(account, "TX002", 200.00, "WITHDRAWAL")
        ProcessTransaction(account, "TX003", 50.00, "WITHDRAWAL")
        
        display("")
        display("Final Balance: $" + account.balance)
    }
    
    step ProcessTransaction(acct Account, txnID string, amount decimal, txnType string) {
        var success bool = false
        var newBalance decimal = acct.balance
        
        if acct.status != "ACTIVE" {
            display("ERROR: Account " + acct.accountNum + " is not active")
            return
        }
        
        if txnType == "DEPOSIT" {
            newBalance = acct.balance + amount
            success = true
        } else if txnType == "WITHDRAWAL" {
            if acct.balance >= amount {
                newBalance = acct.balance - amount
                success = true
            } else {
                display("ERROR: Insufficient funds for " + txnID)
                return
            }
        } else {
            display("ERROR: Invalid transaction type: " + txnType)
            return
        }
        
        if success {
            acct.balance = newBalance
            display("SUCCESS: " + txnID + " | " + txnType + " | $" + amount + " | Balance: $" + acct.balance)
        }
    }
}
```

---

## Phase 4: Compilation and Testing (30 minutes)

### Compile All Programs

Create a compilation script `compile-all.sh`:

```bash
#!/bin/bash

echo "=== CobGO Banking Pilot - Compilation ==="
echo ""

# Compile each program
programs=("customer-validator" "interest-calculator" "transaction-processor")

for prog in "${programs[@]}"; do
    echo "Compiling $prog..."
    ../bin/dslc dsl-source/${prog}.cobgo -o generated-go/${prog}.go
    
    if [ $? -eq 0 ]; then
        echo "✅ $prog compiled successfully"
    else
        echo "❌ $prog compilation failed"
        exit 1
    fi
    echo ""
done

echo "=== All programs compiled successfully! ==="
```

**Run compilation:**

```bash
chmod +x compile-all.sh
./compile-all.sh
```

### Test Individual Programs

```bash
# Test customer validator
cd generated-go
go run customer-validator.go

# Expected output:
# CUSTOMER VALID: JOHN DOE

# Test interest calculator
go run interest-calculator.go

# Expected output:
# ==================================================
# INTEREST CALCULATION REPORT
# ==================================================
# Account Number: 12345
# Principal: $10000.00
# Rate: 5%
# Years: 3
# --------------------------------------------------
# Simple Interest: $1500.00
# Compound Interest: $1576.25
# ==================================================

# Test transaction processor
go run transaction-processor.go

# Expected output:
# Initial Balance: $1000.00
#
# SUCCESS: TX001 | DEPOSIT | $500.00 | Balance: $1500.00
# SUCCESS: TX002 | WITHDRAWAL | $200.00 | Balance: $1300.00
# SUCCESS: TX003 | WITHDRAWAL | $50.00 | Balance: $1250.00
#
# Final Balance: $1250.00
```

---

## Phase 5: Validation (30-60 minutes)

### Create Validation Scripts

**validation/compare-outputs.sh:**

```bash
#!/bin/bash

echo "=== Output Validation ==="

# Function to compare outputs
compare_outputs() {
    local program=$1
    local cobol_output=$2
    local go_output=$3
    
    if diff -w <(echo "$cobol_output") <(echo "$go_output") > /dev/null; then
        echo "✅ $program: Outputs match"
        return 0
    else
        echo "❌ $program: Outputs differ"
        echo "COBOL Output:"
        echo "$cobol_output"
        echo ""
        echo "Go Output:"
        echo "$go_output"
        return 1
    fi
}

# Test customer validator
cobol_out="CUSTOMER VALID: JOHN DOE"
go_out=$(cd ../generated-go && go run customer-validator.go)
compare_outputs "Customer Validator" "$cobol_out" "$go_out"

echo ""
echo "=== Validation Complete ==="
```

### Performance Benchmarking

**validation/benchmark.sh:**

```bash
#!/bin/bash

echo "=== Performance Benchmarking ==="

# Run each program multiple times and measure time
for prog in customer-validator interest-calculator transaction-processor; do
    echo "Benchmarking $prog..."
    
    # Go version
    start=$(date +%s.%N)
    for i in {1..100}; do
        go run ../generated-go/${prog}.go > /dev/null 2>&1
    done
    end=$(date +%s.%N)
    
    duration=$(echo "$end - $start" | bc)
    avg=$(echo "scale=3; $duration / 100" | bc)
    
    echo "  Average execution time: ${avg}s"
    echo ""
done

echo "=== Benchmarking Complete ==="
```

---

## Phase 6: Documentation (30 minutes)

### Create Pilot Report

**docs/pilot-report.md:**

```markdown
# Banking Pilot Report - CobGO

## Executive Summary

**Pilot Date:** November 2, 2025
**Duration:** 4 hours
**Programs Migrated:** 3
**Success Rate:** 100%

## Programs Included

### 1. Customer Validator
- **Complexity:** Low
- **Lines of Code:** 45 (COBOL) → 65 (DSL)
- **Compilation:** ✅ Success
- **Validation:** ✅ Passed
- **Performance:** Within 5% of COBOL

### 2. Interest Calculator
- **Complexity:** Medium
- **Lines of Code:** 80 (COBOL) → 95 (DSL)
- **Compilation:** ✅ Success
- **Validation:** ✅ Passed
- **Performance:** Within 3% of COBOL

### 3. Transaction Processor
- **Complexity:** Medium-High
- **Lines of Code:** 120 (COBOL) → 135 (DSL)
- **Compilation:** ✅ Success
- **Validation:** ✅ Passed
- **Performance:** Within 8% of COBOL

## Test Results

| Program | Test Cases | Passed | Failed | Pass Rate |
|---------|-----------|--------|--------|-----------|
| Customer Validator | 5 | 5 | 0 | 100% |
| Interest Calculator | 8 | 8 | 0 | 100% |
| Transaction Processor | 12 | 12 | 0 | 100% |
| **TOTAL** | **25** | **25** | **0** | **100%** |

## Performance Metrics

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Compilation Time | <5 min | 2 min | ✅ |
| Runtime Performance | Within 10% | Within 8% | ✅ |
| Code Readability | High | High | ✅ |
| Maintainability | High | High | ✅ |

## Key Findings

### Successes ✅
1. All programs compiled without errors
2. 100% test pass rate
3. Generated code is clean and maintainable
4. Performance meets expectations
5. Team quickly understood DSL syntax

### Challenges ⚠️
1. Initial learning curve for DSL syntax (resolved in 30 min)
2. Manual conversion required for complex logic
3. Some COBOL idioms need adaptation

### Recommendations 📋
1. **Proceed to Phase 2:** Larger batch programs
2. **Add Features:** DB2 integration for next pilot
3. **Tooling:** Create automated converter for common patterns
4. **Training:** 1-day workshop for development team

## Cost-Benefit Analysis

### Migration Effort
- **COBOL Lines:** 245
- **DSL Lines:** 295 (20% increase)
- **Time to Migrate:** 2 hours
- **Time to Validate:** 1 hour

### Ongoing Maintenance (Estimated)
- **Readability:** +40% (Go vs COBOL)
- **Debugging Time:** -30% (better tooling)
- **Onboarding Time:** -50% (more developers know Go)

## Conclusion

**Status: PILOT SUCCESSFUL** ✅

CobGO successfully migrated and validated 3 banking programs with 100% accuracy. The tool is ready for larger-scale pilot programs including DB2 integration and batch processing.

**Recommendation:** APPROVE for Phase 2 pilot with 10-15 programs.

---

**Prepared by:** CobGO Pilot Team
**Date:** November 2, 2025
**Next Review:** Week of November 9, 2025
```

---

## Phase 7: Team Handoff (30 minutes)

### Knowledge Transfer Checklist

```markdown
## Pilot Handoff Checklist

### Documentation
- [ ] Pilot report completed
- [ ] All source code committed to repository
- [ ] Test results documented
- [ ] Performance benchmarks recorded

### Code Artifacts
- [ ] Original COBOL programs archived
- [ ] DSL source code reviewed and approved
- [ ] Generated Go code reviewed
- [ ] All tests passing

### Team Training
- [ ] DSL syntax walkthrough completed
- [ ] Compilation process demonstrated
- [ ] Validation process explained
- [ ] Q&A session held

### Next Steps Defined
- [ ] Phase 2 programs identified
- [ ] Timeline for next pilot established
- [ ] Resource allocation confirmed
- [ ] Success criteria defined
```

### Training Materials

Create quick reference cards:

**quickref-dsl-syntax.md:**
```markdown
# CobGO DSL Quick Reference

## Basic Structure
```go
job JobName {
    step StepName {
        // Code here
    }
}
```

## Variables
```go
var name string = "value"
var count int32 = 100
var balance decimal = 1000.50
```

## Control Flow
```go
if condition {
    // code
} else {
    // code
}

for i = 1; i <= 10; i = i + 1 {
    // code
}

while condition {
    // code
}
```

## COBOL-85 Features
```go
inspect string tallying count for all "X"
string src1 delimited by size into dest
unstring input delimited by "," into f1, f2
search table when condition { // code }
```
```

---

## Troubleshooting Common Issues

### Issue 1: Compilation Errors

**Error:** `expected next token to be IDENTIFIER`

**Solution:**
- Check for missing semicolons
- Ensure proper brace matching `{}`
- Verify keyword spelling

### Issue 2: Output Mismatches

**Error:** COBOL output differs from Go output

**Solution:**
- Check decimal precision settings
- Verify data type conversions
- Review arithmetic rounding behavior

### Issue 3: Performance Issues

**Error:** Generated code runs slower than expected

**Solution:**
- Profile the generated code
- Check for unnecessary string operations
- Optimize loops and iterations

---

## Next Steps

After completing this pilot:

1. **Immediate:**
   - Review pilot report with stakeholders
   - Gather team feedback
   - Document lessons learned

2. **Short Term (1-2 weeks):**
   - Plan Phase 2 pilot with 10-15 programs
   - Include DB2 integration
   - Add CICS transaction processing

3. **Medium Term (1-2 months):**
   - Production deployment planning
   - Performance optimization
   - Security hardening review

---

## Additional Resources

- **[Tutorial 3 - Advanced Features](03-advanced-features.md)** - DB2, CICS, batch processing
- **[Tutorial 4 - Migration Patterns](04-migration-patterns.md)** - Common COBOL to DSL conversions
- **BANKING_PILOT_MANIFEST.md** - Complete pilot requirements checklist
- **banking-production.plan.md** - Full production readiness plan

---

## Success! 🎉

**Congratulations on completing your banking pilot!**

You've successfully:
- ✅ Set up a pilot environment
- ✅ Migrated 3 banking programs
- ✅ Validated outputs and performance
- ✅ Documented results
- ✅ Prepared for Phase 2

**Your pilot is production-ready and validated!**

---

**Questions or Issues?**
- Check `docs/` for detailed documentation
- Review `examples/` for more sample code
- See `COMPREHENSIVE_REVIEW_PHASE1-5.md` for technical details

**Next:** [Tutorial 3 - Advanced Features](03-advanced-features.md)

