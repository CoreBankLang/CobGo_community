# CobGO Tutorials

Welcome to the CobGO tutorial series! These step-by-step guides will help you learn how to use CobGO for banking application modernization.

## 📚 Tutorial Series

### **Beginner Level**

#### [Tutorial 1: Getting Started](01-getting-started.md) ⭐
**Time:** 1 hour  
**Prerequisites:** None

Learn the basics of CobGO DSL:
- Installation and setup
- First "Hello World" program
- Basic syntax and data types
- Control flow and operations
- Using the DSL tools

**Start here if you're new to CobGO!**

---

### **Intermediate Level**

#### [Tutorial 2: Running a Banking Pilot](02-banking-pilot.md) ⭐⭐
**Time:** 2-4 hours  
**Prerequisites:** Tutorial 1

Complete hands-on banking pilot:
- Planning and scoping a pilot
- Environment setup
- Migrating real banking programs
- Validation and testing
- Performance benchmarking
- Documentation and handoff

**Best for:** Teams evaluating CobGO for production use

---

### **Advanced Level** (Coming Soon)

#### Tutorial 3: Advanced Features ⭐⭐⭐
**Time:** 3-4 hours  
**Prerequisites:** Tutorial 2

Deep dive into enterprise features:
- DB2 integration (EXEC SQL)
- CICS transaction processing
- Batch job processing
- Copybook handling
- Performance optimization

**Best for:** Teams implementing production systems

---

## 🎯 Learning Paths

### For Individual Developers

```
1. Getting Started (1 hour)
   ↓
2. Build 3-5 sample programs (2 hours)
   ↓
3. Banking Pilot tutorial (4 hours)
   ↓
4. Advanced Features (4 hours)
```

**Total Time:** ~11 hours to proficiency

---

### For Pilot Teams

```
Week 1: Team Setup
├─ Day 1-2: All team members complete Tutorial 1
├─ Day 3-4: Code review of sample programs
└─ Day 5: Team Q&A session

Week 2: Pilot Execution
├─ Day 1: Follow Tutorial 2 - Phase 1-2
├─ Day 2-3: Follow Tutorial 2 - Phase 3-4
├─ Day 4: Follow Tutorial 2 - Phase 5-6
└─ Day 5: Documentation and review

Week 3: Advanced Topics
└─ Selected advanced features as needed
```

---

### For Enterprise Evaluation

```
Phase 1: Assessment (Week 1-2)
├─ Management review of capabilities
├─ Technical team completes Tutorial 1
└─ Identify 3-5 pilot programs

Phase 2: Proof of Concept (Week 3-4)
├─ Follow Tutorial 2 with real programs
├─ Validate results
└─ Performance testing

Phase 3: Decision (Week 5-6)
├─ Review pilot results
├─ Plan full migration (if approved)
└─ Training for larger team
```

---

## 📖 Quick Reference

### Essential Commands

```bash
# Compile DSL to Go
./bin/dslc program.cobgo -o program.go

# Format code
./bin/dslfmt -w program.cobgo

# Lint code
./bin/dsllint program.cobgo

# Convert copybook
./bin/copybook2dsl customer.cpy -o customer.cobgo

# Run generated code
go run program.go
```

### Basic Program Template

```go
// Program description
job ProgramName {
    step MainStep {
        // Declare variables
        var variableName type = initialValue
        
        // Your logic here
        display("Output")
    }
}
```

### Common Data Types

| Type | Example | Banking Use Case |
|------|---------|------------------|
| `string` | `"John Doe"` | Customer names, descriptions |
| `int32` | `12345` | Account numbers, counts |
| `int64` | `9876543210` | Transaction IDs |
| `decimal` | `1000.50` | Money amounts, rates |
| `bool` | `true/false` | Status flags |
| `date` | `"2025-11-02"` | Transaction dates |

---

## 🎓 Additional Learning Resources

### Documentation
- **[COBOL85_SUPPORT.md](../docs/COBOL85_SUPPORT.md)** - Complete language reference
- **[ARCHITECTURE.md](../docs/ARCHITECTURE.md)** - System architecture
- **[API.md](../docs/API.md)** - API documentation
- **[SETUP.md](../docs/SETUP.md)** - Detailed setup guide

### Examples
- **[examples/](../examples/)** - Sample programs
  - `hello.cobgo` - Simple hello world
  - `calculator.cobgo` - Arithmetic operations
  - `customer.cpy` - Copybook example
  - `examples/migration/` - Migration examples

### Test Cases
- **[tests/compliance/](../tests/compliance/)** - Compliance test examples
- **[tests/cobol85/](../tests/cobol85/)** - COBOL-85 statement tests
- **[tests/banking/](../tests/banking/)** - Banking scenario tests

### Reports & Guides
- **[BANKING_PILOT_MANIFEST.md](../BANKING_PILOT_MANIFEST.md)** - Pilot checklist
- **[COBOL85_COMPLETION_SUMMARY.md](../COBOL85_COMPLETION_SUMMARY.md)** - Implementation details
- **[banking-production.plan.md](../banking-production.plan.md)** - Production roadmap

---

## 💡 Tips for Success

### 1. Start Small
Begin with Tutorial 1 and simple programs. Don't jump directly to complex migrations.

### 2. Practice Regularly
Build 2-3 programs per day when learning. Repetition builds proficiency.

### 3. Use the Examples
Study the example programs in `examples/` - they demonstrate best practices.

### 4. Run the Tests
Examine test cases in `tests/` to understand expected behavior.

### 5. Ask Questions
- Check documentation in `docs/`
- Review GitHub issues
- Consult with the CobGO team

---

## 🎯 Tutorial Goals

By completing these tutorials, you will be able to:

**After Tutorial 1:**
- ✅ Write basic CobGO DSL programs
- ✅ Use all DSL tools (compiler, formatter, linter)
- ✅ Understand data types and control flow
- ✅ Debug common issues

**After Tutorial 2:**
- ✅ Plan and execute a banking pilot
- ✅ Migrate COBOL programs to DSL
- ✅ Validate translation correctness
- ✅ Measure performance
- ✅ Document results for stakeholders

**After Tutorial 3:** (Coming soon)
- ✅ Integrate with DB2 databases
- ✅ Process CICS transactions
- ✅ Handle batch jobs
- ✅ Optimize for production

---

## 📊 Skill Assessment

### Beginner ⭐
- Can write simple DSL programs
- Understands basic syntax
- Can compile and run code
- **Recommended:** Complete Tutorial 1

### Intermediate ⭐⭐
- Can migrate COBOL programs
- Understands banking domain
- Can validate translations
- **Recommended:** Complete Tutorial 2

### Advanced ⭐⭐⭐
- Can handle enterprise features
- Optimizes performance
- Mentors others
- **Recommended:** Complete Tutorial 3

---

## 🚀 Getting Started

**New to CobGO?**

1. Clone the repository
2. Build the tools: `make build-all` or `.\build.ps1`
3. Start with [Tutorial 1: Getting Started](01-getting-started.md)
4. Complete the exercises
5. Move to Tutorial 2 when ready

**Ready for a pilot?**

Jump to [Tutorial 2: Running a Banking Pilot](02-banking-pilot.md)

---

## 📞 Support

### Need Help?

- **Documentation:** Check `docs/` folder
- **Examples:** Review `examples/` folder
- **Tests:** Study `tests/` folder for patterns
- **Issues:** GitHub Issues for bug reports
- **Questions:** GitHub Discussions for questions

### Contributing

Found an issue in the tutorials? Want to add content?

1. Create an issue describing the problem/enhancement
2. Submit a pull request with your improvements
3. Help others in discussions

---

## 📈 Progress Tracking

Use this checklist to track your learning:

### Tutorial 1: Getting Started
- [ ] Installed CobGO tools
- [ ] Compiled "Hello World"
- [ ] Understand data types
- [ ] Written 3+ practice programs
- [ ] Used formatter and linter

### Tutorial 2: Banking Pilot
- [ ] Planned pilot scope
- [ ] Set up environment
- [ ] Migrated 3+ programs
- [ ] Validated outputs
- [ ] Measured performance
- [ ] Created pilot report

### Tutorial 3: Advanced Features
- [ ] DB2 integration working
- [ ] CICS transactions tested
- [ ] Batch jobs running
- [ ] Performance optimized
- [ ] Production-ready

---

## 🎉 Congratulations!

Welcome to the CobGO community! These tutorials will help you modernize banking applications efficiently and safely.

**Let's get started!** → [Tutorial 1: Getting Started](01-getting-started.md)

---

**Last Updated:** November 2, 2025  
**Version:** 1.0.0  
**Status:** Production Ready ✅

