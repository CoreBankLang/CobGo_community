# CobGO Community Edition - Feature List

This document outlines what's included in the **Community Edition** versus the **Enterprise Edition**.

---

## ✅ Community Edition Features

### Core Compiler Infrastructure
- ✅ COBOL Parser (COBOL-74 standard)
- ✅ DSL Language Support
- ✅ Intermediate Representation (IR)
- ✅ Go Code Generation
- ✅ Basic Runtime Library

### Developer Tools
- ✅ `dslc` - Main compiler
- ✅ `dslfmt` - Code formatter
- ✅ `dsllint` - Static analyzer
- ✅ `copybook2dsl` - Copybook converter

### COBOL Language Support

#### Data Types
- ✅ All PIC clauses (9, X, A, S, V)
- ✅ DISPLAY format
- ✅ COMP format
- ✅ Basic PACKED-DECIMAL (COMP-3 basic support)
- ✅ OCCURS (fixed tables)
- ✅ REDEFINES
- ✅ Group items
- ✅ Level numbers (01-49, 77, 88)
- ✅ VALUE clauses

#### Control Structures
- ✅ IF-THEN-ELSE
- ✅ EVALUATE (CASE)
- ✅ PERFORM (loops)
- ✅ GO TO (with warnings)

#### Arithmetic
- ✅ ADD, SUBTRACT, MULTIPLY, DIVIDE
- ✅ COMPUTE (expressions)
- ✅ SIZE ERROR handling
- ✅ Basic rounding modes

#### File I/O
- ✅ Sequential files (OPEN, READ, WRITE, CLOSE)
- ✅ File status checking
- ✅ SELECT/ASSIGN
- ✅ FD (File Description)

#### Program Structure
- ✅ All four COBOL divisions
- ✅ Sections and paragraphs
- ✅ COPY statements (basic)

---

## 🏢 Enterprise Edition Features (Not Included)

### Enterprise Integrations
- ❌ DB2 Integration (EXEC SQL)
- ❌ CICS Support (EXEC CICS)
- ❌ Advanced Batch Processing (JCL)
- ❌ XML Processing (GENERATE/PARSE)

### Advanced Security
- ❌ Advanced encryption (AES-256-GCM)
- ❌ Credit card masking
- ❌ Advanced input validation
- ❌ Audit framework

### Compliance & Governance
- ❌ SOX compliance framework
- ❌ PCI-DSS compliance
- ❌ GDPR compliance
- ❌ HIPAA compliance
- ❌ GLBA compliance
- ❌ Basel III compliance

### Advanced COBOL Features
- ❌ All 10 ROUNDED modes (Banker's rounding, etc.)
- ❌ Full COMP-3 packed-decimal support
- ❌ Indexed file I/O (ISAM)
- ❌ Relative file I/O
- ❌ Advanced batch orchestration
- ❌ Checkpoint/restart capability
- ❌ GDG (Generation Data Group) support

### Migration Tools
- ❌ `cobgo-assess` - Migration assessment tool
- ❌ Risk assessment
- ❌ Compliance dashboard
- ❌ Migration planning tools

### Professional Services
- ❌ Professional support
- ❌ SLA guarantees
- ❌ Custom development
- ❌ Training and consulting

---

## 🔄 Upgrade Path

If you need enterprise features, you can:

1. **Contact Us**: Visit [cobgo.com/enterprise](https://cobgo.com/enterprise)
2. **Request Features**: Some enterprise features may be added to community over time
3. **Contribute**: Community contributions are welcome and may influence future releases

---

## 📊 Feature Comparison

| Feature | Community | Enterprise |
|---------|-----------|-----------|
| COBOL Parser | ✅ | ✅ |
| DSL Language | ✅ | ✅ |
| Go Code Generation | ✅ | ✅ |
| Basic Runtime | ✅ | ✅ |
| Sequential I/O | ✅ | ✅ |
| DB2 Integration | ❌ | ✅ |
| CICS Support | ❌ | ✅ |
| Batch Processing | ❌ | ✅ |
| Advanced Security | ❌ | ✅ |
| Compliance Frameworks | ❌ | ✅ |
| Migration Tools | ❌ | ✅ |
| Professional Support | ❌ | ✅ |

---

## 💡 Choosing the Right Edition

### Use Community Edition If:
- You're modernizing basic COBOL programs
- You need sequential file I/O only
- You don't need enterprise integrations
- You're learning or prototyping
- You want to contribute to open source

### Use Enterprise Edition If:
- You need DB2 or CICS integration
- You require compliance frameworks
- You need advanced security features
- You're migrating banking/financial systems
- You need professional support and SLA

---

## 🚀 Getting Started

See [README.md](README.md) for installation and quick start guide.

---

**CobGO Community Edition** - Open source COBOL modernization for everyone 🚀

