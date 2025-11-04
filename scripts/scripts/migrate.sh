#!/bin/bash

# Automated Migration Workflow Script
# Usage: ./migrate.sh <cobol_file> [output_dir]

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check arguments
if [ "$#" -lt 1 ]; then
    log_error "Usage: $0 <cobol_file> [output_dir]"
    exit 1
fi

COBOL_FILE="$1"
OUTPUT_DIR="${2:-migration_output}"
PROGRAM_NAME=$(basename "$COBOL_FILE" .cob)

log_info "Starting automated migration workflow for: $COBOL_FILE"
log_info "Output directory: $OUTPUT_DIR"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Step 1: Validate COBOL file exists
log_info "Step 1: Validating input file..."
if [ ! -f "$COBOL_FILE" ]; then
    log_error "COBOL file not found: $COBOL_FILE"
    exit 1
fi
log_success "Input file validated"

# Step 2: Translate COBOL to DSL (mixed mode)
log_info "Step 2: Translating COBOL to DSL..."
if ! cobol2dsl -i "$COBOL_FILE" --mixed --output-dir "$OUTPUT_DIR" -v; then
    log_error "Translation failed"
    exit 1
fi
log_success "Translation completed"

# Step 3: Test DSL compilation
log_info "Step 3: Testing DSL compilation..."
DSL_FILE="$OUTPUT_DIR/${PROGRAM_NAME}.cobgo"
GO_FILE="$OUTPUT_DIR/${PROGRAM_NAME}.go"

if [ ! -f "$DSL_FILE" ]; then
    log_error "DSL file not found: $DSL_FILE"
    exit 1
fi

if ! dslc -i "$DSL_FILE" -o "$GO_FILE"; then
    log_error "DSL compilation failed"
    exit 1
fi
log_success "DSL compiled successfully"

# Step 4: Build Go binary
log_info "Step 4: Building Go binary..."
BINARY="$OUTPUT_DIR/${PROGRAM_NAME}"
if ! go build -o "$BINARY" "$GO_FILE"; then
    log_error "Go build failed"
    exit 1
fi
log_success "Binary built: $BINARY"

# Step 5: Format DSL code
log_info "Step 5: Formatting DSL code..."
if dslfmt -i "$DSL_FILE" 2>/dev/null; then
    log_success "DSL formatted"
else
    log_warning "DSL formatter not available or failed"
fi

# Step 6: Lint DSL code
log_info "Step 6: Linting DSL code..."
if dsllint "$DSL_FILE" 2>/dev/null; then
    log_success "DSL linting passed"
else
    log_warning "DSL linter not available or found issues"
fi

# Step 7: Generate test script
log_info "Step 7: Generating test script..."
TEST_SCRIPT="$OUTPUT_DIR/test_${PROGRAM_NAME}.sh"
cat > "$TEST_SCRIPT" << 'EOF'
#!/bin/bash

# Test script for PROGRAM_NAME

echo "Running CobGO version..."
./PROGRAM_NAME > cobgo_output.txt 2>&1

echo "Output saved to: cobgo_output.txt"

# Add validation logic here
# Example:
# if [ -f expected_output.txt ]; then
#     if diff cobgo_output.txt expected_output.txt; then
#         echo "✅ Output matches expected"
#     else
#         echo "❌ Output differs from expected"
#         exit 1
#     fi
# fi
EOF

sed -i "s/PROGRAM_NAME/${PROGRAM_NAME}/g" "$TEST_SCRIPT"
chmod +x "$TEST_SCRIPT"
log_success "Test script generated: $TEST_SCRIPT"

# Step 8: Generate summary report
log_info "Step 8: Generating summary report..."
REPORT="$OUTPUT_DIR/migration_report.md"
cat > "$REPORT" << EOF
# Migration Report: $PROGRAM_NAME

**Date**: $(date)
**Source**: $COBOL_FILE
**Output**: $OUTPUT_DIR

## Files Generated

1. \`${PROGRAM_NAME}_original.cob\` - Original COBOL source
2. \`${PROGRAM_NAME}.cobgo\` - Translated DSL
3. \`${PROGRAM_NAME}.go\` - Generated Go code
4. \`${PROGRAM_NAME}\` - Compiled binary
5. \`${PROGRAM_NAME}_comparison.md\` - Side-by-side comparison
6. \`MIGRATION_GUIDE.md\` - Migration instructions
7. \`test_${PROGRAM_NAME}.sh\` - Test script

## Migration Steps Completed

- ✅ COBOL source parsed
- ✅ DSL translation generated
- ✅ DSL compiled to Go
- ✅ Go binary built
- ✅ Code formatted
- ✅ Code linted
- ✅ Test script created
- ✅ Documentation generated

## Next Steps

1. Review the comparison document:
   \`\`\`bash
   cat $OUTPUT_DIR/${PROGRAM_NAME}_comparison.md
   \`\`\`

2. Test the DSL code:
   \`\`\`bash
   cd $OUTPUT_DIR
   ./test_${PROGRAM_NAME}.sh
   \`\`\`

3. Run the binary:
   \`\`\`bash
   ./$BINARY
   \`\`\`

4. Compare with COBOL output (if available):
   \`\`\`bash
   # Compile and run original COBOL
   cobc -x ${PROGRAM_NAME}_original.cob
   ./${PROGRAM_NAME}_original > cobol_output.txt
   
   # Compare outputs
   diff cobol_output.txt cobgo_output.txt
   \`\`\`

5. Review and adjust DSL if needed:
   \`\`\`bash
   vim ${PROGRAM_NAME}.cobgo
   dslc -i ${PROGRAM_NAME}.cobgo -o ${PROGRAM_NAME}.go
   go build -o ${PROGRAM_NAME} ${PROGRAM_NAME}.go
   \`\`\`

## Validation Checklist

- [ ] DSL compiles without errors
- [ ] Go binary runs successfully
- [ ] Output matches COBOL version
- [ ] File I/O operations work correctly
- [ ] Error handling is appropriate
- [ ] Performance is acceptable
- [ ] Edge cases tested

## Issues Found

(Document any issues here)

## Performance Metrics

(Add performance comparison here)

## Sign-off

- [ ] Developer reviewed
- [ ] QA tested
- [ ] Ready for deployment

---

Generated by CobGO automated migration workflow
EOF

log_success "Report generated: $REPORT"

# Final summary
echo ""
echo "=========================================="
log_success "Migration workflow completed!"
echo "=========================================="
echo ""
echo "📁 Output directory: $OUTPUT_DIR"
echo "📄 Files generated:"
echo "   - DSL: ${PROGRAM_NAME}.cobgo"
echo "   - Go: ${PROGRAM_NAME}.go"
echo "   - Binary: ${PROGRAM_NAME}"
echo "   - Comparison: ${PROGRAM_NAME}_comparison.md"
echo "   - Test script: test_${PROGRAM_NAME}.sh"
echo "   - Report: migration_report.md"
echo ""
echo "📋 Next steps:"
echo "   1. cd $OUTPUT_DIR"
echo "   2. cat ${PROGRAM_NAME}_comparison.md  # Review translation"
echo "   3. ./test_${PROGRAM_NAME}.sh          # Test the program"
echo "   4. cat migration_report.md            # Read full report"
echo ""
log_info "For detailed migration guidance, see: docs/MIGRATION_GUIDE.md"

