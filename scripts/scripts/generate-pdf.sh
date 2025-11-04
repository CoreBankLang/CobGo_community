#!/bin/bash

# CobGO PDF Generation Script
# Generates PDF versions of documentation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if required tools are installed
check_dependencies() {
    print_status "Checking dependencies..."
    
    if ! command -v pandoc &> /dev/null; then
        print_error "pandoc is not installed. Please install it first:"
        echo "  - Ubuntu/Debian: sudo apt-get install pandoc"
        echo "  - macOS: brew install pandoc"
        echo "  - Windows: choco install pandoc"
        exit 1
    fi
    
    if ! command -v wkhtmltopdf &> /dev/null; then
        print_warning "wkhtmltopdf is not installed. Using pandoc default PDF engine."
        echo "  For better PDF formatting, install wkhtmltopdf:"
        echo "  - Ubuntu/Debian: sudo apt-get install wkhtmltopdf"
        echo "  - macOS: brew install wkhtmltopdf"
        echo "  - Windows: choco install wkhtmltopdf"
    fi
    
    print_status "Dependencies check completed."
}

# Create output directory
create_output_dir() {
    print_status "Creating output directory..."
    mkdir -p docs/pdf
    print_status "Output directory created: docs/pdf/"
}

# Generate PDF from markdown
generate_pdf() {
    local input_file="$1"
    local output_file="$2"
    local title="$3"
    
    print_status "Generating PDF: $output_file"
    
    if command -v wkhtmltopdf &> /dev/null; then
        # Use wkhtmltopdf for better formatting
        pandoc "$input_file" \
            --from markdown \
            --to html \
            --standalone \
            --css scripts/pdf-styles.css \
            --metadata title="$title" \
            --metadata date="$(date '+%B %d, %Y')" \
            --metadata author="CobGO Team" \
            --metadata subject="COBOL Modernization Platform" \
            --metadata keywords="COBOL,Go,DSL,Modernization,Migration" \
            --output - | \
        wkhtmltopdf \
            --page-size A4 \
            --margin-top 20mm \
            --margin-right 20mm \
            --margin-bottom 20mm \
            --margin-left 20mm \
            --encoding UTF-8 \
            --enable-local-file-access \
            --print-media-type \
            --footer-center "[page]" \
            --footer-font-size 10 \
            --footer-spacing 5 \
            - "$output_file"
    else
        # Use pandoc default PDF engine
        pandoc "$input_file" \
            --from markdown \
            --to pdf \
            --standalone \
            --metadata title="$title" \
            --metadata date="$(date '+%B %d, %Y')" \
            --metadata author="CobGO Team" \
            --metadata subject="COBOL Modernization Platform" \
            --metadata keywords="COBOL,Go,DSL,Modernization,Migration" \
            --pdf-engine=pdflatex \
            --output "$output_file"
    fi
    
    if [ $? -eq 0 ]; then
        print_status "✅ PDF generated successfully: $output_file"
    else
        print_error "❌ Failed to generate PDF: $output_file"
        exit 1
    fi
}

# Generate all PDFs
generate_all_pdfs() {
    print_status "Generating all PDF documentation..."
    
    # Migration Guide
    generate_pdf \
        "docs/migration.md" \
        "docs/pdf/CobGO-Migration-Guide.pdf" \
        "CobGO Migration Guide"
    
    # Governance Document
    generate_pdf \
        "docs/GOVERNANCE.md" \
        "docs/pdf/CobGO-Governance.pdf" \
        "CobGO Governance & Roadmap"
    
    # Roadmap
    generate_pdf \
        "docs/ROADMAP.md" \
        "docs/pdf/CobGO-Roadmap.pdf" \
        "CobGO Strategic Roadmap"
    
    # Contributing Guide
    generate_pdf \
        "CONTRIBUTING.md" \
        "docs/pdf/CobGO-Contributing-Guide.pdf" \
        "CobGO Contributing Guide"
    
    # Architecture Document
    if [ -f "docs/ARCHITECTURE.md" ]; then
        generate_pdf \
            "docs/ARCHITECTURE.md" \
            "docs/pdf/CobGO-Architecture.pdf" \
            "CobGO Architecture"
    fi
    
    # Language Specification
    if [ -f "docs/spec.md" ]; then
        generate_pdf \
            "docs/spec.md" \
            "docs/pdf/CobGO-Language-Specification.pdf" \
            "CobGO Language Specification"
    fi
    
    print_status "✅ All PDFs generated successfully!"
}

# Create PDF stylesheet
create_pdf_styles() {
    print_status "Creating PDF stylesheet..."
    
    cat > scripts/pdf-styles.css << 'EOF'
/* CobGO PDF Stylesheet */

body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    color: #333;
    max-width: 800px;
    margin: 0 auto;
    padding: 20px;
}

h1 {
    color: #2c3e50;
    border-bottom: 3px solid #3498db;
    padding-bottom: 10px;
    margin-top: 30px;
}

h2 {
    color: #34495e;
    border-bottom: 2px solid #ecf0f1;
    padding-bottom: 5px;
    margin-top: 25px;
}

h3 {
    color: #7f8c8d;
    margin-top: 20px;
}

h4 {
    color: #95a5a6;
    margin-top: 15px;
}

code {
    background-color: #f8f9fa;
    border: 1px solid #e9ecef;
    border-radius: 3px;
    padding: 2px 4px;
    font-family: 'Courier New', monospace;
    font-size: 0.9em;
}

pre {
    background-color: #f8f9fa;
    border: 1px solid #e9ecef;
    border-radius: 5px;
    padding: 15px;
    overflow-x: auto;
    font-family: 'Courier New', monospace;
    font-size: 0.9em;
}

pre code {
    background-color: transparent;
    border: none;
    padding: 0;
}

blockquote {
    border-left: 4px solid #3498db;
    margin: 0;
    padding-left: 20px;
    color: #7f8c8d;
    font-style: italic;
}

table {
    border-collapse: collapse;
    width: 100%;
    margin: 20px 0;
}

th, td {
    border: 1px solid #ddd;
    padding: 12px;
    text-align: left;
}

th {
    background-color: #f2f2f2;
    font-weight: bold;
}

tr:nth-child(even) {
    background-color: #f9f9f9;
}

ul, ol {
    margin: 15px 0;
    padding-left: 30px;
}

li {
    margin: 5px 0;
}

a {
    color: #3498db;
    text-decoration: none;
}

a:hover {
    text-decoration: underline;
}

hr {
    border: none;
    border-top: 2px solid #ecf0f1;
    margin: 30px 0;
}

/* Print-specific styles */
@media print {
    body {
        font-size: 12pt;
        line-height: 1.4;
    }
    
    h1 {
        page-break-before: always;
    }
    
    h1:first-child {
        page-break-before: avoid;
    }
    
    pre, blockquote {
        page-break-inside: avoid;
    }
    
    table {
        page-break-inside: avoid;
    }
}
EOF
    
    print_status "✅ PDF stylesheet created: scripts/pdf-styles.css"
}

# Main function
main() {
    print_status "Starting CobGO PDF generation..."
    
    check_dependencies
    create_output_dir
    create_pdf_styles
    generate_all_pdfs
    
    print_status "🎉 PDF generation completed successfully!"
    print_status "Generated PDFs are available in: docs/pdf/"
    
    # List generated files
    echo ""
    print_status "Generated files:"
    ls -la docs/pdf/*.pdf 2>/dev/null || print_warning "No PDF files found in docs/pdf/"
}

# Run main function
main "$@"
