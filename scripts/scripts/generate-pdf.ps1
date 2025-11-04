# CobGO PDF Generation Script (PowerShell)
# Generates PDF versions of documentation

param(
    [switch]$Force,
    [string]$OutputDir = "docs/pdf"
)

# Function to print colored output
function Write-Status {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor Green
}

function Write-Warning {
    param([string]$Message)
    Write-Host "[WARNING] $Message" -ForegroundColor Yellow
}

function Write-Error {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

# Check if required tools are installed
function Test-Dependencies {
    Write-Status "Checking dependencies..."
    
    if (-not (Get-Command pandoc -ErrorAction SilentlyContinue)) {
        Write-Error "pandoc is not installed. Please install it first:"
        Write-Host "  - Download from: https://pandoc.org/installing.html"
        Write-Host "  - Or use chocolatey: choco install pandoc"
        exit 1
    }
    
    if (-not (Get-Command wkhtmltopdf -ErrorAction SilentlyContinue)) {
        Write-Warning "wkhtmltopdf is not installed. Using pandoc default PDF engine."
        Write-Host "  For better PDF formatting, install wkhtmltopdf:"
        Write-Host "  - Download from: https://wkhtmltopdf.org/downloads.html"
        Write-Host "  - Or use chocolatey: choco install wkhtmltopdf"
    }
    
    Write-Status "Dependencies check completed."
}

# Create output directory
function New-OutputDirectory {
    param([string]$Path)
    
    Write-Status "Creating output directory..."
    if (-not (Test-Path $Path)) {
        New-Item -ItemType Directory -Path $Path -Force | Out-Null
    }
    Write-Status "Output directory created: $Path/"
}

# Generate PDF from markdown
function New-PDF {
    param(
        [string]$InputFile,
        [string]$OutputFile,
        [string]$Title
    )
    
    Write-Status "Generating PDF: $OutputFile"
    
    $metadata = @{
        title = $Title
        date = (Get-Date -Format "MMMM dd, yyyy")
        author = "CobGO Team"
        subject = "COBOL Modernization Platform"
        keywords = "COBOL,Go,DSL,Modernization,Migration"
    }
    
    try {
        if (Get-Command wkhtmltopdf -ErrorAction SilentlyContinue) {
            # Use wkhtmltopdf for better formatting
            $htmlFile = [System.IO.Path]::ChangeExtension($OutputFile, ".html")
            
            pandoc $InputFile `
                --from markdown `
                --to html `
                --standalone `
                --css "scripts/pdf-styles.css" `
                --metadata "title=$($metadata.title)" `
                --metadata "date=$($metadata.date)" `
                --metadata "author=$($metadata.author)" `
                --metadata "subject=$($metadata.subject)" `
                --metadata "keywords=$($metadata.keywords)" `
                --output $htmlFile
            
            if ($LASTEXITCODE -eq 0) {
                wkhtmltopdf `
                    --page-size A4 `
                    --margin-top 20mm `
                    --margin-right 20mm `
                    --margin-bottom 20mm `
                    --margin-left 20mm `
                    --encoding UTF-8 `
                    --enable-local-file-access `
                    --print-media-type `
                    --footer-center "[page]" `
                    --footer-font-size 10 `
                    --footer-spacing 5 `
                    $htmlFile $OutputFile
                
                # Clean up temporary HTML file
                Remove-Item $htmlFile -ErrorAction SilentlyContinue
            }
        } else {
            # Use pandoc default PDF engine
            pandoc $InputFile `
                --from markdown `
                --to pdf `
                --standalone `
                --metadata "title=$($metadata.title)" `
                --metadata "date=$($metadata.date)" `
                --metadata "author=$($metadata.author)" `
                --metadata "subject=$($metadata.subject)" `
                --metadata "keywords=$($metadata.keywords)" `
                --pdf-engine=pdflatex `
                --output $OutputFile
        }
        
        if ($LASTEXITCODE -eq 0) {
            Write-Status "✅ PDF generated successfully: $OutputFile"
        } else {
            Write-Error "❌ Failed to generate PDF: $OutputFile"
            exit 1
        }
    }
    catch {
        Write-Error "❌ Error generating PDF: $($_.Exception.Message)"
        exit 1
    }
}

# Generate all PDFs
function New-AllPDFs {
    param([string]$OutputDir)
    
    Write-Status "Generating all PDF documentation..."
    
    # Migration Guide
    if (Test-Path "docs/migration.md") {
        New-PDF `
            -InputFile "docs/migration.md" `
            -OutputFile "$OutputDir/CobGO-Migration-Guide.pdf" `
            -Title "CobGO Migration Guide"
    }
    
    # Governance Document
    if (Test-Path "docs/GOVERNANCE.md") {
        New-PDF `
            -InputFile "docs/GOVERNANCE.md" `
            -OutputFile "$OutputDir/CobGO-Governance.pdf" `
            -Title "CobGO Governance & Roadmap"
    }
    
    # Roadmap
    if (Test-Path "docs/ROADMAP.md") {
        New-PDF `
            -InputFile "docs/ROADMAP.md" `
            -OutputFile "$OutputDir/CobGO-Roadmap.pdf" `
            -Title "CobGO Strategic Roadmap"
    }
    
    # Contributing Guide
    if (Test-Path "CONTRIBUTING.md") {
        New-PDF `
            -InputFile "CONTRIBUTING.md" `
            -OutputFile "$OutputDir/CobGO-Contributing-Guide.pdf" `
            -Title "CobGO Contributing Guide"
    }
    
    # Architecture Document
    if (Test-Path "docs/ARCHITECTURE.md") {
        New-PDF `
            -InputFile "docs/ARCHITECTURE.md" `
            -OutputFile "$OutputDir/CobGO-Architecture.pdf" `
            -Title "CobGO Architecture"
    }
    
    # Language Specification
    if (Test-Path "docs/spec.md") {
        New-PDF `
            -InputFile "docs/spec.md" `
            -OutputFile "$OutputDir/CobGO-Language-Specification.pdf" `
            -Title "CobGO Language Specification"
    }
    
    Write-Status "✅ All PDFs generated successfully!"
}

# Create PDF stylesheet
function New-PDFStyles {
    Write-Status "Creating PDF stylesheet..."
    
    $cssContent = @'
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
'@
    
    $cssContent | Out-File -FilePath "scripts/pdf-styles.css" -Encoding UTF8
    Write-Status "✅ PDF stylesheet created: scripts/pdf-styles.css"
}

# Main function
function Main {
    Write-Status "Starting CobGO PDF generation..."
    
    Test-Dependencies
    New-OutputDirectory -Path $OutputDir
    New-PDFStyles
    New-AllPDFs -OutputDir $OutputDir
    
    Write-Status "🎉 PDF generation completed successfully!"
    Write-Status "Generated PDFs are available in: $OutputDir/"
    
    # List generated files
    Write-Host ""
    Write-Status "Generated files:"
    Get-ChildItem "$OutputDir/*.pdf" -ErrorAction SilentlyContinue | ForEach-Object {
        Write-Host "  - $($_.Name) ($([math]::Round($_.Length / 1KB, 2)) KB)"
    }
}

# Run main function
Main
