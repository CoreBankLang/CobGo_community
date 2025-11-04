# CobGO Build Script for Windows PowerShell
# Alternative to Makefile for Windows users

param(
    [string]$Target = "help",
    [string]$Output = "bin/dslc.exe",
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

# Configuration
$BinaryName = "dslc"
$BuildDir = "bin"
$Version = "dev"

function Write-Info {
    param([string]$Message)
    Write-Host "INFO: $Message" -ForegroundColor Green
}

function Write-Error {
    param([string]$Message)
    Write-Host "ERROR: $Message" -ForegroundColor Red
}

function Show-Help {
    Write-Host "CobGO Build Script for Windows" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Available targets:"
    Write-Host "  build     - Build all components"
    Write-Host "  test      - Run all tests"
    Write-Host "  clean     - Clean build artifacts"
    Write-Host "  deps      - Install dependencies"
    Write-Host "  lint      - Run linters (requires golangci-lint)"
    Write-Host "  docs      - Generate documentation"
    Write-Host "  install   - Install dslc compiler"
    Write-Host "  examples  - Build and test examples"
    Write-Host "  verify    - Verify project structure"
    Write-Host ""
    Write-Host "Usage: .\build.ps1 -Target <target>"
    Write-Host "Example: .\build.ps1 -Target build"
}

function Test-GoInstalled {
    try {
        $goVersion = go version 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Info "Go is installed: $goVersion"
            return $true
        }
    }
    catch {
        Write-Error "Go is not installed or not in PATH"
        Write-Error "Please install Go from https://golang.org/dl/"
        return $false
    }
    return $false
}

function Invoke-Build {
    Write-Info "Building CobGO compiler..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    # Create build directory
    if (-not (Test-Path $BuildDir)) {
        New-Item -ItemType Directory -Path $BuildDir -Force | Out-Null
    }
    
    # Build the compiler
    $ldflags = "-ldflags `"-X main.version=$Version`""
    $buildCmd = "go build $ldflags -o $BuildDir/$BinaryName.exe ./cmd/dslc"
    
    if ($Verbose) {
        Write-Info "Running: $buildCmd"
    }
    
    Invoke-Expression $buildCmd
    
    if ($LASTEXITCODE -eq 0) {
        Write-Info "Build successful: $BuildDir/$BinaryName.exe"
    } else {
        Write-Error "Build failed"
    }
}

function Invoke-Test {
    Write-Info "Running tests..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    go test -v ./...
    
    if ($LASTEXITCODE -eq 0) {
        Write-Info "Tests completed successfully"
    } else {
        Write-Error "Tests failed"
    }
}

function Invoke-Clean {
    Write-Info "Cleaning build artifacts..."
    
    if (Test-Path $BuildDir) {
        Remove-Item -Path $BuildDir -Recurse -Force
        Write-Info "Removed $BuildDir directory"
    }
    
    go clean
    
    Write-Info "Clean completed"
}

function Invoke-Deps {
    Write-Info "Installing dependencies..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    go mod download
    go mod tidy
    
    Write-Info "Dependencies installed"
}

function Invoke-Lint {
    Write-Info "Running linters..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    # Check if golangci-lint is installed
    try {
        golangci-lint version 2>$null | Out-Null
        if ($LASTEXITCODE -ne 0) {
            Write-Info "Installing golangci-lint..."
            go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
        }
    }
    catch {
        Write-Info "Installing golangci-lint..."
        go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
    }
    
    golangci-lint run
    
    if ($LASTEXITCODE -eq 0) {
        Write-Info "Linting completed successfully"
    } else {
        Write-Error "Linting failed"
    }
}

function Invoke-Docs {
    Write-Info "Generating documentation..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    if (-not (Test-Path "docs/api")) {
        New-Item -ItemType Directory -Path "docs/api" -Force | Out-Null
    }
    
    go doc -all ./... > docs/api/index.txt
    
    Write-Info "Documentation generated in docs/api/"
}

function Invoke-Install {
    Write-Info "Installing $BinaryName..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    $ldflags = "-ldflags `"-X main.version=$Version`""
    go install $ldflags ./cmd/dslc
    
    if ($LASTEXITCODE -eq 0) {
        Write-Info "Installation successful"
    } else {
        Write-Error "Installation failed"
    }
}

function Invoke-Examples {
    Write-Info "Building examples..."
    
    if (-not (Test-GoInstalled)) {
        return
    }
    
    # Build the compiler first
    Invoke-Build
    
    if (-not (Test-Path "examples/generated")) {
        New-Item -ItemType Directory -Path "examples/generated" -Force | Out-Null
    }
    
    # Compile example COBOL files
    $cobFiles = Get-ChildItem -Path "examples" -Filter "*.cob"
    foreach ($file in $cobFiles) {
        $outputFile = "examples/generated/$($file.BaseName).go"
        Write-Info "Compiling $($file.Name) -> $outputFile"
        
        & "./$BuildDir/$BinaryName.exe" -i $file.FullName -o $outputFile
        
        if ($LASTEXITCODE -eq 0) {
            Write-Info "Generated: $outputFile"
        } else {
            Write-Error "Failed to compile: $($file.Name)"
        }
    }
}

function Invoke-Verify {
    Write-Info "Verifying project structure..."
    
    $requiredDirs = @(
        "cmd/dslc",
        "pkg/parser",
        "pkg/ir", 
        "pkg/codegen",
        "runtime",
        "docs",
        "examples",
        ".github/workflows"
    )
    
    $requiredFiles = @(
        "go.mod",
        "README.md",
        "Makefile",
        "cmd/dslc/main.go",
        "pkg/parser/parser.go",
        "pkg/ir/ir.go",
        "pkg/codegen/codegen.go",
        "runtime/runtime.go"
    )
    
    $allGood = $true
    
    foreach ($dir in $requiredDirs) {
        if (Test-Path $dir) {
            Write-Info "Directory exists: $dir"
        } else {
            Write-Error "Missing directory: $dir"
            $allGood = $false
        }
    }
    
    foreach ($file in $requiredFiles) {
        if (Test-Path $file) {
            Write-Info "File exists: $file"
        } else {
            Write-Error "Missing file: $file"
            $allGood = $false
        }
    }
    
    if ($allGood) {
        Write-Info "Project structure verification passed"
    } else {
        Write-Error "Project structure verification failed"
    }
}

# Main execution
switch ($Target.ToLower()) {
    "help" { Show-Help }
    "build" { Invoke-Build }
    "test" { Invoke-Test }
    "clean" { Invoke-Clean }
    "deps" { Invoke-Deps }
    "lint" { Invoke-Lint }
    "docs" { Invoke-Docs }
    "install" { Invoke-Install }
    "examples" { Invoke-Examples }
    "verify" { Invoke-Verify }
    default {
        Write-Error "Unknown target: $Target"
        Show-Help
    }
}