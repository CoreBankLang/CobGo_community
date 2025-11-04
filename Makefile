# CobGO Makefile
# Production-grade build automation for COBOL modernization

.PHONY: help build test clean deps lint docs install tools

# Default target
help:
	@echo "CobGO - COBOL Modernization Language & Runtime"
	@echo ""
	@echo "Available targets:"
	@echo "  build     - Build all components"
	@echo "  tools     - Build all tools (dslc, dslfmt, dsllint, copybook2dsl)"
	@echo "  test      - Run all tests"
	@echo "  clean     - Clean build artifacts"
	@echo "  deps      - Install dependencies"
	@echo "  lint      - Run linters"
	@echo "  docs      - Generate documentation"
	@echo "  install   - Install dslc compiler"
	@echo "  examples  - Build and test examples"
	@echo "  format    - Format DSL files"
	@echo "  copybook  - Convert copybook examples"

# Build configuration
BINARY_NAME=dslc
BUILD_DIR=bin
VERSION=$(shell git describe --tags --always --dirty)
LDFLAGS=-ldflags "-X main.version=$(VERSION)"

# Build all components
build: deps
	@echo "Building CobGO compiler..."
	@mkdir -p $(BUILD_DIR)
	go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME) ./cmd/dslc
	@echo "Build complete: $(BUILD_DIR)/$(BINARY_NAME)"

# Build all tools
tools: deps
	@echo "Building CobGO tools..."
	@mkdir -p $(BUILD_DIR)
	go build $(LDFLAGS) -o $(BUILD_DIR)/dslc ./cmd/dslc
	go build $(LDFLAGS) -o $(BUILD_DIR)/dslfmt ./cmd/dslfmt
	go build $(LDFLAGS) -o $(BUILD_DIR)/dsllint ./cmd/dsllint
	go build $(LDFLAGS) -o $(BUILD_DIR)/copybook2dsl ./cmd/copybook2dsl
	@echo "All tools built successfully"

# Run tests
test:
	@echo "Running tests..."
	go test -v ./...

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	go clean

# Install dependencies
deps:
	@echo "Installing dependencies..."
	go mod download
	go mod tidy

# Run linters
lint:
	@echo "Running linters..."
	golangci-lint run

# Generate documentation
docs:
	@echo "Generating documentation..."
	@mkdir -p docs/api
	go doc -all ./... > docs/api/index.txt

# Install the compiler
install: build
	@echo "Installing $(BINARY_NAME)..."
	go install $(LDFLAGS) ./cmd/dslc

# Build and test examples
examples: build
	@echo "Building examples..."
	@mkdir -p examples/generated
	@for file in examples/*.cob; do \
		if [ -f "$$file" ]; then \
			echo "Compiling $$file..."; \
			./$(BUILD_DIR)/$(BINARY_NAME) "$$file" -o "examples/generated/$$(basename $$file .cob).go"; \
		fi; \
	done

# Development targets
dev-setup: deps
	@echo "Setting up development environment..."
	go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# CI targets
ci-test: deps test lint
	@echo "CI tests completed successfully"

# Format DSL files
format: tools
	@echo "Formatting DSL files..."
	@for file in examples/*.cobgo; do \
		if [ -f "$$file" ]; then \
			echo "Formatting $$file..."; \
			./$(BUILD_DIR)/dslfmt -i "$$file"; \
		fi; \
	done

# Convert copybook examples
copybook: tools
	@echo "Converting copybook examples..."
	@mkdir -p examples/generated
	@for file in examples/*.cpy; do \
		if [ -f "$$file" ]; then \
			echo "Converting $$file..."; \
			./$(BUILD_DIR)/copybook2dsl -i "$$file" -o "examples/generated/$$(basename $$file .cpy).cobgo"; \
		fi; \
	done

# Lint DSL files
lint-dsl: tools
	@echo "Linting DSL files..."
	@for file in examples/*.cobgo; do \
		if [ -f "$$file" ]; then \
			echo "Linting $$file..."; \
			./$(BUILD_DIR)/dsllint -i "$$file"; \
		fi; \
	done

# Run acceptance tests
acceptance-tests:
	@echo "Running acceptance tests..."
	go test ./tests/acceptance/... -v -timeout 10m

# Run performance benchmarks
benchmarks:
	@echo "Running performance benchmarks..."
	go test ./tests/acceptance/... -bench=. -benchmem -timeout 5m

# Run CI tests
ci-tests:
	@echo "Running CI tests..."
	go test ./tests/acceptance/... -v -run TestCI -timeout 5m

# Generate PDF documentation
docs-pdf:
	@echo "Generating PDF documentation..."
	@if command -v pwsh >/dev/null 2>&1; then \
		pwsh -File scripts/generate-pdf.ps1; \
	elif command -v powershell >/dev/null 2>&1; then \
		powershell -File scripts/generate-pdf.ps1; \
	else \
		echo "PowerShell not found, using bash script..."; \
		chmod +x scripts/generate-pdf.sh && ./scripts/generate-pdf.sh; \
	fi

# Validate documentation completeness
docs-validate:
	@echo "Validating documentation..."
	@if [ -f "docs/migration.md" ]; then echo "✅ Migration guide exists"; else echo "❌ Migration guide missing"; fi
	@if [ -f "docs/GOVERNANCE.md" ]; then echo "✅ Governance document exists"; else echo "❌ Governance document missing"; fi
	@if [ -f "docs/ROADMAP.md" ]; then echo "✅ Roadmap document exists"; else echo "❌ Roadmap document missing"; fi
	@if [ -f "CONTRIBUTING.md" ]; then echo "✅ Contributing guide exists"; else echo "❌ Contributing guide missing"; fi

# Complete documentation generation
docs-complete: docs-pdf docs-validate
	@echo "Documentation generation completed successfully"

# Compliance tests
compliance-tests:
	@echo "Running compliance tests..."
	go test ./tests/compliance/... -v -timeout 15m

# Build test runner
test-runner:
	@echo "Building test runner..."
	cd cmd/cobgotest && go build -o ../../bin/cobgotest

# Run compliance tests with test runner
run-compliance:
	@echo "Running compliance tests with cobgotest..."
	./bin/cobgotest -d tests/compliance -v

# Update golden files
update-golden:
	@echo "Updating golden output files..."
	go test ./tests/compliance/... -update-golden

# Epic 8: Migration tools
migration-tools:
	@echo "Building migration tools..."
	@mkdir -p $(BUILD_DIR)
	cd cmd/cobol2dsl && go build -o ../../bin/cobol2dsl
	@echo "Migration tools built successfully"

# Test migration
test-migration:
	@echo "Testing COBOL to DSL translation..."
	go test ./pkg/translator/... -v
	go test ./pkg/cobolparser/... -v

# Run migration example
migrate-example:
	@echo "Running migration example..."
	@if [ -f bin/cobol2dsl ]; then \
		bin/cobol2dsl -i examples/migration/hello_world.cob --mixed --output-dir examples/migration/output -v; \
	else \
		echo "Error: cobol2dsl not built. Run 'make migration-tools' first"; \
		exit 1; \
	fi

# Automated migration workflow
migrate:
	@echo "Running automated migration workflow..."
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required"; \
		echo "Usage: make migrate FILE=program.cob"; \
		exit 1; \
	fi
	@chmod +x scripts/migrate.sh
	@scripts/migrate.sh $(FILE) $(DIR)

# Release preparation
release-check: clean deps test lint build tools migration-tools examples format copybook lint-dsl acceptance-tests compliance-tests benchmarks docs-complete
	@echo "Release check completed successfully"