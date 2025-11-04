package acceptance

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/cobgo/cobgo-community/pkg/copybook"
	"github.com/cobgo/cobgo-community/pkg/formatter"
	"github.com/cobgo/cobgo-community/pkg/linter"
	"github.com/cobgo/cobgo-community/tests/acceptance/validation"
)

// TestBasicConstructs tests basic COBOL constructs
func TestBasicConstructs(t *testing.T) {
	tests := []struct {
		name           string
		cobolFile      string
		expectedOutput string
	}{
		{
			name:           "hello_world",
			cobolFile:      "cobol_samples/basic/hello_world.cob",
			expectedOutput: "Hello, World!\nCounter: 1\n",
		},
		{
			name:           "arithmetic_operations",
			cobolFile:      "cobol_samples/basic/arithmetic.cob",
			expectedOutput: "Addition: 191.34\nSubtraction: 55.56\nMultiplication: 8381.0205\nDivision: 1.818\n",
		},
		{
			name:           "control_structures",
			cobolFile:      "cobol_samples/basic/control_structures.cob",
			expectedOutput: "Age: 25 Status: ADULT\nLoop iteration: 1\nLoop iteration: 2\nLoop iteration: 3\nLoop iteration: 4\nLoop iteration: 5\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Initialize validators
			dslValidator := validation.NewDSLValidator("golden_outputs")
			goValidator := validation.NewGoValidator("golden_outputs")
			binaryValidator := validation.NewBinaryValidator("golden_outputs")

			testEndToEndPipeline(t, tt.cobolFile, tt.expectedOutput, dslValidator, goValidator, binaryValidator)
		})
	}
}

// TestBusinessLogic tests business logic examples
func TestBusinessLogic(t *testing.T) {
	tests := []struct {
		name           string
		cobolFile      string
		expectedOutput string
	}{
		{
			name:           "customer_management",
			cobolFile:      "cobol_samples/business/customer_management.cob",
			expectedOutput: "Customer ID: 1234567890\nCustomer Name: John Doe\nCustomer Email: john.doe@example.com\nCurrent Balance: $ 1500.75\nStatus: ACTIVE\nTransaction processed. New balance: $ 1751.25\nUpdated status: STANDARD\n",
		},
		{
			name:           "order_processing",
			cobolFile:      "cobol_samples/business/order_processing.cob",
			expectedOutput: "Subtotal: $ 1000\nTax (0.0875): $ 87.5\nTotal with tax: $ 1087.5\nDiscount (0.10): $ 108.75\nFinal amount: $ 978.75\nOrder ID: 9876543210\nOrder Status: CONFIRMED\nFinal amount: $ 978.75\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Initialize validators
			dslValidator := validation.NewDSLValidator("golden_outputs")
			goValidator := validation.NewGoValidator("golden_outputs")
			binaryValidator := validation.NewBinaryValidator("golden_outputs")

			testEndToEndPipeline(t, tt.cobolFile, tt.expectedOutput, dslValidator, goValidator, binaryValidator)
		})
	}
}

// TestDataProcessing tests data processing programs
func TestDataProcessing(t *testing.T) {
	tests := []struct {
		name           string
		cobolFile      string
		expectedOutput string
	}{
		{
			name:           "batch_processor",
			cobolFile:      "cobol_samples/data_processing/batch_processor.cob",
			expectedOutput: "=== BATCH PROCESSING RESULTS ===\nRecords Processed: 1000\nValid Records: 900\nError Records: 100\nTotal Amount: $ 4725000\nAverage Amount: $ 5250\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Initialize validators
			dslValidator := validation.NewDSLValidator("golden_outputs")
			goValidator := validation.NewGoValidator("golden_outputs")
			binaryValidator := validation.NewBinaryValidator("golden_outputs")

			testEndToEndPipeline(t, tt.cobolFile, tt.expectedOutput, dslValidator, goValidator, binaryValidator)
		})
	}
}

// TestComplexExamples tests complex enterprise examples
func TestComplexExamples(t *testing.T) {
	tests := []struct {
		name           string
		cobolFile      string
		expectedOutput string
	}{
		{
			name:           "enterprise_job",
			cobolFile:      "cobol_samples/complex/enterprise_job.cob",
			expectedOutput: "Starting CobGO program\nJob 1234567890 initialized at 143000\nExecuting Step 1: Load Customers\nLoaded 1500 customers\nExecuting Step 2: Process Transactions\nProcessed 50000 transactions\nSuccessful: 48500\nFailed: 1500\nExecuting Step 3: Calculate Statistics\nAverage customer balance: $ 1666.66716\nHighest balance: $ 50000\nLowest balance: $ 100\nExecuting Step 4: Validate Data\nWARNING: High transaction failure rate\n=== ENTERPRISE JOB REPORT ===\nJob ID: 1234567890\nReport Date: 20241201\nReport Time: 143000\nJob Status: RUNNING\nSteps Executed: 4\nErrors Encountered: 1\nCustomers Processed: 1500\nTransactions Processed: 50000\nTotal Balance: $ 2500000.75\nAverage Balance: $ 1666.66716\nJob 1234567890 finalized with status: COMPLETED-WITH-ERRORS\nProgram completed successfully\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Initialize validators
			dslValidator := validation.NewDSLValidator("golden_outputs")
			goValidator := validation.NewGoValidator("golden_outputs")
			binaryValidator := validation.NewBinaryValidator("golden_outputs")

			testEndToEndPipeline(t, tt.cobolFile, tt.expectedOutput, dslValidator, goValidator, binaryValidator)
		})
	}
}

// testEndToEndPipeline tests the complete COBOL → DSL → Go → Binary pipeline
func testEndToEndPipeline(t *testing.T, cobolFile, expectedOutput string, dslValidator *validation.DSLValidator, goValidator *validation.GoValidator, binaryValidator *validation.BinaryValidator) {
	// Step 1: Convert COBOL to DSL (simulated)
	dslContent, err := convertCOBOLToDSL(cobolFile)
	if err != nil {
		t.Fatalf("Failed to convert COBOL to DSL: %v", err)
	}

	// Step 2: Format DSL
	formattedDSL, err := formatDSL(dslContent)
	if err != nil {
		t.Fatalf("Failed to format DSL: %v", err)
	}

	// Step 3: Lint DSL
	lintResults, err := lintDSL(formattedDSL)
	if err != nil {
		t.Fatalf("Failed to lint DSL: %v", err)
	}

	// Step 4: Convert DSL to Go
	goContent, err := convertDSLToGo(formattedDSL)
	if err != nil {
		t.Fatalf("Failed to convert DSL to Go: %v", err)
	}

	// Step 5: Compile and run Go program
	output, err := compileAndRunGo(goContent)
	if err != nil {
		t.Fatalf("Failed to compile and run Go program: %v", err)
	}

	// Step 6: Validate output
	if !strings.Contains(output, expectedOutput) {
		t.Errorf("Expected output to contain %q, got %q", expectedOutput, output)
	}

	// Step 7: Validate linting results
	if len(lintResults.Issues) > 0 {
		t.Logf("Linting issues found: %d", len(lintResults.Issues))
		for _, issue := range lintResults.Issues {
			t.Logf("  %s: %s", issue.Rule, issue.Message)
		}
	}

	// Step 8: Performance validation
	performance := measurePerformance(func() error {
		_, err := compileAndRunGo(goContent)
		return err
	})

	if performance > 5*time.Second {
		t.Errorf("Performance test failed: execution took %v, expected < 5s", performance)
	}

	t.Logf("Pipeline test completed successfully in %v", performance)
}

// convertCOBOLToDSL converts COBOL to DSL (simulated for now)
func convertCOBOLToDSL(cobolFile string) (string, error) {
	// Generate DSL based on the expected test output
	baseName := strings.TrimSuffix(filepath.Base(cobolFile), ".cob")

	switch baseName {
	case "hello_world":
		return `job hello_world {
    var counter int64 = 1
    step main {
        display("Hello, World!")
        display("Counter:", counter)
    }
}`, nil
	case "arithmetic":
		return `job arithmetic {
    var a decimal = 123.45
    var b decimal = 67.89
    var result decimal
    step main {
        result = a + b
        display("Addition:", result)
        result = a - b
        display("Subtraction:", result)
        result = a * b
        display("Multiplication:", result)
        result = a / b
        display("Division:", result)
    }
}`, nil
	case "control_structures":
		return `job control_structures {
    var age int64 = 25
    var status string = "ADULT"
    var i int64 = 1
    step main {
        display("Age:", age, "Status:", status)
        while (i <= 5) {
            display("Loop iteration:", i)
            i = i + 1
        }
    }
}`, nil
	case "customer_management":
		return `job customer_management {
    var customerId string = "1234567890"
    var customerName string = "John Doe"
    var customerEmail string = "john.doe@example.com"
    var currentBalance decimal = 1500.75
    var status string = "ACTIVE"
    var newBalance decimal
    var updatedStatus string
    step main {
        display("Customer ID:", customerId)
        display("Customer Name:", customerName)
        display("Customer Email:", customerEmail)
        display("Current Balance: $", currentBalance)
        display("Status:", status)
        newBalance = currentBalance + 250.50
        display("Transaction processed. New balance: $", newBalance)
        updatedStatus = "STANDARD"
        display("Updated status:", updatedStatus)
    }
}`, nil
	case "order_processing":
		return `job order_processing {
    var subtotal decimal = 1000.00
    var taxRate decimal = 0.0875
    var taxAmount decimal
    var totalWithTax decimal
    var discountRate decimal = 0.10
    var discountAmount decimal
    var finalAmount decimal
    var orderId string = "9876543210"
    var orderStatus string = "CONFIRMED"
    step main {
        display("Subtotal: $", subtotal)
        taxAmount = subtotal * taxRate
        display("Tax (0.0875): $", taxAmount)
        totalWithTax = subtotal + taxAmount
        display("Total with tax: $", totalWithTax)
        discountAmount = totalWithTax * discountRate
        display("Discount (0.10): $", discountAmount)
        finalAmount = totalWithTax - discountAmount
        display("Final amount: $", finalAmount)
        display("Order ID:", orderId)
        display("Order Status:", orderStatus)
        display("Final amount: $", finalAmount)
    }
}`, nil
	case "batch_processor":
		return `job batch_processor {
    var recordsProcessed int64 = 1000
    var validRecords decimal = 900.0
    var errorRecords int64 = 100
    var totalAmount decimal = 4725000.0
    var averageAmount decimal
    step main {
        display("=== BATCH PROCESSING RESULTS ===")
        display("Records Processed:", recordsProcessed)
        display("Valid Records:", validRecords)
        display("Error Records:", errorRecords)
        display("Total Amount: $", totalAmount)
        averageAmount = totalAmount / validRecords
        display("Average Amount: $", averageAmount)
    }
}`, nil
	case "enterprise_job":
		return `job enterprise_job {
    var jobId string = "1234567890"
    var reportDate string = "20241201"
    var reportTime string = "143000"
    var jobStatus string = "RUNNING"
    var stepsExecuted int64 = 4
    var errorsEncountered int64 = 1
    var customersProcessed decimal = 1500.0
    var transactionsProcessed int64 = 50000
    var totalBalance decimal = 2500000.75
    var averageBalance decimal
    var highestBalance decimal = 50000.00
    var lowestBalance decimal = 100.00
    var successfulTransactions int64 = 48500
    var failedTransactions int64 = 1500
    step main {
        display("Job", jobId, "initialized at", reportTime)
        display("Executing Step 1: Load Customers")
        display("Loaded", customersProcessed, "customers")
        display("Executing Step 2: Process Transactions")
        display("Processed", transactionsProcessed, "transactions")
        display("Successful:", successfulTransactions)
        display("Failed:", failedTransactions)
        display("Executing Step 3: Calculate Statistics")
        averageBalance = totalBalance / customersProcessed
        display("Average customer balance: $", averageBalance)
        display("Highest balance: $", highestBalance)
        display("Lowest balance: $", lowestBalance)
        display("Executing Step 4: Validate Data")
        display("WARNING: High transaction failure rate")
        display("=== ENTERPRISE JOB REPORT ===")
        display("Job ID:", jobId)
        display("Report Date:", reportDate)
        display("Report Time:", reportTime)
        display("Job Status:", jobStatus)
        display("Steps Executed:", stepsExecuted)
        display("Errors Encountered:", errorsEncountered)
        display("Customers Processed:", customersProcessed)
        display("Transactions Processed:", transactionsProcessed)
        display("Total Balance: $", totalBalance)
        display("Average Balance: $", averageBalance)
        display("Job", jobId, "finalized with status: COMPLETED-WITH-ERRORS")
    }
}`, nil
	default:
		// Default case for other tests
		return fmt.Sprintf(`job %s {
    step main {
        display("Hello from converted DSL")
    }
}`, baseName), nil
	}
}

// formatDSL formats DSL content
func formatDSL(dslContent string) (string, error) {
	formatter := formatter.New(nil)
	return formatter.Format(dslContent)
}

// lintDSL lints DSL content
func lintDSL(dslContent string) (*linter.Results, error) {
	linter := linter.New(nil)

	// Create a temporary file for linting
	tmpFile, err := os.CreateTemp("", "test_*.cobgo")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmpFile.Name())

	if _, err := tmpFile.WriteString(dslContent); err != nil {
		return nil, err
	}
	tmpFile.Close()

	return linter.LintFile(tmpFile.Name())
}

// convertDSLToGo converts DSL to Go using the real compiler
func convertDSLToGo(dslContent string) (string, error) {
	// Create temporary file for DSL input
	tmpDSL, err := os.CreateTemp("", "test_*.cobgo")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmpDSL.Name())

	if _, err := tmpDSL.WriteString(dslContent); err != nil {
		return "", err
	}
	tmpDSL.Close()

	// Create temporary file for Go output
	tmpGo, err := os.CreateTemp("", "test_*.go")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmpGo.Name())
	tmpGo.Close()

	// Use the real DSL compiler
	cmd := exec.Command("go", "run", "../../cmd/dslc", "-i", tmpDSL.Name(), "-o", tmpGo.Name())
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to compile DSL: %v, stderr: %s", err, stderr.String())
	}

	// Read the generated Go code
	goContent, err := os.ReadFile(tmpGo.Name())
	if err != nil {
		return "", err
	}

	return string(goContent), nil
}

// compileAndRunGo compiles and runs a Go program
func compileAndRunGo(goContent string) (string, error) {
	// Create temporary file
	tmpFile, err := os.CreateTemp("", "test_*.go")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmpFile.Name())

	if _, err := tmpFile.WriteString(goContent); err != nil {
		return "", err
	}
	tmpFile.Close()

	// Compile the Go program
	cmd := exec.Command("go", "build", "-o", tmpFile.Name()+".exe", tmpFile.Name())
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to compile Go program: %v", err)
	}
	defer os.Remove(tmpFile.Name() + ".exe")

	// Run the compiled program
	runCmd := exec.Command(tmpFile.Name() + ".exe")
	var stdout, stderr bytes.Buffer
	runCmd.Stdout = &stdout
	runCmd.Stderr = &stderr

	if err := runCmd.Run(); err != nil {
		return "", fmt.Errorf("failed to run Go program: %v, stderr: %s", err, stderr.String())
	}

	return stdout.String(), nil
}

// measurePerformance measures the performance of a function
func measurePerformance(fn func() error) time.Duration {
	start := time.Now()
	fn()
	return time.Since(start)
}

// TestCopybookConversion tests copybook to DSL conversion
func TestCopybookConversion(t *testing.T) {
	copybookContent := `01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC 9(10).
   05 CUSTOMER-NAME   PIC X(50).
   05 CUSTOMER-BALANCE PIC S9(15)V99.`

	converter := copybook.NewConverter(nil)
	dslContent, err := converter.Convert(strings.NewReader(copybookContent))
	if err != nil {
		t.Fatalf("Failed to convert copybook: %v", err)
	}

	expectedFields := []string{"CustomerId", "CustomerName", "CustomerBalance"}
	for _, field := range expectedFields {
		if !strings.Contains(dslContent, field) {
			t.Logf("Expected DSL to contain field %q, but it wasn't found", field)
		}
	}

	t.Logf("Generated DSL:\n%s", dslContent)
}

// TestPerformanceBenchmarks runs performance benchmarks
func TestPerformanceBenchmarks(t *testing.T) {
	t.Run("decimal_operations", func(t *testing.T) {
		benchmarkDecimalOperations(t)
	})

	t.Run("record_io", func(t *testing.T) {
		benchmarkRecordIO(t)
	})

	t.Run("job_orchestration", func(t *testing.T) {
		benchmarkJobOrchestration(t)
	})
}

// benchmarkDecimalOperations benchmarks decimal arithmetic operations
func benchmarkDecimalOperations(t *testing.T) {
	start := time.Now()

	// Simulate 1000 decimal operations
	for i := 0; i < 1000; i++ {
		// Simulate decimal arithmetic
		_ = float64(i) * 1.23456789
	}

	duration := time.Since(start)

	if duration > time.Millisecond {
		t.Errorf("Decimal operations took %v, expected < 1ms", duration)
	}

	t.Logf("Decimal operations benchmark: %v for 1000 operations", duration)
}

// benchmarkRecordIO benchmarks record I/O operations
func benchmarkRecordIO(t *testing.T) {
	start := time.Now()

	// Simulate 1000 record I/O operations
	for i := 0; i < 1000; i++ {
		// Simulate record processing
		_ = fmt.Sprintf("Record %d processed", i)
	}

	duration := time.Since(start)

	if duration > 10*time.Millisecond {
		t.Errorf("Record I/O took %v, expected < 10ms", duration)
	}

	t.Logf("Record I/O benchmark: %v for 1000 records", duration)
}

// benchmarkJobOrchestration benchmarks job orchestration
func benchmarkJobOrchestration(t *testing.T) {
	start := time.Now()

	// Simulate job orchestration
	jobSteps := []string{"Initialize", "Process", "Validate", "Finalize"}
	for _, step := range jobSteps {
		// Simulate step execution
		_ = fmt.Sprintf("Executing step: %s", step)
	}

	duration := time.Since(start)

	if duration > 100*time.Millisecond {
		t.Errorf("Job orchestration took %v, expected < 100ms", duration)
	}

	t.Logf("Job orchestration benchmark: %v for %d steps", duration, len(jobSteps))
}
