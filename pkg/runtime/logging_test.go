package runtime

import (
	"bytes"
	"fmt"
	"os"
	"strings"
	"testing"
	"time"
)

func TestLogger(t *testing.T) {
	// Create a buffer to capture log output
	var buf bytes.Buffer

	// Create logger
	logger := NewLogger(LogLevelDebug, &buf)

	// Test different log levels
	t.Run("LogLevels", func(t *testing.T) {
		logger.Debug("Debug message")
		logger.Info("Info message")
		logger.Warn("Warning message")
		logger.Error("Error message")

		output := buf.String()

		// Check that all messages were logged
		if !strings.Contains(output, "DEBUG") {
			t.Error("Expected DEBUG message to be logged")
		}
		if !strings.Contains(output, "INFO") {
			t.Error("Expected INFO message to be logged")
		}
		if !strings.Contains(output, "WARN") {
			t.Error("Expected WARN message to be logged")
		}
		if !strings.Contains(output, "ERROR") {
			t.Error("Expected ERROR message to be logged")
		}
	})

	// Test log level filtering
	t.Run("LogLevelFiltering", func(t *testing.T) {
		buf.Reset()
		logger.SetLevel(LogLevelWarn)

		logger.Debug("Debug message")
		logger.Info("Info message")
		logger.Warn("Warning message")
		logger.Error("Error message")

		output := buf.String()

		// Check that only WARN and ERROR messages were logged
		if strings.Contains(output, "DEBUG") {
			t.Error("Expected DEBUG message to be filtered out")
		}
		if strings.Contains(output, "INFO") {
			t.Error("Expected INFO message to be filtered out")
		}
		if !strings.Contains(output, "WARN") {
			t.Error("Expected WARN message to be logged")
		}
		if !strings.Contains(output, "ERROR") {
			t.Error("Expected ERROR message to be logged")
		}
	})

	// Test structured logging with fields
	t.Run("StructuredLogging", func(t *testing.T) {
		buf.Reset()
		logger.SetLevel(LogLevelDebug)

		structuredLogger := logger.WithField("user_id", "12345")
		structuredLogger.Info("User logged in")

		output := buf.String()

		if !strings.Contains(output, "user_id=12345") {
			t.Error("Expected user_id field to be included in log")
		}
	})

	// Test multiple fields
	t.Run("MultipleFields", func(t *testing.T) {
		buf.Reset()

		fields := map[string]interface{}{
			"user_id":    "12345",
			"session_id": "abc123",
			"action":     "login",
		}

		structuredLogger := logger.WithFields(fields)
		structuredLogger.Info("User performed action")

		output := buf.String()

		for key, value := range fields {
			expected := fmt.Sprintf("%s=%v", key, value)
			if !strings.Contains(output, expected) {
				t.Errorf("Expected field %s to be included in log", expected)
			}
		}
	})

	// Test message formatting
	t.Run("MessageFormatting", func(t *testing.T) {
		buf.Reset()

		logger.Info("User %s performed %d actions", "john", 5)

		output := buf.String()

		if !strings.Contains(output, "User john performed 5 actions") {
			t.Error("Expected formatted message to be logged")
		}
	})
}

func TestFileLogger(t *testing.T) {
	// Create a temporary log file
	tmpFile, err := os.CreateTemp("", "test_log_*.log")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpFile.Name())
	tmpFile.Close()

	// Create file logger
	logger, err := NewFileLogger(LogLevelInfo, tmpFile.Name())
	if err != nil {
		t.Fatalf("Failed to create file logger: %v", err)
	}

	// Log some messages
	logger.Info("Test message 1")
	logger.Warn("Test message 2")
	logger.Error("Test message 3")

	// Read the log file
	content, err := os.ReadFile(tmpFile.Name())
	if err != nil {
		t.Fatalf("Failed to read log file: %v", err)
	}

	output := string(content)

	// Check that messages were written to file
	if !strings.Contains(output, "Test message 1") {
		t.Error("Expected message 1 to be in log file")
	}
	if !strings.Contains(output, "Test message 2") {
		t.Error("Expected message 2 to be in log file")
	}
	if !strings.Contains(output, "Test message 3") {
		t.Error("Expected message 3 to be in log file")
	}
}

func TestRuntimeLogger(t *testing.T) {
	// Create a buffer to capture log output
	var buf bytes.Buffer

	// Create runtime logger
	logger := NewRuntimeLogger("test_component", LogLevelInfo, &buf)

	// Test job logging
	t.Run("JobLogging", func(t *testing.T) {
		logger.LogJobStart("job_123", "TestJob")
		logger.LogJobEnd("job_123", "TestJob", "completed", 5*time.Second)

		output := buf.String()

		if !strings.Contains(output, "Job started") {
			t.Error("Expected job start message")
		}
		if !strings.Contains(output, "Job completed") {
			t.Error("Expected job end message")
		}
		if !strings.Contains(output, "job_id=job_123") {
			t.Error("Expected job_id field")
		}
		if !strings.Contains(output, "job_name=TestJob") {
			t.Error("Expected job_name field")
		}
	})

	// Test step logging
	t.Run("StepLogging", func(t *testing.T) {
		buf.Reset()

		logger.LogStepStart("job_123", "Step1")
		logger.LogStepEnd("job_123", "Step1", "completed", 2*time.Second)

		output := buf.String()

		if !strings.Contains(output, "Step started") {
			t.Error("Expected step start message")
		}
		if !strings.Contains(output, "Step completed") {
			t.Error("Expected step end message")
		}
		if !strings.Contains(output, "step_name=Step1") {
			t.Error("Expected step_name field")
		}
	})

	// Test dataset operation logging
	t.Run("DatasetOperationLogging", func(t *testing.T) {
		buf.Reset()

		logger.LogDatasetOperation("READ", "test_dataset", 100, 1*time.Second)

		output := buf.String()

		if !strings.Contains(output, "Dataset operation completed") {
			t.Error("Expected dataset operation message")
		}
		if !strings.Contains(output, "operation=READ") {
			t.Error("Expected operation field")
		}
		if !strings.Contains(output, "dataset_name=test_dataset") {
			t.Error("Expected dataset_name field")
		}
		if !strings.Contains(output, "record_count=100") {
			t.Error("Expected record_count field")
		}
	})

	// Test transaction logging
	t.Run("TransactionLogging", func(t *testing.T) {
		buf.Reset()

		logger.LogTransactionStart("txn_123")
		logger.LogTransactionEnd("txn_123", "committed", 3*time.Second)

		output := buf.String()

		if !strings.Contains(output, "Transaction started") {
			t.Error("Expected transaction start message")
		}
		if !strings.Contains(output, "Transaction completed") {
			t.Error("Expected transaction end message")
		}
		if !strings.Contains(output, "transaction_id=txn_123") {
			t.Error("Expected transaction_id field")
		}
		if !strings.Contains(output, "status=committed") {
			t.Error("Expected status field")
		}
	})

	// Test error logging
	t.Run("ErrorLogging", func(t *testing.T) {
		buf.Reset()

		err := fmt.Errorf("test error")
		fields := map[string]interface{}{
			"operation":   "test_operation",
			"retry_count": 3,
		}

		logger.LogError(err, "test_context", fields)

		output := buf.String()

		if !strings.Contains(output, "Error occurred") {
			t.Error("Expected error message")
		}
		if !strings.Contains(output, "context=test_context") {
			t.Error("Expected context field")
		}
		if !strings.Contains(output, "operation=test_operation") {
			t.Error("Expected operation field")
		}
		if !strings.Contains(output, "retry_count=3") {
			t.Error("Expected retry_count field")
		}
	})

	// Test performance logging
	t.Run("PerformanceLogging", func(t *testing.T) {
		buf.Reset()

		metrics := map[string]interface{}{
			"memory_usage": "50MB",
			"cpu_usage":    "25%",
		}

		logger.LogPerformance("test_operation", 1*time.Second, metrics)

		output := buf.String()

		if !strings.Contains(output, "Performance metric") {
			t.Error("Expected performance message")
		}
		if !strings.Contains(output, "operation=test_operation") {
			t.Error("Expected operation field")
		}
		if !strings.Contains(output, "memory_usage=50MB") {
			t.Error("Expected memory_usage field")
		}
		if !strings.Contains(output, "cpu_usage=25%") {
			t.Error("Expected cpu_usage field")
		}
	})

	// Test checkpoint logging
	t.Run("CheckpointLogging", func(t *testing.T) {
		buf.Reset()

		checkpointData := map[string]interface{}{
			"variables": map[string]interface{}{
				"counter": 42,
				"status":  "running",
			},
			"position": 100,
		}

		logger.LogCheckpoint("job_123", "step_1", checkpointData)

		output := buf.String()

		if !strings.Contains(output, "Checkpoint created") {
			t.Error("Expected checkpoint message")
		}
		if !strings.Contains(output, "job_id=job_123") {
			t.Error("Expected job_id field")
		}
		if !strings.Contains(output, "step_id=step_1") {
			t.Error("Expected step_id field")
		}
	})

	// Test recovery logging
	t.Run("RecoveryLogging", func(t *testing.T) {
		buf.Reset()

		logger.LogRecovery("job_123", "step_1", "restart", true)

		output := buf.String()

		if !strings.Contains(output, "Recovery operation completed") {
			t.Error("Expected recovery message")
		}
		if !strings.Contains(output, "job_id=job_123") {
			t.Error("Expected job_id field")
		}
		if !strings.Contains(output, "step_id=step_1") {
			t.Error("Expected step_id field")
		}
		if !strings.Contains(output, "recovery_type=restart") {
			t.Error("Expected recovery_type field")
		}
		if !strings.Contains(output, "success=true") {
			t.Error("Expected success field")
		}
	})
}

func TestErrorHandler(t *testing.T) {
	// Create a buffer to capture log output
	var buf bytes.Buffer

	// Create logger and error handler
	logger := NewLogger(LogLevelError, &buf)
	errorHandler := NewErrorHandler(logger)

	// Test error handling
	t.Run("HandleError", func(t *testing.T) {
		buf.Reset()

		originalErr := fmt.Errorf("original error")
		fields := map[string]interface{}{
			"operation":   "test_operation",
			"retry_count": 3,
		}

		wrappedErr := errorHandler.HandleError(originalErr, "test_context", fields)

		output := buf.String()

		// Check that error was logged
		if !strings.Contains(output, "Error in test_context") {
			t.Error("Expected error to be logged")
		}

		// Check that wrapped error contains context
		if !strings.Contains(wrappedErr.Error(), "test_context") {
			t.Error("Expected wrapped error to contain context")
		}

		// Check that original error is preserved
		if !strings.Contains(wrappedErr.Error(), "original error") {
			t.Error("Expected wrapped error to contain original error")
		}
	})

	// Test panic handling
	t.Run("HandlePanic", func(t *testing.T) {
		if testing.Short() {
			t.Skip("Skipping panic recovery test in short mode (calls Fatal)")
		}

		buf.Reset()

		// This should not panic due to recovery
		// Note: HandlePanic calls Fatal which calls os.Exit, so we skip this in normal testing
		// In production, this ensures the panic is logged before the process exits
		t.Skip("Skipping test that would call os.Exit (Fatal)")
	})

	// Test error handling with recovery
	t.Run("HandleErrorWithRecovery", func(t *testing.T) {
		buf.Reset()

		originalErr := fmt.Errorf("original error")
		recoveryCalled := false

		recovery := func() error {
			recoveryCalled = true
			return nil
		}

		fields := map[string]interface{}{
			"operation": "test_operation",
		}

		wrappedErr := errorHandler.HandleErrorWithRecovery(originalErr, "test_context", recovery, fields)

		// Check that recovery was called
		if !recoveryCalled {
			t.Error("Expected recovery function to be called")
		}

		// Check that error was still returned
		if wrappedErr == nil {
			t.Error("Expected wrapped error to be returned")
		}

		output := buf.String()

		// Check that error was logged
		if !strings.Contains(output, "Error in test_context") {
			t.Error("Expected error to be logged")
		}
	})
}
