package runtime

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"
)

// LogLevel represents the logging level
type LogLevel int

const (
	LogLevelDebug LogLevel = iota
	LogLevelInfo
	LogLevelWarn
	LogLevelError
	LogLevelFatal
)

// LogEntry represents a log entry
type LogEntry struct {
	Timestamp time.Time
	Level     LogLevel
	Message   string
	Fields    map[string]interface{}
	Source    string
	Line      int
}

// Logger provides structured logging functionality
type Logger struct {
	level  LogLevel
	logger *log.Logger
	output io.Writer
	fields map[string]interface{}
	mu     sync.RWMutex
}

// NewLogger creates a new logger
func NewLogger(level LogLevel, output io.Writer) *Logger {
	if output == nil {
		output = os.Stdout
	}

	return &Logger{
		level:  level,
		logger: log.New(output, "", 0),
		output: output,
		fields: make(map[string]interface{}),
	}
}

// NewFileLogger creates a new file logger
func NewFileLogger(level LogLevel, logFile string) (*Logger, error) {
	// Create log directory if it doesn't exist
	logDir := filepath.Dir(logFile)
	if err := os.MkdirAll(logDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create log directory: %w", err)
	}

	file, err := os.OpenFile(logFile, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		return nil, fmt.Errorf("failed to open log file: %w", err)
	}

	return NewLogger(level, file), nil
}

// SetLevel sets the logging level
func (l *Logger) SetLevel(level LogLevel) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.level = level
}

// WithField adds a field to the logger context
func (l *Logger) WithField(key string, value interface{}) *Logger {
	l.mu.Lock()
	defer l.mu.Unlock()

	newLogger := &Logger{
		level:  l.level,
		logger: l.logger,
		output: l.output,
		fields: make(map[string]interface{}),
	}

	// Copy existing fields
	for k, v := range l.fields {
		newLogger.fields[k] = v
	}

	// Add new field
	newLogger.fields[key] = value

	return newLogger
}

// WithFields adds multiple fields to the logger context
func (l *Logger) WithFields(fields map[string]interface{}) *Logger {
	l.mu.Lock()
	defer l.mu.Unlock()

	newLogger := &Logger{
		level:  l.level,
		logger: l.logger,
		output: l.output,
		fields: make(map[string]interface{}),
	}

	// Copy existing fields
	for k, v := range l.fields {
		newLogger.fields[k] = v
	}

	// Add new fields
	for k, v := range fields {
		newLogger.fields[k] = v
	}

	return newLogger
}

// Debug logs a debug message
func (l *Logger) Debug(message string, args ...interface{}) {
	l.log(LogLevelDebug, message, args...)
}

// Info logs an info message
func (l *Logger) Info(message string, args ...interface{}) {
	l.log(LogLevelInfo, message, args...)
}

// Warn logs a warning message
func (l *Logger) Warn(message string, args ...interface{}) {
	l.log(LogLevelWarn, message, args...)
}

// Error logs an error message
func (l *Logger) Error(message string, args ...interface{}) {
	l.log(LogLevelError, message, args...)
}

// Fatal logs a fatal message and exits
func (l *Logger) Fatal(message string, args ...interface{}) {
	l.log(LogLevelFatal, message, args...)
	os.Exit(1)
}

// log logs a message at the specified level
func (l *Logger) log(level LogLevel, message string, args ...interface{}) {
	if level < l.level {
		return
	}

	// Format message
	formattedMessage := message
	if len(args) > 0 {
		formattedMessage = fmt.Sprintf(message, args...)
	}

	// Get caller information
	_, file, line, ok := runtime.Caller(3)
	source := "unknown"
	if ok {
		source = filepath.Base(file)
	}

	// Create log entry
	entry := &LogEntry{
		Timestamp: time.Now(),
		Level:     level,
		Message:   formattedMessage,
		Fields:    make(map[string]interface{}),
		Source:    source,
		Line:      line,
	}

	// Copy fields
	l.mu.RLock()
	for k, v := range l.fields {
		entry.Fields[k] = v
	}
	l.mu.RUnlock()

	// Format and write log entry
	logLine := l.formatLogEntry(entry)
	l.logger.Println(logLine)
}

// formatLogEntry formats a log entry for output
func (l *Logger) formatLogEntry(entry *LogEntry) string {
	levelStr := l.levelToString(entry.Level)
	timestamp := entry.Timestamp.Format("2006-01-02 15:04:05.000")

	// Build fields string
	fieldsStr := ""
	if len(entry.Fields) > 0 {
		fieldParts := make([]string, 0, len(entry.Fields))
		for k, v := range entry.Fields {
			fieldParts = append(fieldParts, fmt.Sprintf("%s=%v", k, v))
		}
		fieldsStr = " " + strings.Join(fieldParts, " ")
	}

	return fmt.Sprintf("[%s] %s %s:%d %s%s",
		timestamp, levelStr, entry.Source, entry.Line, entry.Message, fieldsStr)
}

// levelToString converts a log level to string
func (l *Logger) levelToString(level LogLevel) string {
	switch level {
	case LogLevelDebug:
		return "DEBUG"
	case LogLevelInfo:
		return "INFO"
	case LogLevelWarn:
		return "WARN"
	case LogLevelError:
		return "ERROR"
	case LogLevelFatal:
		return "FATAL"
	default:
		return "UNKNOWN"
	}
}

// ErrorHandler provides centralized error handling
type ErrorHandler struct {
	logger *Logger
}

// NewErrorHandler creates a new error handler
func NewErrorHandler(logger *Logger) *ErrorHandler {
	return &ErrorHandler{
		logger: logger,
	}
}

// HandleError handles an error with appropriate logging and recovery
func (eh *ErrorHandler) HandleError(err error, context string, fields map[string]interface{}) error {
	if err == nil {
		return nil
	}

	// Log the error
	logger := eh.logger.WithFields(fields)
	logger.Error("Error in %s: %v", context, err)

	// Return wrapped error with context
	return fmt.Errorf("%s: %w", context, err)
}

// HandlePanic handles panics with recovery
func (eh *ErrorHandler) HandlePanic(context string, fields map[string]interface{}) {
	if r := recover(); r != nil {
		logger := eh.logger.WithFields(fields)
		logger.Fatal("Panic in %s: %v", context, r)
	}
}

// HandleErrorWithRecovery handles an error with recovery action
func (eh *ErrorHandler) HandleErrorWithRecovery(err error, context string, recovery func() error, fields map[string]interface{}) error {
	if err == nil {
		return nil
	}

	// Log the error
	logger := eh.logger.WithFields(fields)
	logger.Error("Error in %s: %v", context, err)

	// Attempt recovery
	if recovery != nil {
		if recoveryErr := recovery(); recoveryErr != nil {
			logger.Error("Recovery failed in %s: %v", context, recoveryErr)
			return fmt.Errorf("%s (recovery failed): %w", context, err)
		}
		logger.Info("Recovery successful in %s", context)
	}

	return fmt.Errorf("%s: %w", context, err)
}

// RuntimeLogger provides runtime-specific logging
type RuntimeLogger struct {
	*Logger
	component string
}

// NewRuntimeLogger creates a new runtime logger
func NewRuntimeLogger(component string, level LogLevel, output io.Writer) *RuntimeLogger {
	return &RuntimeLogger{
		Logger:    NewLogger(level, output),
		component: component,
	}
}

// LogJobStart logs the start of a job
func (rl *RuntimeLogger) LogJobStart(jobID string, jobName string) {
	rl.WithFields(map[string]interface{}{
		"job_id":    jobID,
		"job_name":  jobName,
		"component": rl.component,
	}).Info("Job started")
}

// LogJobEnd logs the end of a job
func (rl *RuntimeLogger) LogJobEnd(jobID string, jobName string, status string, duration time.Duration) {
	rl.WithFields(map[string]interface{}{
		"job_id":    jobID,
		"job_name":  jobName,
		"status":    status,
		"duration":  duration.String(),
		"component": rl.component,
	}).Info("Job completed")
}

// LogStepStart logs the start of a step
func (rl *RuntimeLogger) LogStepStart(jobID string, stepName string) {
	rl.WithFields(map[string]interface{}{
		"job_id":    jobID,
		"step_name": stepName,
		"component": rl.component,
	}).Info("Step started")
}

// LogStepEnd logs the end of a step
func (rl *RuntimeLogger) LogStepEnd(jobID string, stepName string, status string, duration time.Duration) {
	rl.WithFields(map[string]interface{}{
		"job_id":    jobID,
		"step_name": stepName,
		"status":    status,
		"duration":  duration.String(),
		"component": rl.component,
	}).Info("Step completed")
}

// LogDatasetOperation logs a dataset operation
func (rl *RuntimeLogger) LogDatasetOperation(operation string, datasetName string, recordCount int, duration time.Duration) {
	rl.WithFields(map[string]interface{}{
		"operation":    operation,
		"dataset_name": datasetName,
		"record_count": recordCount,
		"duration":     duration.String(),
		"component":    rl.component,
	}).Info("Dataset operation completed")
}

// LogTransactionStart logs the start of a transaction
func (rl *RuntimeLogger) LogTransactionStart(transactionID string) {
	rl.WithFields(map[string]interface{}{
		"transaction_id": transactionID,
		"component":      rl.component,
	}).Info("Transaction started")
}

// LogTransactionEnd logs the end of a transaction
func (rl *RuntimeLogger) LogTransactionEnd(transactionID string, status string, duration time.Duration) {
	rl.WithFields(map[string]interface{}{
		"transaction_id": transactionID,
		"status":         status,
		"duration":       duration.String(),
		"component":      rl.component,
	}).Info("Transaction completed")
}

// LogError logs an error with context
func (rl *RuntimeLogger) LogError(err error, context string, fields map[string]interface{}) {
	allFields := make(map[string]interface{})
	allFields["component"] = rl.component
	allFields["context"] = context

	for k, v := range fields {
		allFields[k] = v
	}

	rl.WithFields(allFields).Error("Error occurred: %v", err)
}

// LogPerformance logs performance metrics
func (rl *RuntimeLogger) LogPerformance(operation string, duration time.Duration, metrics map[string]interface{}) {
	allFields := make(map[string]interface{})
	allFields["operation"] = operation
	allFields["duration"] = duration.String()
	allFields["component"] = rl.component

	for k, v := range metrics {
		allFields[k] = v
	}

	rl.WithFields(allFields).Info("Performance metric")
}

// LogCheckpoint logs a checkpoint
func (rl *RuntimeLogger) LogCheckpoint(jobID string, stepID string, checkpointData map[string]interface{}) {
	allFields := make(map[string]interface{})
	allFields["job_id"] = jobID
	allFields["step_id"] = stepID
	allFields["component"] = rl.component

	for k, v := range checkpointData {
		allFields[k] = v
	}

	rl.WithFields(allFields).Info("Checkpoint created")
}

// LogRecovery logs a recovery operation
func (rl *RuntimeLogger) LogRecovery(jobID string, stepID string, recoveryType string, success bool) {
	rl.WithFields(map[string]interface{}{
		"job_id":        jobID,
		"step_id":       stepID,
		"recovery_type": recoveryType,
		"success":       success,
		"component":     rl.component,
	}).Info("Recovery operation completed")
}
