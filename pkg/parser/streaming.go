package parser

import (
	"bufio"
	"fmt"
	"io"
)

// StreamingParser parses large COBOL/CobGO files efficiently
// without loading the entire file into memory
type StreamingParser struct {
	reader     *bufio.Reader
	chunkSize  int
	lineNumber int
	errors     []error
}

// NewStreamingParser creates a new streaming parser
func NewStreamingParser(reader io.Reader, chunkSize int) *StreamingParser {
	if chunkSize == 0 {
		chunkSize = 10000 // Default 10k lines per chunk
	}

	return &StreamingParser{
		reader:    bufio.NewReader(reader),
		chunkSize: chunkSize,
		errors:    []error{},
	}
}

// ParseChunk parses the next chunk of the file
func (sp *StreamingParser) ParseChunk() (*ProgramChunk, error) {
	chunk := &ProgramChunk{
		StartLine: sp.lineNumber + 1,
		Nodes:     []string{}, // Simplified: just track lines
	}

	lines := make([]string, 0, sp.chunkSize)

	// Read chunk of lines
	for i := 0; i < sp.chunkSize; i++ {
		line, err := sp.reader.ReadString('\n')
		if err == io.EOF {
			if len(line) > 0 {
				lines = append(lines, line)
			}
			break
		}
		if err != nil {
			return nil, fmt.Errorf("error reading file: %w", err)
		}

		lines = append(lines, line)
		sp.lineNumber++
	}

	if len(lines) == 0 {
		return nil, io.EOF
	}

	chunk.EndLine = sp.lineNumber
	chunk.Nodes = lines // Store lines for processing
	chunk.Errors = []error{}

	return chunk, nil
}

// ParseAll parses the entire file in chunks
func (sp *StreamingParser) ParseAll() (*StreamedProgram, error) {
	program := &StreamedProgram{
		Chunks: []*ProgramChunk{},
		Errors: []error{},
	}

	for {
		chunk, err := sp.ParseChunk()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}

		program.Chunks = append(program.Chunks, chunk)
		program.Errors = append(program.Errors, chunk.Errors...)
	}

	return program, nil
}

// HasMore returns true if there's more content to parse
func (sp *StreamingParser) HasMore() bool {
	peek, err := sp.reader.Peek(1)
	return err == nil && len(peek) > 0
}

// GetErrors returns all parsing errors
func (sp *StreamingParser) GetErrors() []error {
	return sp.errors
}

// Reset resets the parser for a new file
func (sp *StreamingParser) Reset(reader io.Reader) {
	sp.reader = bufio.NewReader(reader)
	sp.lineNumber = 0
	sp.errors = sp.errors[:0]
}

// ProgramChunk represents a chunk of a parsed program
type ProgramChunk struct {
	StartLine int
	EndLine   int
	Nodes     []string // Simplified: lines of code
	Errors    []error
}

// StreamedProgram represents a program parsed in chunks
type StreamedProgram struct {
	Chunks []*ProgramChunk
	Errors []error
}

// GetAllLines returns all lines from all chunks
func (sp *StreamedProgram) GetAllLines() []string {
	lines := []string{}
	for _, chunk := range sp.Chunks {
		lines = append(lines, chunk.Nodes...)
	}
	return lines
}

// TotalLines returns the total number of lines parsed
func (sp *StreamedProgram) TotalLines() int {
	if len(sp.Chunks) == 0 {
		return 0
	}
	lastChunk := sp.Chunks[len(sp.Chunks)-1]
	return lastChunk.EndLine
}

// ChunkCount returns the number of chunks
func (sp *StreamedProgram) ChunkCount() int {
	return len(sp.Chunks)
}

// HasErrors returns true if there were parsing errors
func (sp *StreamedProgram) HasErrors() bool {
	return len(sp.Errors) > 0
}

// StreamingCompiler compiles large programs in chunks
type StreamingCompiler struct {
	parser    *StreamingParser
	chunkSize int
	progress  CompilationProgress
}

// NewStreamingCompiler creates a new streaming compiler
func NewStreamingCompiler(reader io.Reader, chunkSize int) *StreamingCompiler {
	return &StreamingCompiler{
		parser:    NewStreamingParser(reader, chunkSize),
		chunkSize: chunkSize,
	}
}

// Compile compiles the program in chunks
func (sc *StreamingCompiler) Compile() (*StreamedProgram, error) {
	program, err := sc.parser.ParseAll()
	if err != nil {
		return nil, err
	}

	sc.progress.TotalChunks = program.ChunkCount()
	sc.progress.CompletedChunks = program.ChunkCount()
	sc.progress.TotalLines = program.TotalLines()

	return program, nil
}

// GetProgress returns the current compilation progress
func (sc *StreamingCompiler) GetProgress() CompilationProgress {
	return sc.progress
}

// CompilationProgress tracks compilation progress
type CompilationProgress struct {
	TotalChunks     int
	CompletedChunks int
	TotalLines      int
	ProcessedLines  int
	Errors          int
}

// PercentComplete returns the percentage of compilation completed
func (cp *CompilationProgress) PercentComplete() float64 {
	if cp.TotalChunks == 0 {
		return 0
	}
	return float64(cp.CompletedChunks) / float64(cp.TotalChunks) * 100
}
