package parser

import (
	"strings"
	"testing"
)

func TestNew(t *testing.T) {
	p := New()
	if p == nil {
		t.Error("New() returned nil")
	}
}

func TestParse(t *testing.T) {
	p := New()

	tests := []struct {
		name    string
		input   string
		wantErr bool
	}{
		{
			name:    "empty input",
			input:   "",
			wantErr: false, // Empty input should parse to empty program
		},
		{
			name:    "simple job",
			input:   "job Test { }",
			wantErr: false,
		},
		{
			name:    "job with step",
			input:   "job Test { step Main { display(\"Hello\") } }",
			wantErr: false,
		},
		{
			name:    "record definition",
			input:   "record Customer { id int32 name string(50) }",
			wantErr: false,
		},
		{
			name:    "variable declaration",
			input:   "job Test { var count int32 = 10 }",
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			reader := strings.NewReader(tt.input)
			program, err := p.Parse(reader)

			if tt.wantErr && err == nil {
				t.Error("Expected error but got none")
			}
			if !tt.wantErr && err != nil {
				t.Errorf("Unexpected error: %v", err)
			}
			if program == nil {
				t.Error("Program should not be nil")
			}
		})
	}
}

func TestLexer(t *testing.T) {
	input := `job Test {
    var name string(50) = "Hello"
    display(name)
}`

	lexer := NewLexer(input)

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{JOB, "job"},
		{IDENTIFIER, "Test"},
		{LEFT_BRACE, "{"},
		{VAR, "var"},
		{IDENTIFIER, "name"},
		{STRING, "string"},
		{LEFT_PAREN, "("},
		{NUMBER_LITERAL, "50"},
		{RIGHT_PAREN, ")"},
		{ASSIGN, "="},
		{STRING_LITERAL, "Hello"},
		{DISPLAY, "display"},
		{LEFT_PAREN, "("},
		{IDENTIFIER, "name"},
		{RIGHT_PAREN, ")"},
		{RIGHT_BRACE, "}"},
		{EOF, ""},
	}

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestParseJobStatement(t *testing.T) {
	input := `job TestJob {
    step Main {
        display("Hello, World!")
    }
}`

	p := New()
	reader := strings.NewReader(input)
	program, err := p.Parse(reader)

	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	if len(program.Jobs) != 1 {
		t.Fatalf("Expected 1 job, got %d", len(program.Jobs))
	}

	job := program.Jobs[0]
	if job.Name.Value != "TestJob" {
		t.Fatalf("Expected job name 'TestJob', got %s", job.Name.Value)
	}

	if len(job.Body.Statements) != 1 {
		t.Fatalf("Expected 1 statement in job body, got %d", len(job.Body.Statements))
	}
}

func TestParseRecordStatement(t *testing.T) {
	input := `record Customer {
    id int32
    name string(50)
    balance decimal(15,2)
}`

	p := New()
	reader := strings.NewReader(input)
	program, err := p.Parse(reader)

	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}

	// Note: Record statements are not currently added to program.Jobs
	// This test verifies parsing doesn't fail
	if program == nil {
		t.Fatal("Program should not be nil")
	}
}
