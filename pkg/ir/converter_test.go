package ir

import (
	"testing"

	"github.com/cobgo/cobgo-community/pkg/parser"
)

func TestNewASTToIRConverter(t *testing.T) {
	converter := NewASTToIRConverter()

	if converter.Program == nil {
		t.Error("Expected converter program to be initialized")
	}
	if len(converter.Errors) != 0 {
		t.Errorf("Expected no errors, got %d", len(converter.Errors))
	}
}

func TestConvertSimpleJob(t *testing.T) {
	// Create a simple AST program
	astProgram := &parser.Program{
		Jobs: []*parser.JobStatement{
			{
				Token: parser.Token{Type: parser.JOB, Literal: "job", Line: 1, Column: 1},
				Name:  &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "TestJob"}, Value: "TestJob"},
				Body: &parser.BlockStatement{
					Token: parser.Token{Type: parser.LEFT_BRACE, Literal: "{", Line: 1, Column: 10},
					Statements: []parser.Statement{
						&parser.StepStatement{
							Token: parser.Token{Type: parser.STEP, Literal: "step", Line: 2, Column: 1},
							Name:  &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "Main"}, Value: "Main"},
							Body: &parser.BlockStatement{
								Token: parser.Token{Type: parser.LEFT_BRACE, Literal: "{", Line: 2, Column: 10},
								Statements: []parser.Statement{
									&parser.DisplayStatement{
										Token: parser.Token{Type: parser.DISPLAY, Literal: "display", Line: 3, Column: 1},
										Args: []parser.Expression{
											&parser.StringLiteral{
												Token: parser.Token{Type: parser.STRING_LITERAL, Literal: "Hello, World!", Line: 3, Column: 10},
												Value: "Hello, World!",
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	converter := NewASTToIRConverter()
	program, err := converter.Convert(astProgram)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if program == nil {
		t.Error("Expected non-nil program")
	}
	if len(program.Jobs) != 1 {
		t.Errorf("Expected 1 job, got %d", len(program.Jobs))
	}
	if program.Jobs[0].Name != "TestJob" {
		t.Errorf("Expected job name 'TestJob', got '%s'", program.Jobs[0].Name)
	}
	if len(program.Jobs[0].Steps) != 1 {
		t.Errorf("Expected 1 step, got %d", len(program.Jobs[0].Steps))
	}
	if program.Jobs[0].Steps[0].Name != "Main" {
		t.Errorf("Expected step name 'Main', got '%s'", program.Jobs[0].Steps[0].Name)
	}
}

func TestConvertJobWithVariables(t *testing.T) {
	// Create an AST program with variables
	astProgram := &parser.Program{
		Jobs: []*parser.JobStatement{
			{
				Token: parser.Token{Type: parser.JOB, Literal: "job", Line: 1, Column: 1},
				Name:  &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "TestJob"}, Value: "TestJob"},
				Body: &parser.BlockStatement{
					Token: parser.Token{Type: parser.LEFT_BRACE, Literal: "{", Line: 1, Column: 10},
					Statements: []parser.Statement{
						&parser.VarStatement{
							Token: parser.Token{Type: parser.VAR, Literal: "var", Line: 2, Column: 1},
							Name:  &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "count"}, Value: "count"},
							Type: &parser.TypeAnnotation{
								Type:     parser.INT32,
								TypeName: "int32",
							},
							Value: &parser.IntegerLiteral{
								Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "10", Line: 2, Column: 15},
								Value: 10,
							},
						},
					},
				},
			},
		},
	}

	converter := NewASTToIRConverter()
	program, err := converter.Convert(astProgram)

	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}
	if len(program.Jobs[0].Variables) != 1 {
		t.Errorf("Expected 1 variable, got %d", len(program.Jobs[0].Variables))
	}
	if program.Jobs[0].Variables[0].Name != "count" {
		t.Errorf("Expected variable name 'count', got '%s'", program.Jobs[0].Variables[0].Name)
	}
	if program.Jobs[0].Variables[0].Type.Type != "int32" {
		t.Errorf("Expected variable type 'int32', got '%s'", program.Jobs[0].Variables[0].Type.Type)
	}
}

func TestConvertRecord(t *testing.T) {
	// Create an AST program with a record
	// Note: Records are not currently parsed by the parser, so we'll test the converter directly

	// Add a record to the program (this would normally be done by the parser)
	// For testing, we'll create the record directly
	record := &parser.RecordStatement{
		Token: parser.Token{Type: parser.RECORD, Literal: "record", Line: 1, Column: 1},
		Name:  &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "Customer"}, Value: "Customer"},
		Fields: []*parser.FieldDeclaration{
			{
				Name: &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "id"}, Value: "id"},
				Type: &parser.TypeAnnotation{
					Type:     parser.INT32,
					TypeName: "int32",
				},
			},
			{
				Name: &parser.Identifier{Token: parser.Token{Type: parser.IDENTIFIER, Literal: "name"}, Value: "name"},
				Type: &parser.TypeAnnotation{
					Type:     parser.STRING,
					TypeName: "string",
					Size: &parser.IntegerLiteral{
						Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "50", Line: 2, Column: 20},
						Value: 50,
					},
				},
			},
		},
	}

	converter := NewASTToIRConverter()
	irRecord := converter.convertRecord(record)

	if irRecord.Name != "Customer" {
		t.Errorf("Expected record name 'Customer', got '%s'", irRecord.Name)
	}
	if len(irRecord.Fields) != 2 {
		t.Errorf("Expected 2 fields, got %d", len(irRecord.Fields))
	}
	if irRecord.Fields[0].Name != "id" {
		t.Errorf("Expected first field name 'id', got '%s'", irRecord.Fields[0].Name)
	}
	if irRecord.Fields[1].Name != "name" {
		t.Errorf("Expected second field name 'name', got '%s'", irRecord.Fields[1].Name)
	}
}

func TestConvertExpressions(t *testing.T) {
	converter := NewASTToIRConverter()

	// Test identifier conversion
	identAST := &parser.Identifier{
		Token: parser.Token{Type: parser.IDENTIFIER, Literal: "testVar", Line: 1, Column: 1},
		Value: "testVar",
	}
	identIR := converter.convertIdentifier(identAST)
	if identIR.Name != "testVar" {
		t.Errorf("Expected identifier name 'testVar', got '%s'", identIR.Name)
	}

	// Test integer literal conversion
	intAST := &parser.IntegerLiteral{
		Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "42", Line: 1, Column: 1},
		Value: 42,
	}
	intIR := converter.convertIntegerLiteral(intAST)
	if intIR.Value != int64(42) {
		t.Errorf("Expected integer value 42, got %v", intIR.Value)
	}
	if intIR.Type != "integer" {
		t.Errorf("Expected literal type 'integer', got '%s'", intIR.Type)
	}

	// Test string literal conversion
	strAST := &parser.StringLiteral{
		Token: parser.Token{Type: parser.STRING_LITERAL, Literal: "hello", Line: 1, Column: 1},
		Value: "hello",
	}
	strIR := converter.convertStringLiteral(strAST)
	if strIR.Value != "hello" {
		t.Errorf("Expected string value 'hello', got '%v'", strIR.Value)
	}
	if strIR.Type != "string" {
		t.Errorf("Expected literal type 'string', got '%s'", strIR.Type)
	}
}

func TestConvertBinaryExpression(t *testing.T) {
	converter := NewASTToIRConverter()

	// Create a binary expression AST: 5 + 3
	leftAST := &parser.IntegerLiteral{
		Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "5", Line: 1, Column: 1},
		Value: 5,
	}
	rightAST := &parser.IntegerLiteral{
		Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "3", Line: 1, Column: 5},
		Value: 3,
	}
	binaryAST := &parser.InfixExpression{
		Token:    parser.Token{Type: parser.PLUS, Literal: "+", Line: 1, Column: 3},
		Left:     leftAST,
		Operator: "+",
		Right:    rightAST,
	}

	binaryIR := converter.convertInfixExpression(binaryAST)
	if binaryIR.Operator != "+" {
		t.Errorf("Expected operator '+', got '%s'", binaryIR.Operator)
	}
	if binaryIR.Left == nil {
		t.Error("Expected non-nil left operand")
	}
	if binaryIR.Right == nil {
		t.Error("Expected non-nil right operand")
	}
}

func TestGetGoType(t *testing.T) {
	converter := NewASTToIRConverter()

	tests := []struct {
		cobgoType string
		expected  string
	}{
		{"int32", "int32"},
		{"int64", "int64"},
		{"string", "string"},
		{"decimal", "decimal.Decimal"},
		{"date", "time.Time"},
		{"bool", "bool"},
		{"unknown", "interface{}"},
	}

	for _, test := range tests {
		result := converter.getGoType(test.cobgoType)
		if result != test.expected {
			t.Errorf("Expected Go type '%s' for CobGO type '%s', got '%s'",
				test.expected, test.cobgoType, result)
		}
	}
}

func TestConvertTypeAnnotation(t *testing.T) {
	converter := NewASTToIRConverter()

	// Test basic type annotation
	typeAST := &parser.TypeAnnotation{
		Type:     parser.STRING,
		TypeName: "string",
		Size: &parser.IntegerLiteral{
			Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "50", Line: 1, Column: 1},
			Value: 50,
		},
	}

	typeIR := converter.convertTypeAnnotation(typeAST)
	if typeIR.Type != "string" {
		t.Errorf("Expected type 'string', got '%s'", typeIR.Type)
	}
	if typeIR.GoType != "string" {
		t.Errorf("Expected Go type 'string', got '%s'", typeIR.GoType)
	}
	if typeIR.Size == nil || *typeIR.Size != 50 {
		t.Errorf("Expected size 50, got %v", typeIR.Size)
	}

	// Test decimal type annotation
	decimalAST := &parser.TypeAnnotation{
		Type:     parser.DECIMAL,
		TypeName: "decimal",
		Precision: &parser.IntegerLiteral{
			Token: parser.Token{Type: parser.NUMBER_LITERAL, Literal: "10", Line: 1, Column: 1},
			Value: 10,
		},
	}

	decimalIR := converter.convertTypeAnnotation(decimalAST)
	if decimalIR.Type != "decimal" {
		t.Errorf("Expected type 'decimal', got '%s'", decimalIR.Type)
	}
	if decimalIR.GoType != "decimal.Decimal" {
		t.Errorf("Expected Go type 'decimal.Decimal', got '%s'", decimalIR.GoType)
	}
	if decimalIR.Precision == nil || *decimalIR.Precision != 10 {
		t.Errorf("Expected precision 10, got %v", decimalIR.Precision)
	}
	if decimalIR.Scale == nil || *decimalIR.Scale != 0 {
		t.Errorf("Expected scale 0 (default), got %v", decimalIR.Scale)
	}
}
