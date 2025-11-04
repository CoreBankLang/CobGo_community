package parser

// Grammar defines the CobGO DSL grammar rules
// This is a hand-rolled parser for better Go integration and control

// Token types for the CobGO DSL
type TokenType int

const (
	// Special tokens
	EOF TokenType = iota
	ILLEGAL
	NEWLINE
	WHITESPACE

	// Literals
	IDENTIFIER
	STRING_LITERAL
	NUMBER_LITERAL
	DECIMAL_LITERAL
	BOOLEAN_LITERAL
	DATE_LITERAL

	// Keywords
	JOB
	STEP
	RECORD
	VAR
	IF
	ELSE
	FOR
	WHILE
	LOOP
	SWITCH
	CASE
	DEFAULT
	TRY
	CATCH
	RETURN
	DISPLAY
	ACCEPT
	READ
	WRITE
	FILE
	EXIT
	BREAK
	CONTINUE

	// COBOL-specific keywords
	PERFORM
	UNTIL
	VARYING
	TIMES
	EVALUATE
	WHEN
	MOVE
	COMPUTE
	OPEN
	CLOSE
	REWRITE
	DELETE
	COPY
	OCCURS
	REDEFINES
	PIC
	PICTURE
	VALUE
	USING
	INTO
	FROM
	INVALID
	END
	TO
	BY
	AT
	IN
	ON

	// COBOL-85 Advanced keywords
	INSPECT
	TALLYING
	REPLACING
	CONVERTING
	STRING_STMT // renamed to avoid conflict with STRING type
	UNSTRING
	SEARCH
	DELIMITED
	POINTER
	OVERFLOW
	SIZE
	INITIAL
	BEFORE
	AFTER
	ALL
	LEADING
	FIRST
	CHARACTERS
	WITH

	// Data types
	DECIMAL
	STRING
	DATE
	INT32
	INT64
	BOOL
	ARRAY

	// Operators
	ASSIGN        // =
	PLUS          // +
	MINUS         // -
	MULTIPLY      // *
	DIVIDE        // /
	MODULO        // %
	EQUAL         // ==
	NOT_EQUAL     // !=
	LESS          // <
	LESS_EQUAL    // <=
	GREATER       // >
	GREATER_EQUAL // >=
	AND           // &&
	OR            // ||
	NOT           // !

	// Delimiters
	LEFT_PAREN    // (
	RIGHT_PAREN   // )
	LEFT_BRACE    // {
	RIGHT_BRACE   // }
	LEFT_BRACKET  // [
	RIGHT_BRACKET // ]
	COMMA         // ,
	SEMICOLON     // ;
	COLON         // :
	DOT           // .

	// Comments
	LINE_COMMENT  // //
	BLOCK_COMMENT // /* */
)

// Token represents a lexical token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

// String returns a string representation of the token type
func (tt TokenType) String() string {
	switch tt {
	case EOF:
		return "EOF"
	case ILLEGAL:
		return "ILLEGAL"
	case NEWLINE:
		return "NEWLINE"
	case WHITESPACE:
		return "WHITESPACE"
	case IDENTIFIER:
		return "IDENTIFIER"
	case STRING_LITERAL:
		return "STRING_LITERAL"
	case NUMBER_LITERAL:
		return "NUMBER_LITERAL"
	case DECIMAL_LITERAL:
		return "DECIMAL_LITERAL"
	case BOOLEAN_LITERAL:
		return "BOOLEAN_LITERAL"
	case DATE_LITERAL:
		return "DATE_LITERAL"
	case JOB:
		return "JOB"
	case STEP:
		return "STEP"
	case RECORD:
		return "RECORD"
	case VAR:
		return "VAR"
	case IF:
		return "IF"
	case ELSE:
		return "ELSE"
	case FOR:
		return "FOR"
	case WHILE:
		return "WHILE"
	case LOOP:
		return "LOOP"
	case SWITCH:
		return "SWITCH"
	case CASE:
		return "CASE"
	case DEFAULT:
		return "DEFAULT"
	case TRY:
		return "TRY"
	case CATCH:
		return "CATCH"
	case RETURN:
		return "RETURN"
	case DISPLAY:
		return "DISPLAY"
	case ACCEPT:
		return "ACCEPT"
	case READ:
		return "READ"
	case WRITE:
		return "WRITE"
	case FILE:
		return "FILE"
	case EXIT:
		return "EXIT"
	case BREAK:
		return "BREAK"
	case CONTINUE:
		return "CONTINUE"
	case DECIMAL:
		return "DECIMAL"
	case STRING:
		return "STRING"
	case DATE:
		return "DATE"
	case INT32:
		return "INT32"
	case INT64:
		return "INT64"
	case BOOL:
		return "BOOL"
	case ARRAY:
		return "ARRAY"
	case ASSIGN:
		return "ASSIGN"
	case PLUS:
		return "PLUS"
	case MINUS:
		return "MINUS"
	case MULTIPLY:
		return "MULTIPLY"
	case DIVIDE:
		return "DIVIDE"
	case MODULO:
		return "MODULO"
	case EQUAL:
		return "EQUAL"
	case NOT_EQUAL:
		return "NOT_EQUAL"
	case LESS:
		return "LESS"
	case LESS_EQUAL:
		return "LESS_EQUAL"
	case GREATER:
		return "GREATER"
	case GREATER_EQUAL:
		return "GREATER_EQUAL"
	case AND:
		return "AND"
	case OR:
		return "OR"
	case NOT:
		return "NOT"
	case LEFT_PAREN:
		return "LEFT_PAREN"
	case RIGHT_PAREN:
		return "RIGHT_PAREN"
	case LEFT_BRACE:
		return "LEFT_BRACE"
	case RIGHT_BRACE:
		return "RIGHT_BRACE"
	case LEFT_BRACKET:
		return "LEFT_BRACKET"
	case RIGHT_BRACKET:
		return "RIGHT_BRACKET"
	case COMMA:
		return "COMMA"
	case SEMICOLON:
		return "SEMICOLON"
	case COLON:
		return "COLON"
	case DOT:
		return "DOT"
	case LINE_COMMENT:
		return "LINE_COMMENT"
	case BLOCK_COMMENT:
		return "BLOCK_COMMENT"
	case PERFORM:
		return "PERFORM"
	case UNTIL:
		return "UNTIL"
	case VARYING:
		return "VARYING"
	case TIMES:
		return "TIMES"
	case EVALUATE:
		return "EVALUATE"
	case WHEN:
		return "WHEN"
	case MOVE:
		return "MOVE"
	case COMPUTE:
		return "COMPUTE"
	case OPEN:
		return "OPEN"
	case CLOSE:
		return "CLOSE"
	case REWRITE:
		return "REWRITE"
	case DELETE:
		return "DELETE"
	case COPY:
		return "COPY"
	case OCCURS:
		return "OCCURS"
	case REDEFINES:
		return "REDEFINES"
	case PIC:
		return "PIC"
	case PICTURE:
		return "PICTURE"
	case VALUE:
		return "VALUE"
	case USING:
		return "USING"
	case INTO:
		return "INTO"
	case FROM:
		return "FROM"
	case INVALID:
		return "INVALID"
	case END:
		return "END"
	case TO:
		return "TO"
	case BY:
		return "BY"
	case AT:
		return "AT"
	case IN:
		return "IN"
	case ON:
		return "ON"
	case INSPECT:
		return "INSPECT"
	case TALLYING:
		return "TALLYING"
	case REPLACING:
		return "REPLACING"
	case CONVERTING:
		return "CONVERTING"
	case STRING_STMT:
		return "STRING_STMT"
	case UNSTRING:
		return "UNSTRING"
	case SEARCH:
		return "SEARCH"
	case DELIMITED:
		return "DELIMITED"
	case POINTER:
		return "POINTER"
	case OVERFLOW:
		return "OVERFLOW"
	case SIZE:
		return "SIZE"
	case INITIAL:
		return "INITIAL"
	case BEFORE:
		return "BEFORE"
	case AFTER:
		return "AFTER"
	case ALL:
		return "ALL"
	case LEADING:
		return "LEADING"
	case FIRST:
		return "FIRST"
	case CHARACTERS:
		return "CHARACTERS"
	case WITH:
		return "WITH"
	default:
		return "UNKNOWN"
	}
}

// Keywords map string literals to token types
var keywords = map[string]TokenType{
	"job":      JOB,
	"step":     STEP,
	"record":   RECORD,
	"var":      VAR,
	"if":       IF,
	"else":     ELSE,
	"for":      FOR,
	"while":    WHILE,
	"loop":     LOOP,
	"switch":   SWITCH,
	"case":     CASE,
	"default":  DEFAULT,
	"try":      TRY,
	"catch":    CATCH,
	"return":   RETURN,
	"display":  DISPLAY,
	"accept":   ACCEPT,
	"read":     READ,
	"write":    WRITE,
	"file":     FILE,
	"exit":     EXIT,
	"break":    BREAK,
	"continue": CONTINUE,
	"decimal":  DECIMAL,
	"string":   STRING,
	"date":     DATE,
	"int32":    INT32,
	"int64":    INT64,
	"bool":     BOOL,
	"array":    ARRAY,
	"true":     BOOLEAN_LITERAL,
	"false":    BOOLEAN_LITERAL,

	// COBOL-specific keywords
	"perform":   PERFORM,
	"until":     UNTIL,
	"varying":   VARYING,
	"times":     TIMES,
	"evaluate":  EVALUATE,
	"when":      WHEN,
	"move":      MOVE,
	"compute":   COMPUTE,
	"open":      OPEN,
	"close":     CLOSE,
	"rewrite":   REWRITE,
	"delete":    DELETE,
	"copy":      COPY,
	"occurs":    OCCURS,
	"redefines": REDEFINES,
	"pic":       PIC,
	"picture":   PICTURE,
	"value":     VALUE,
	"using":     USING,
	"into":      INTO,
	"from":      FROM,
	"invalid":   INVALID,
	"end":       END,
	"to":        TO,
	"by":        BY,
	"at":        AT,
	"in":        IN,
	"on":        ON,

	// COBOL-85 Advanced keywords
	"inspect":    INSPECT,
	"tallying":   TALLYING,
	"replacing":  REPLACING,
	"converting": CONVERTING,
	// Note: "string" statement will need context-sensitive parsing
	// since it conflicts with "string" type
	"unstring":   UNSTRING,
	"search":     SEARCH,
	"delimited":  DELIMITED,
	"pointer":    POINTER,
	"overflow":   OVERFLOW,
	"size":       SIZE,
	"initial":    INITIAL,
	"before":     BEFORE,
	"after":      AFTER,
	"all":        ALL,
	"leading":    LEADING,
	"first":      FIRST,
	"characters": CHARACTERS,
	"with":       WITH,
}

// LookupIdentifier checks if the identifier is a keyword
func LookupIdentifier(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENTIFIER
}
