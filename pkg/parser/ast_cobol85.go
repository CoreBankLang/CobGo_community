package parser

// COBOL-85 AST Types
// These are parser-level AST nodes that get converted to IR
// This avoids circular dependency between parser and IR packages

// InspectStatementAST represents a parsed INSPECT statement
type InspectStatementAST struct {
	Token       Token
	Target      *Identifier
	Operation   string // "TALLYING", "REPLACING", "CONVERTING"
	Mode        string // "ALL", "LEADING", "CHARACTERS", "FIRST"
	Pattern     Expression
	Replacement Expression
	Counter     *Identifier
	BeforeAfter *InspectBeforeAfterAST
	Line        int
	Column      int
}

func (is *InspectStatementAST) statementNode()       {}
func (is *InspectStatementAST) TokenLiteral() string { return is.Token.Literal }
func (is *InspectStatementAST) String() string       { return "INSPECT " + is.Target.String() }

// InspectBeforeAfterAST represents BEFORE/AFTER INITIAL clause
type InspectBeforeAfterAST struct {
	Type      string // "BEFORE" or "AFTER"
	Delimiter Expression
}

// StringStatementAST represents a parsed STRING statement
type StringStatementAST struct {
	Token       Token
	Sources     []*StringSourceAST
	Destination *Identifier
	Delimiter   string
	Pointer     *Identifier
	OnOverflow  *BlockStatement
	Line        int
	Column      int
}

func (ss *StringStatementAST) statementNode()       {}
func (ss *StringStatementAST) TokenLiteral() string { return ss.Token.Literal }
func (ss *StringStatementAST) String() string       { return "STRING" }

// StringSourceAST represents a source field in STRING statement
type StringSourceAST struct {
	Value     Expression
	Delimiter string
}

// UnstringStatementAST represents a parsed UNSTRING statement
type UnstringStatementAST struct {
	Token      Token
	Source     Expression
	Delimiters []string
	Targets    []*Identifier
	Tallying   *Identifier
	Pointer    *Identifier
	OnOverflow *BlockStatement
	Line       int
	Column     int
}

func (us *UnstringStatementAST) statementNode()       {}
func (us *UnstringStatementAST) TokenLiteral() string { return us.Token.Literal }
func (us *UnstringStatementAST) String() string       { return "UNSTRING" }

// SearchStatementAST represents a parsed SEARCH statement
type SearchStatementAST struct {
	Token       Token
	TableName   *Identifier
	IndexName   *Identifier
	IsSearchAll bool // true for SEARCH ALL (binary), false for SEARCH (linear)
	Varying     *Identifier
	WhenClauses []*SearchWhenAST
	AtEnd       *BlockStatement
	Line        int
	Column      int
}

func (ss *SearchStatementAST) statementNode()       {}
func (ss *SearchStatementAST) TokenLiteral() string { return ss.Token.Literal }
func (ss *SearchStatementAST) String() string {
	if ss.IsSearchAll {
		return "SEARCH ALL " + ss.TableName.String()
	}
	return "SEARCH " + ss.TableName.String()
}

// SearchWhenAST represents a WHEN clause in SEARCH statement
type SearchWhenAST struct {
	Condition Expression
	Body      *BlockStatement
}

// Helper function to create INSPECT AST from parsed elements
func newInspectAST(token Token, target *Identifier, operation string) *InspectStatementAST {
	return &InspectStatementAST{
		Token:     token,
		Target:    target,
		Operation: operation,
		Line:      token.Line,
		Column:    token.Column,
	}
}

// Helper function to create STRING AST from parsed elements
func newStringAST(token Token) *StringStatementAST {
	return &StringStatementAST{
		Token:   token,
		Sources: []*StringSourceAST{},
		Line:    token.Line,
		Column:  token.Column,
	}
}

// Helper function to create UNSTRING AST from parsed elements
func newUnstringAST(token Token, source Expression) *UnstringStatementAST {
	return &UnstringStatementAST{
		Token:      token,
		Source:     source,
		Delimiters: []string{},
		Targets:    []*Identifier{},
		Line:       token.Line,
		Column:     token.Column,
	}
}

// Helper function to create SEARCH AST from parsed elements
func newSearchAST(token Token, tableName *Identifier, isSearchAll bool) *SearchStatementAST {
	return &SearchStatementAST{
		Token:       token,
		TableName:   tableName,
		IsSearchAll: isSearchAll,
		WhenClauses: []*SearchWhenAST{},
		Line:        token.Line,
		Column:      token.Column,
	}
}
