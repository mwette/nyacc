%token FUNCTION
%token FINALLY
%token CATCH
%token TRY
%token THROW
%token DEFAULT
%token CASE
%token SWITCH
%token WITH
%token RETURN
%token BREAK
%token CONTINUE
%token FOR
%token WHILE
%token DO
%token ELSE
%token IF
%token ';'
%token VAR
%token ChSeq_61_124
%token ChSeq_61_94
%token ChSeq_61_38
%token ChSeq_61_62_62_62
%token ChSeq_61_62_62
%token ChSeq_61_60_60
%token ChSeq_61_45
%token ChSeq_61_43
%token ChSeq_61_37
%token ChSeq_61_47
%token ChSeq_61_42
%token '='
%token '?'
%token ChSeq_124_124
%token ChSeq_38_38
%token '|'
%token '^'
%token '&'
%token ChSeq_61_61_33
%token ChSeq_61_61_61
%token ChSeq_61_33
%token ChSeq_61_61
%token IN
%token INSTANCEOF
%token ChSeq_61_62
%token ChSeq_61_60
%token '>'
%token '<'
%token ChSeq_62_62_62
%token ChSeq_62_62
%token ChSeq_60_60
%token '%'
%token '/'
%token '*'
%token '!'
%token '~'
%token '-'
%token '+'
%token TYPEOF
%token VOID
%token DELETE
%token ChSeq_45_45
%token ChSeq_43_43
%token NEW
%token '.'
%token ':'
%token '}'
%token '{'
%token ','
%token ']'
%token '['
%token ')'
%token '('
%token THIS
%token _ident
%token _string
%token _fl
%token _fx
%token FALSE
%token TRUE
%token NULL
%token _end
%start Program
%%
Literal: NullLiteral ;
Literal: BooleanLiteral ;
Literal: NumericLiteral ;
Literal: StringLiteral ;
NullLiteral: NULL ;
BooleanLiteral: TRUE ;
BooleanLiteral: FALSE ;
NumericLiteral: _fx ;
NumericLiteral: _fl ;
StringLiteral: _string ;
Identifier: _ident ;
PrimaryExpression: THIS ;
PrimaryExpression: Identifier ;
PrimaryExpression: Literal ;
PrimaryExpression: ArrayLiteral ;
PrimaryExpression: ObjectLiteral ;
PrimaryExpression: '(' Expression ')' ;
ArrayLiteral: '[' Elision ']' ;
ArrayLiteral: '[' ']' ;
ArrayLiteral: '[' ElementList ',' Elision ']' ;
ArrayLiteral: '[' ElementList ',' ']' ;
ElementList: Elision AssignmentExpression ;
ElementList: AssignmentExpression ;
ElementList: ElementList ',' Elision AssignmentExpression ;
ElementList: ElementList ',' AssignmentExpression ;
Elision: ',' ;
Elision: Elision ',' ;
ObjectLiteral: '{' '}' ;
ObjectLiteral: '{' PropertyNameAndValueList '}' ;
PropertyNameAndValueList: PropertyName ':' AssignmentExpression ;
PropertyNameAndValueList: PropertyNameAndValueList ',' PropertyName ':' AssignmentExpression ;
PropertyName: Identifier ;
PropertyName: StringLiteral ;
PropertyName: NumericLiteral ;
MemberExpression: PrimaryExpression ;
MemberExpression: MemberExpression '[' Expression ']' ;
MemberExpression: MemberExpression '.' Identifier ;
MemberExpression: NEW MemberExpression Arguments ;
NewExpression: MemberExpression ;
NewExpression: NEW NewExpression ;
CallExpression: MemberExpression Arguments ;
CallExpression: CallExpression Arguments ;
CallExpression: CallExpression '[' Expression ']' ;
CallExpression: CallExpression '.' Identifier ;
Arguments: '(' ')' ;
Arguments: '(' ArgumentList ')' ;
ArgumentList: AssignmentExpression ;
ArgumentList: ArgumentList ',' AssignmentExpression ;
LeftHandSideExpression: NewExpression ;
LeftHandSideExpression: CallExpression ;
PostfixExpression: LeftHandSideExpression ;
PostfixExpression: LeftHandSideExpression _P1 ChSeq_43_43 ;
PostfixExpression: LeftHandSideExpression _P2 ChSeq_45_45 ;
_P1: %empty ;
_P2: %empty ;
UnaryExpression: PostfixExpression ;
UnaryExpression: DELETE UnaryExpression ;
UnaryExpression: VOID UnaryExpression ;
UnaryExpression: TYPEOF UnaryExpression ;
UnaryExpression: ChSeq_43_43 UnaryExpression ;
UnaryExpression: ChSeq_45_45 UnaryExpression ;
UnaryExpression: '+' UnaryExpression ;
UnaryExpression: '-' UnaryExpression ;
UnaryExpression: '~' UnaryExpression ;
UnaryExpression: '!' UnaryExpression ;
MultiplicativeExpression: UnaryExpression ;
MultiplicativeExpression: MultiplicativeExpression '*' UnaryExpression ;
MultiplicativeExpression: MultiplicativeExpression '/' UnaryExpression ;
MultiplicativeExpression: MultiplicativeExpression '%' UnaryExpression ;
AdditiveExpression: MultiplicativeExpression ;
AdditiveExpression: AdditiveExpression '+' MultiplicativeExpression ;
AdditiveExpression: AdditiveExpression '-' MultiplicativeExpression ;
ShiftExpression: AdditiveExpression ;
ShiftExpression: ShiftExpression ChSeq_60_60 AdditiveExpression ;
ShiftExpression: ShiftExpression ChSeq_62_62 AdditiveExpression ;
ShiftExpression: ShiftExpression ChSeq_62_62_62 AdditiveExpression ;
RelationalExpression: ShiftExpression ;
RelationalExpression: RelationalExpression '<' ShiftExpression ;
RelationalExpression: RelationalExpression '>' ShiftExpression ;
RelationalExpression: RelationalExpression ChSeq_61_60 ShiftExpression ;
RelationalExpression: RelationalExpression ChSeq_61_62 ShiftExpression ;
RelationalExpression: RelationalExpression INSTANCEOF ShiftExpression ;
RelationalExpression: RelationalExpression IN ShiftExpression ;
RelationalExpressionNoIn: ShiftExpression ;
RelationalExpressionNoIn: RelationalExpressionNoIn '<' ShiftExpression ;
RelationalExpressionNoIn: RelationalExpressionNoIn '>' ShiftExpression ;
RelationalExpressionNoIn: RelationalExpressionNoIn ChSeq_61_60 ShiftExpression ;
RelationalExpressionNoIn: RelationalExpressionNoIn ChSeq_61_62 ShiftExpression ;
RelationalExpressionNoIn: RelationalExpressionNoIn INSTANCEOF ShiftExpression ;
EqualityExpression: RelationalExpression ;
EqualityExpression: EqualityExpression ChSeq_61_61 RelationalExpression ;
EqualityExpression: EqualityExpression ChSeq_61_33 RelationalExpression ;
EqualityExpression: EqualityExpression ChSeq_61_61_61 RelationalExpression ;
EqualityExpression: EqualityExpression ChSeq_61_61_33 RelationalExpression ;
EqualityExpressionNoIn: RelationalExpressionNoIn ;
EqualityExpressionNoIn: EqualityExpressionNoIn ChSeq_61_61 RelationalExpressionNoIn ;
EqualityExpressionNoIn: EqualityExpressionNoIn ChSeq_61_33 RelationalExpressionNoIn ;
EqualityExpressionNoIn: EqualityExpressionNoIn ChSeq_61_61_61 RelationalExpressionNoIn ;
EqualityExpressionNoIn: EqualityExpressionNoIn ChSeq_61_61_33 RelationalExpressionNoIn ;
BitwiseANDExpression: EqualityExpression ;
BitwiseANDExpression: BitwiseANDExpression '&' EqualityExpression ;
BitwiseANDExpressionNoIn: EqualityExpressionNoIn ;
BitwiseANDExpressionNoIn: BitwiseANDExpressionNoIn '&' EqualityExpressionNoIn ;
BitwiseXORExpression: BitwiseANDExpression ;
BitwiseXORExpression: BitwiseXORExpression '^' BitwiseANDExpression ;
BitwiseXORExpressionNoIn: BitwiseANDExpressionNoIn ;
BitwiseXORExpressionNoIn: BitwiseXORExpressionNoIn '^' BitwiseANDExpressionNoIn ;
BitwiseORExpression: BitwiseXORExpression ;
BitwiseORExpression: BitwiseORExpression '|' BitwiseXORExpression ;
BitwiseORExpressionNoIn: BitwiseXORExpressionNoIn ;
BitwiseORExpressionNoIn: BitwiseORExpressionNoIn '|' BitwiseXORExpressionNoIn ;
LogicalANDExpression: BitwiseORExpression ;
LogicalANDExpression: LogicalANDExpression ChSeq_38_38 BitwiseORExpression ;
LogicalANDExpressionNoIn: BitwiseORExpressionNoIn ;
LogicalANDExpressionNoIn: LogicalANDExpressionNoIn ChSeq_38_38 BitwiseORExpressionNoIn ;
LogicalORExpression: LogicalANDExpression ;
LogicalORExpression: LogicalORExpression ChSeq_124_124 LogicalANDExpression ;
LogicalORExpressionNoIn: LogicalANDExpressionNoIn ;
LogicalORExpressionNoIn: LogicalORExpressionNoIn ChSeq_124_124 LogicalANDExpressionNoIn ;
ConditionalExpression: LogicalORExpression ;
ConditionalExpression: LogicalORExpression '?' AssignmentExpression ':' AssignmentExpression ;
ConditionalExpressionNoIn: LogicalORExpressionNoIn ;
ConditionalExpressionNoIn: LogicalORExpressionNoIn '?' AssignmentExpressionNoIn ':' AssignmentExpressionNoIn ;
AssignmentExpression: ConditionalExpression ;
AssignmentExpression: LeftHandSideExpression AssignmentOperator AssignmentExpression ;
AssignmentExpressionNoIn: ConditionalExpressionNoIn ;
AssignmentExpressionNoIn: LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn ;
AssignmentOperator: '=' ;
AssignmentOperator: ChSeq_61_42 ;
AssignmentOperator: ChSeq_61_47 ;
AssignmentOperator: ChSeq_61_37 ;
AssignmentOperator: ChSeq_61_43 ;
AssignmentOperator: ChSeq_61_45 ;
AssignmentOperator: ChSeq_61_60_60 ;
AssignmentOperator: ChSeq_61_62_62 ;
AssignmentOperator: ChSeq_61_62_62_62 ;
AssignmentOperator: ChSeq_61_38 ;
AssignmentOperator: ChSeq_61_94 ;
AssignmentOperator: ChSeq_61_124 ;
Expression: AssignmentExpression ;
Expression: Expression ',' AssignmentExpression ;
ExpressionNoIn: AssignmentExpressionNoIn ;
ExpressionNoIn: ExpressionNoIn ',' AssignmentExpressionNoIn ;
Statement: VariableStatement ;
Statement: EmptyStatement ;
Statement: ExpressionStatement ;
Statement: IfStatement ;
Statement: IterationStatement ;
Statement: ContinueStatement ;
Statement: BreakStatement ;
Statement: ReturnStatement ;
Statement: WithStatement ;
Statement: LabelledStatement ;
Statement: SwitchStatement ;
Statement: ThrowStatement ;
Statement: TryStatement ;
Block: '{' StatementList '}' ;
Block: '{' '}' ;
StatementList: Statement ;
StatementList: StatementList Statement ;
VariableStatement: VAR VariableDeclarationList ';' ;
VariableDeclarationList: VariableDeclaration ;
VariableDeclarationList: VariableDeclarationList ',' VariableDeclaration ;
VariableDeclarationListNoIn: VariableDeclarationNoIn ;
VariableDeclarationListNoIn: VariableDeclarationListNoIn ',' VariableDeclarationNoIn ;
VariableDeclaration: Identifier Initializer ;
VariableDeclaration: Identifier ;
VariableDeclarationNoIn: Identifier InitializerNoIn ;
VariableDeclarationNoIn: Identifier ;
Initializer: '=' AssignmentExpression ;
InitializerNoIn: '=' AssignmentExpressionNoIn ;
EmptyStatement: ';' ;
ExpressionStatement: Expression ';' ;
IfStatement: IF '(' Expression ')' Statement ELSE Statement ;
IfStatement: IF '(' Expression ')' Statement ;
IterationStatement: DO Statement WHILE '(' Expression ')' ';' ;
IterationStatement: WHILE '(' Expression ')' Statement ;
IterationStatement: FOR '(' OptExprStmtNoIn OptExprStmt OptExprClose Statement ;
IterationStatement: FOR '(' VAR VariableDeclarationListNoIn ';' OptExprStmt OptExprClose Statement ;
IterationStatement: FOR '(' LeftHandSideExpression IN Expression ')' Statement ;
IterationStatement: FOR '(' VAR VariableDeclarationNoIn IN Expression ')' Statement ;
OptExprStmtNoIn: ':' ;
OptExprStmtNoIn: ExpressionNoIn ';' ;
OptExprStmt: ';' ;
OptExprStmt: Expression ';' ;
OptExprClose: ';' ;
OptExprClose: Expression ')' ;
ContinueStatement: CONTINUE _P3 Identifier ';' ;
ContinueStatement: CONTINUE ';' ;
_P3: %empty ;
BreakStatement: BREAK _P4 Identifier ';' ;
BreakStatement: BREAK ';' ;
_P4: %empty ;
ReturnStatement: RETURN _P5 Expression ';' ;
ReturnStatement: RETURN ';' ;
_P5: %empty ;
WithStatement: WITH '(' Expression ')' Statement ;
SwitchStatement: SWITCH '(' Expression ')' CaseBlock ;
CaseBlock: '{' CaseClauses '}' ;
CaseBlock: '{' '}' ;
CaseBlock: '{' CaseClauses DefaultClause CaseClauses '}' ;
CaseBlock: '{' CaseClauses DefaultClause '}' ;
CaseBlock: '{' DefaultClause CaseClauses '}' ;
CaseBlock: '{' DefaultClause '}' ;
CaseClauses: CaseClause ;
CaseClauses: CaseClauses CaseClause ;
CaseClause: CASE Expression ':' StatementList ;
CaseClause: CASE Expression ':' ;
DefaultClause: DEFAULT ':' StatementList ;
DefaultClause: DEFAULT ':' ;
LabelledStatement: Identifier ':' Statement ;
ThrowStatement: THROW _P6 Expression ';' ;
_P6: %empty ;
TryStatement: TRY Block Catch ;
TryStatement: TRY Block Finally ;
TryStatement: TRY Block Catch Finally ;
Catch: CATCH '(' Identifier ')' Block ;
Finally: FINALLY Block ;
FunctionDeclaration: FUNCTION Identifier '(' FormalParameterList ')' '{' FunctionBody '}' ;
FunctionDeclaration: FUNCTION Identifier '(' ')' '{' FunctionBody '}' ;
FunctionExpression: FUNCTION Identifier '(' FormalParameterList ')' '{' FunctionBody '}' ;
FunctionExpression: FUNCTION '(' FormalParameterList ')' '{' FunctionBody '}' ;
FunctionExpression: FUNCTION Identifier '(' ')' '{' FunctionBody '}' ;
FunctionExpression: FUNCTION '(' ')' '{' FunctionBody '}' ;
FormalParameterList: Identifier ;
FormalParameterList: FormalParameterList ',' Identifier ;
FunctionBody: SourceElements ;
Program: SourceElements ;
SourceElements: SourceElement ;
SourceElements: SourceElements SourceElement ;
SourceElement: Statement ;
SourceElement: FunctionDeclaration ;

