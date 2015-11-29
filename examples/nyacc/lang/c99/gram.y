// Copyright (C) 2015 Matthew R. Wette
// 
// This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
// or any later version published by the Free Software Foundation.  See the
// file COPYING included with the this distribution.
%token cpp_stmt
%token _lone_comm
%token _code_comm
%token _chlit
%token _float
%token _fixed
%token cpp_ident
%token _ident
%token _string
%token RETURN
%token BREAK
%token CONTINUE
%token GOTO
%token FOR
%token DO
%token WHILE
%token SWITCH
%token ELSE
%token THEN
%token IF
%token DEFAULT
%token CASE
%token typename
%token ChSeq_46_46_46
%token INLINE
%token RESTRICT
%token VOLATILE
%token CONST
%token ENUM
%token UNION
%token STRUCT
%token _COMPLEX
%token DOUBLE
%token FLOAT
%token CHAR
%token UNSIGNED
%token LONG
%token SIGNED
%token INT
%token IMP
%token SHORT
%token _BOOL
%token VOID
%token TYPEDEF
%token STATIC
%token REGISTER
%token EXTERN
%token AUTO
%token ';'
%token ChSeq_61_124
%token ChSeq_61_94
%token ChSeq_61_38
%token ChSeq_61_62_62
%token ChSeq_61_60_60
%token ChSeq_61_37
%token ChSeq_61_47
%token ChSeq_61_42
%token ChSeq_61_45
%token ChSeq_61_43
%token '='
%token ':'
%token '?'
%token ChSeq_124_124
%token ChSeq_38_38
%token '|'
%token '^'
%token ChSeq_61_33
%token ChSeq_61_61
%token ChSeq_61_62
%token ChSeq_61_60
%token '>'
%token '<'
%token ChSeq_62_62
%token ChSeq_60_60
%token '%'
%token '/'
%token '!'
%token '~'
%token '-'
%token '+'
%token '*'
%token '&'
%token SIZEOF
%token ','
%token '}'
%token '{'
%token ChSeq_45_45
%token ChSeq_43_43
%token ChSeq_62_45
%token '.'
%token ']'
%token '['
%token ')'
%token '('
%token _end
%start translation_unit_proxy
%%
translation_unit_proxy: translation_unit ;
primary_expression: identifier ;
primary_expression: constant ;
primary_expression: string_literal ;
primary_expression: '(' expression ')' ;
postfix_expression: primary_expression ;
postfix_expression: postfix_expression '[' expression ']' ;
postfix_expression: postfix_expression '(' argument_expression_list ')' ;
postfix_expression: postfix_expression '(' ')' ;
postfix_expression: postfix_expression '.' identifier ;
postfix_expression: postfix_expression ChSeq_62_45 identifier ;
postfix_expression: postfix_expression ChSeq_43_43 ;
postfix_expression: postfix_expression ChSeq_45_45 ;
postfix_expression: '(' type_name ')' '{' initializer_list '}' ;
postfix_expression: '(' type_name ')' '{' initializer_list ',' '}' ;
argument_expression_list: assignment_expression ;
argument_expression_list: argument_expression_list ',' assignment_expression ;
unary_expression: postfix_expression ;
unary_expression: ChSeq_43_43 unary_expression ;
unary_expression: ChSeq_45_45 unary_expression ;
unary_expression: unary_operator cast_expression ;
unary_expression: SIZEOF unary_expression ;
unary_expression: SIZEOF '(' type_name ')' ;
unary_operator: '&' ;
unary_operator: '*' ;
unary_operator: '+' ;
unary_operator: '-' ;
unary_operator: '~' ;
unary_operator: '!' ;
cast_expression: unary_expression ;
cast_expression: '(' type_name ')' cast_expression ;
multiplicative_expression: cast_expression ;
multiplicative_expression: multiplicative_expression '*' cast_expression ;
multiplicative_expression: multiplicative_expression '/' cast_expression ;
multiplicative_expression: multiplicative_expression '%' cast_expression ;
additive_expression: multiplicative_expression ;
additive_expression: additive_expression '+' multiplicative_expression ;
additive_expression: additive_expression '-' multiplicative_expression ;
shift_expression: additive_expression ;
shift_expression: shift_expression ChSeq_60_60 additive_expression ;
shift_expression: shift_expression ChSeq_62_62 additive_expression ;
relational_expression: shift_expression ;
relational_expression: relational_expression '<' shift_expression ;
relational_expression: relational_expression '>' shift_expression ;
relational_expression: relational_expression ChSeq_61_60 shift_expression ;
relational_expression: relational_expression ChSeq_61_62 shift_expression ;
equality_expression: relational_expression ;
equality_expression: equality_expression ChSeq_61_61 relational_expression ;
equality_expression: equality_expression ChSeq_61_33 relational_expression ;
bitwise_and_expression: equality_expression ;
bitwise_and_expression: bitwise_and_expression '&' equality_expression ;
bitwise_xor_expression: bitwise_and_expression ;
bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression ;
bitwise_or_expression: bitwise_xor_expression ;
bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression ;
logical_and_expression: bitwise_or_expression ;
logical_and_expression: logical_and_expression ChSeq_38_38 bitwise_or_expression ;
logical_or_expression: logical_and_expression ;
logical_or_expression: logical_or_expression ChSeq_124_124 logical_and_expression ;
conditional_expression: logical_or_expression ;
conditional_expression: logical_or_expression '?' expression ':' conditional_expression ;
assignment_expression: conditional_expression ;
assignment_expression: unary_expression assignment_operator assignment_expression ;
assignment_operator: '=' ;
assignment_operator: ChSeq_61_43 ;
assignment_operator: ChSeq_61_45 ;
assignment_operator: ChSeq_61_42 ;
assignment_operator: ChSeq_61_47 ;
assignment_operator: ChSeq_61_37 ;
assignment_operator: ChSeq_61_60_60 ;
assignment_operator: ChSeq_61_62_62 ;
assignment_operator: ChSeq_61_38 ;
assignment_operator: ChSeq_61_94 ;
assignment_operator: ChSeq_61_124 ;
expression: assignment_expression ;
expression: expression ',' assignment_expression ;
constant_expression: conditional_expression ;
declaration: declaration_specifiers init_declarator_list _P1 ';' opt_code_comment ;
declaration: declaration_specifiers ';' opt_code_comment ;
_P1: %empty ;
declaration_specifiers: storage_class_specifier ;
declaration_specifiers: storage_class_specifier declaration_specifiers ;
declaration_specifiers: type_specifier ;
declaration_specifiers: type_specifier declaration_specifiers ;
declaration_specifiers: type_qualifier ;
declaration_specifiers: type_qualifier declaration_specifiers ;
declaration_specifiers: function_specifier ;
declaration_specifiers: function_specifier declaration_specifiers ;
init_declarator_list: init_declarator ;
init_declarator_list: init_declarator_list ',' init_declarator ;
init_declarator: declarator ;
init_declarator: declarator '=' initializer ;
storage_class_specifier: AUTO ;
storage_class_specifier: EXTERN ;
storage_class_specifier: REGISTER ;
storage_class_specifier: STATIC ;
storage_class_specifier: TYPEDEF ;
type_specifier: VOID ;
type_specifier: fixed_type_specifier ;
type_specifier: float_type_specifier ;
type_specifier: _BOOL ;
type_specifier: complex_type_specifier ;
type_specifier: struct_or_union_specifier ;
type_specifier: enum_specifier ;
type_specifier: typedef_name ;
fixed_type_specifier: SHORT %prec IMP ;
fixed_type_specifier: SHORT INT ;
fixed_type_specifier: SIGNED SHORT %prec IMP ;
fixed_type_specifier: SIGNED SHORT INT ;
fixed_type_specifier: INT ;
fixed_type_specifier: SIGNED %prec IMP ;
fixed_type_specifier: SIGNED INT ;
fixed_type_specifier: LONG %prec IMP ;
fixed_type_specifier: LONG INT ;
fixed_type_specifier: SIGNED LONG %prec IMP ;
fixed_type_specifier: SIGNED LONG INT ;
fixed_type_specifier: LONG LONG %prec IMP ;
fixed_type_specifier: LONG LONG INT ;
fixed_type_specifier: SIGNED LONG LONG %prec IMP ;
fixed_type_specifier: SIGNED LONG LONG INT ;
fixed_type_specifier: UNSIGNED SHORT INT ;
fixed_type_specifier: UNSIGNED SHORT %prec IMP ;
fixed_type_specifier: UNSIGNED INT ;
fixed_type_specifier: UNSIGNED %prec IMP ;
fixed_type_specifier: UNSIGNED LONG INT ;
fixed_type_specifier: UNSIGNED LONG %prec IMP ;
fixed_type_specifier: UNSIGNED LONG LONG INT ;
fixed_type_specifier: UNSIGNED LONG LONG %prec IMP ;
fixed_type_specifier: CHAR ;
fixed_type_specifier: SIGNED CHAR ;
fixed_type_specifier: UNSIGNED CHAR ;
float_type_specifier: FLOAT %prec IMP ;
float_type_specifier: DOUBLE %prec IMP ;
float_type_specifier: LONG DOUBLE ;
complex_type_specifier: _COMPLEX ;
complex_type_specifier: FLOAT _COMPLEX ;
complex_type_specifier: DOUBLE _COMPLEX ;
complex_type_specifier: LONG DOUBLE _COMPLEX ;
struct_or_union_specifier: STRUCT identifier '{' struct_declaration_list '}' ;
struct_or_union_specifier: STRUCT '{' struct_declaration_list '}' ;
struct_or_union_specifier: STRUCT identifier ;
struct_or_union_specifier: UNION identifier '{' struct_declaration_list '}' ;
struct_or_union_specifier: UNION '{' struct_declaration_list '}' ;
struct_or_union_specifier: UNION identifier ;
struct_declaration_list: struct_declaration ;
struct_declaration_list: lone_comment ;
struct_declaration_list: struct_declaration_list struct_declaration ;
struct_declaration_list: struct_declaration_list lone_comment ;
struct_declaration: specifier_qualifier_list struct_declarator_list ';' opt_code_comment ;
specifier_qualifier_list: type_specifier specifier_qualifier_list ;
specifier_qualifier_list: type_specifier ;
specifier_qualifier_list: type_qualifier specifier_qualifier_list ;
specifier_qualifier_list: type_qualifier ;
struct_declarator_list: struct_declarator ;
struct_declarator_list: struct_declarator_list ',' struct_declarator ;
struct_declarator: declarator ;
struct_declarator: declarator ':' constant_expression ;
struct_declarator: ':' constant_expression ;
enum_specifier: ENUM identifier '{' enumerator_list '}' ;
enum_specifier: ENUM identifier '{' enumerator_list ',' '}' ;
enum_specifier: ENUM '{' enumerator_list '}' ;
enum_specifier: ENUM '{' enumerator_list ',' '}' ;
enum_specifier: ENUM identifier ;
enumerator_list: enumerator ;
enumerator_list: enumerator_list ',' enumerator ;
enumerator: identifier ;
enumerator: identifier '=' constant_expression ;
type_qualifier: CONST ;
type_qualifier: VOLATILE ;
type_qualifier: RESTRICT ;
function_specifier: INLINE ;
declarator: pointer direct_declarator ;
declarator: direct_declarator ;
direct_declarator: identifier ;
direct_declarator: '(' declarator ')' ;
direct_declarator: direct_declarator '[' type_qualifier_list assignment_expression ']' ;
direct_declarator: direct_declarator '[' type_qualifier_list ']' ;
direct_declarator: direct_declarator '[' assignment_expression ']' ;
direct_declarator: direct_declarator '[' ']' ;
direct_declarator: direct_declarator '[' STATIC type_qualifier_list assignment_expression ']' ;
direct_declarator: direct_declarator '[' type_qualifier_list STATIC assignment_expression ']' ;
direct_declarator: direct_declarator '[' type_qualifier_list '*' ']' ;
direct_declarator: direct_declarator '[' '*' ']' ;
direct_declarator: direct_declarator '(' parameter_type_list ')' ;
direct_declarator: direct_declarator '(' identifier_list ')' ;
direct_declarator: direct_declarator '(' ')' ;
pointer: '*' type_qualifier_list ;
pointer: '*' ;
pointer: '*' type_qualifier_list pointer ;
pointer: '*' pointer ;
type_qualifier_list: type_qualifier ;
type_qualifier_list: type_qualifier_list type_qualifier ;
parameter_type_list: parameter_list ;
parameter_type_list: parameter_list ',' ChSeq_46_46_46 ;
parameter_list: parameter_declaration ;
parameter_list: parameter_list ',' parameter_declaration ;
parameter_declaration: declaration_specifiers declarator ;
parameter_declaration: declaration_specifiers abstract_declarator ;
parameter_declaration: declaration_specifiers ;
identifier_list: identifier ;
identifier_list: identifier_list ',' identifier ;
type_name: specifier_qualifier_list abstract_declarator ;
type_name: declaration_specifiers ;
abstract_declarator: pointer ;
abstract_declarator: pointer direct_abstract_declarator ;
abstract_declarator: direct_abstract_declarator ;
direct_abstract_declarator: '(' abstract_declarator ')' ;
direct_abstract_declarator: direct_abstract_declarator '[' type_qualifier_list assignment_expression ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' type_qualifier_list ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' assignment_expression ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' STATIC type_qualifier_list ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']' ;
direct_abstract_declarator: '[' type_qualifier_list assignment_expression ']' ;
direct_abstract_declarator: '[' type_qualifier_list ']' ;
direct_abstract_declarator: '[' assignment_expression ']' ;
direct_abstract_declarator: '[' ']' ;
direct_abstract_declarator: '[' STATIC type_qualifier_list assignment_expression ']' ;
direct_abstract_declarator: '[' STATIC type_qualifier_list ']' ;
direct_abstract_declarator: '[' type_qualifier_list STATIC assignment_expression ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' '*' ']' ;
direct_abstract_declarator: '[' '*' ']' ;
direct_abstract_declarator: direct_abstract_declarator '(' parameter_type_list ')' ;
direct_abstract_declarator: direct_abstract_declarator '(' ')' ;
direct_abstract_declarator: '(' parameter_type_list ')' ;
direct_abstract_declarator: '(' ')' ;
typedef_name: typename ;
initializer: assignment_expression ;
initializer: '{' initializer_list '}' ;
initializer: '{' initializer_list ',' '}' ;
initializer_list: designation initializer ;
initializer_list: initializer ;
initializer_list: initializer_list ',' designation initializer ;
initializer_list: initializer_list ',' initializer ;
designation: designator_list '=' ;
designator_list: designator ;
designator_list: designator_list designator ;
designator: '[' constant_expression ']' ;
designator: '.' identifier ;
statement: labeled_statement ;
statement: compound_statement ;
statement: expression_statement ;
statement: selection_statement ;
statement: iteration_statement ;
statement: jump_statement ;
labeled_statement: identifier ':' statement ;
labeled_statement: CASE constant_expression ':' statement ;
labeled_statement: DEFAULT ':' statement ;
compound_statement: '{' block_item_list '}' ;
compound_statement: '{' '}' ;
block_item_list: block_item ;
block_item_list: block_item_list block_item ;
block_item: declaration ;
block_item: statement ;
expression_statement: expression ';' ;
expression_statement: ';' ;
selection_statement: IF '(' expression ')' statement %prec THEN ;
selection_statement: IF '(' expression ')' statement ELSE statement ;
selection_statement: SWITCH '(' expression ')' statement ;
iteration_statement: WHILE '(' expression ')' statement ;
iteration_statement: DO statement WHILE '(' expression ')' ';' ;
iteration_statement: FOR '(' initial_clause expression ';' expression ')' statement ;
iteration_statement: FOR '(' initial_clause expression ';' ')' statement ;
iteration_statement: FOR '(' initial_clause ';' expression ')' statement ;
iteration_statement: FOR '(' initial_clause ';' ')' statement ;
initial_clause: expression ';' ;
initial_clause: ';' ;
initial_clause: declaration ;
jump_statement: GOTO identifier ';' ;
jump_statement: CONTINUE ';' ;
jump_statement: BREAK ';' ;
jump_statement: RETURN expression ';' ;
jump_statement: RETURN ';' ;
translation_unit: external_declaration ;
translation_unit: translation_unit external_declaration ;
external_declaration: function_definition ;
external_declaration: declaration ;
external_declaration: lone_comment ;
external_declaration: cpp_statement ;
external_declaration: EXTERN _string '{' translation_unit '}' ;
function_definition: declaration_specifiers declarator declaration_list compound_statement ;
function_definition: declaration_specifiers declarator compound_statement ;
declaration_list: declaration ;
declaration_list: declaration_list declaration ;
opt_code_comment: %empty ;
opt_code_comment: code_comment ;
identifier: _ident ;
identifier: cpp_ident ;
constant: _fixed ;
constant: _float ;
constant: _chlit ;
string_literal: _string ;
code_comment: _code_comm ;
lone_comment: _lone_comm ;
cpp_statement: cpp_stmt ;

