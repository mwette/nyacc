// Copyright (C) 2015 Matthew R. Wette
// 
// This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
// or any later version published by the Free Software Foundation.  See the
// file COPYING included with the this distribution.
%token cpp_stmt
%token _lone_comm
%token _code_comm
%token _string
%token _ch
%token _fl
%token _fx
%token cpp_ident
%token _ident
%token GOTO
%token RETURN
%token CONTINUE
%token BREAK
%token DEFAULT
%token CASE
%token SWITCH
%token FOR
%token DO
%token WHILE
%token ELSE
%token IF
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
%token '?'
%token ChSeq_38_38
%token ChSeq_124_124
%token '^'
%token '|'
%token ChSeq_61_33
%token ChSeq_61_61
%token ChSeq_61_62
%token '>'
%token ChSeq_61_60
%token '<'
%token ChSeq_62_62
%token ChSeq_60_60
%token '%'
%token '/'
%token '&'
%token '~'
%token '!'
%token '+'
%token '-'
%token SIZEOF
%token ChSeq_45_45
%token ChSeq_43_43
%token ChSeq_62_45
%token typename
%token VOID
%token UNION
%token ':'
%token STRUCT
%token ENUM
%token _COMPLEX
%token DOUBLE
%token FLOAT
%token _BOOL
%token CHAR
%token UNSIGNED
%token LONG
%token SIGNED
%token INT
%token SHORT
%token '.'
%token '}'
%token '{'
%token ChSeq_46_46_46
%token ']'
%token '['
%token '*'
%token ')'
%token '('
%token RESTRICT
%token VOLATILE
%token CONST
%token INLINE
%token TYPEDEF
%token STATIC
%token REGISTER
%token EXTERN
%token AUTO
%token '='
%token ','
%token ';'
%token _end
%start translation_unit_proxy
%%
translation_unit_proxy: translation_unit ;
declaration: declaration_specifiers initialized_declarator_list _P1 ';' opt_code_comment ;
_P1: %empty ;
declaration_specifiers: storage_class_specifier ;
declaration_specifiers: storage_class_specifier declaration_specifiers ;
declaration_specifiers: type_specifier ;
declaration_specifiers: type_specifier declaration_specifiers ;
declaration_specifiers: type_qualifier ;
declaration_specifiers: type_qualifier declaration_specifiers ;
declaration_specifiers: function_specifier ;
declaration_specifiers: function_specifier declaration_specifiers ;
initialized_declarator_list: initialized_declarator ;
initialized_declarator_list: initialized_declarator_list ',' initialized_declarator ;
initialized_declarator: declarator ;
initialized_declarator: declarator '=' initializer ;
storage_class_specifier: AUTO ;
storage_class_specifier: EXTERN ;
storage_class_specifier: REGISTER ;
storage_class_specifier: STATIC ;
storage_class_specifier: TYPEDEF ;
function_specifier: INLINE ;
type_specifier: enumeration_type_specifier ;
type_specifier: floating_point_type_specifier ;
type_specifier: integer_type_specifier ;
type_specifier: structure_type_specifier ;
type_specifier: typedef_name ;
type_specifier: union_type_specifier ;
type_specifier: void_type_specifier ;
type_qualifier: CONST ;
type_qualifier: VOLATILE ;
type_qualifier: RESTRICT ;
declarator: pointer_declarator ;
declarator: direct_declarator ;
direct_declarator: simple_declarator ;
direct_declarator: '(' declarator ')' ;
direct_declarator: function_declarator ;
direct_declarator: array_declarator ;
simple_declarator: identifier ;
pointer_declarator: pointer direct_declarator ;
pointer: '*' type_qualifier_list ;
pointer: '*' ;
pointer: '*' type_qualifier_list pointer ;
pointer: '*' pointer ;
type_qualifier_list: type_qualifier ;
type_qualifier_list: type_qualifier_list type_qualifier ;
array_declarator: direct_declarator '[' array_qualifier_list array_size_expression ']' ;
array_declarator: direct_declarator '[' array_qualifier_list ']' ;
array_declarator: direct_declarator '[' array_size_expression ']' ;
array_declarator: direct_declarator '[' ']' ;
array_declarator: direct_declarator '[' array_qualifier_list '*' ']' ;
array_declarator: direct_declarator '[' '*' ']' ;
array_qualifier_list: array_qualifier ;
array_qualifier_list: array_qualifier_list array_qualifier ;
array_qualifier: STATIC ;
array_qualifier: RESTRICT ;
array_qualifier: CONST ;
array_qualifier: VOLATILE ;
array_size_expression: assignment_expression ;
function_declarator: direct_declarator '(' parameter_type_list ')' ;
function_declarator: direct_declarator '(' identifier_list ')' ;
function_declarator: direct_declarator '(' ')' ;
parameter_type_list: parameter_list ;
parameter_type_list: parameter_list ',' ChSeq_46_46_46 ;
parameter_list: parameter_declaration ;
parameter_list: parameter_list ',' parameter_declaration ;
parameter_declaration: declaration_specifiers declarator ;
parameter_declaration: declaration_specifiers abstract_declarator ;
parameter_declaration: declaration_specifiers ;
identifier_list: identifier ;
identifier_list: identifier_list ',' identifier ;
initializer: assignment_expression ;
initializer: '{' initializer_list ',' '}' ;
initializer: '{' initializer_list '}' ;
initializer_list: initializer ;
initializer_list: initializer_list ',' initializer ;
initializer_list: designation initializer ;
initializer_list: initializer_list ',' designation initializer ;
designation: designator_list '=' ;
designator_list: designator ;
designator_list: designator_list designator ;
designator: '[' constant_expression ']' ;
designator: '.' identifier ;
integer_type_specifier: signed_type_specifier ;
integer_type_specifier: unsigned_type_specifier ;
integer_type_specifier: character_type_specifier ;
integer_type_specifier: bool_type_specifier ;
signed_type_specifier: SHORT ;
signed_type_specifier: INT ;
signed_type_specifier: SIGNED ;
signed_type_specifier: LONG ;
unsigned_type_specifier: UNSIGNED ;
character_type_specifier: CHAR ;
bool_type_specifier: _BOOL ;
floating_point_type_specifier: FLOAT ;
floating_point_type_specifier: DOUBLE ;
floating_point_type_specifier: complex_type_specifier ;
complex_type_specifier: _COMPLEX ;
enumeration_type_specifier: enumeration_type_definition ;
enumeration_type_specifier: enumeration_type_reference ;
enumeration_type_definition: ENUM enumeration_tag '{' enumeration_definition_list '}' ;
enumeration_type_definition: ENUM '{' enumeration_definition_list '}' ;
enumeration_type_definition: ENUM enumeration_tag '{' enumeration_definition_list ',' '}' ;
enumeration_type_definition: ENUM '{' enumeration_definition_list ',' '}' ;
enumeration_type_reference: ENUM enumeration_tag ;
enumeration_tag: identifier ;
enumeration_definition_list: enumeration_constant_definition ;
enumeration_definition_list: enumeration_definition_list ',' enumeration_constant_definition ;
enumeration_constant_definition: enumeration_constant ;
enumeration_constant_definition: enumeration_constant '=' constant_expression ;
enumeration_constant: identifier ;
structure_type_specifier: structure_type_definition ;
structure_type_specifier: structure_type_reference ;
structure_type_definition: STRUCT structure_tag '{' field_list '}' ;
structure_type_definition: STRUCT '{' field_list '}' ;
structure_type_reference: STRUCT structure_tag ;
structure_tag: identifier ;
field_list: component_declaration ;
field_list: field_list component_declaration ;
field_list: field_list lone_comment ;
component_declaration: type_specifier component_declarator_list ';' opt_code_comment ;
component_declarator_list: component_declarator ;
component_declarator_list: component_declarator_list ',' component_declarator ;
component_declarator: simple_component ;
component_declarator: bit_field ;
simple_component: declarator ;
bit_field: declarator ':' width ;
bit_field: ':' width ;
width: constant_expression ;
union_type_specifier: union_type_definition ;
union_type_specifier: union_type_reference ;
union_type_definition: UNION union_tag '{' field_list '}' ;
union_type_definition: UNION '{' field_list '}' ;
union_type_reference: UNION union_tag ;
union_tag: identifier ;
void_type_specifier: VOID ;
typedef_name: typename ;
type_name: declaration_specifiers abstract_declarator ;
type_name: declaration_specifiers ;
abstract_declarator: pointer ;
abstract_declarator: pointer direct_abstract_declarator ;
abstract_declarator: direct_abstract_declarator ;
direct_abstract_declarator: '(' abstract_declarator ')' ;
direct_abstract_declarator: direct_abstract_declarator '[' ']' ;
direct_abstract_declarator: '[' ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' expression ']' ;
direct_abstract_declarator: '[' expression ']' ;
direct_abstract_declarator: direct_abstract_declarator '[' '*' ']' ;
direct_abstract_declarator: '[' '*' ']' ;
direct_abstract_declarator: direct_abstract_declarator '(' parameter_type_list ')' ;
direct_abstract_declarator: direct_abstract_declarator '(' ')' ;
direct_abstract_declarator: '(' parameter_type_list ')' ;
direct_abstract_declarator: '(' ')' ;
primary_expression: identifier ;
primary_expression: constant ;
primary_expression: parenthesized_expression ;
parenthesized_expression: '(' expression ')' ;
postfix_expression: primary_expression ;
postfix_expression: subscript_expression ;
postfix_expression: component_selection_expression ;
postfix_expression: function_call ;
postfix_expression: postincrement_expression ;
postfix_expression: postdecrement_expression ;
postfix_expression: compound_literal ;
subscript_expression: postfix_expression '[' expression ']' ;
component_selection_expression: direct_component_selection ;
component_selection_expression: indirect_component_selection ;
direct_component_selection: postfix_expression '.' identifier ;
indirect_component_selection: postfix_expression ChSeq_62_45 identifier ;
function_call: postfix_expression '(' expression_list ')' ;
function_call: postfix_expression '(' ')' ;
expression_list: assignment_expression ;
expression_list: expression_list ',' assignment_expression ;
postincrement_expression: postfix_expression ChSeq_43_43 ;
postdecrement_expression: postfix_expression ChSeq_45_45 ;
compound_literal: '(' type_name ')' '{' initializer_list '}' ;
compound_literal: '(' type_name ')' '{' initializer_list ',' '}' ;
cast_expression: unary_expression ;
cast_expression: '(' type_name ')' cast_expression ;
unary_expression: postfix_expression ;
unary_expression: sizeof_expression ;
unary_expression: unary_minus_expression ;
unary_expression: unary_plus_expression ;
unary_expression: logical_negation_expression ;
unary_expression: bitwise_negation_expression ;
unary_expression: address_expression ;
unary_expression: indirection_expression ;
unary_expression: preincrement_expression ;
unary_expression: predecrement_expression ;
sizeof_expression: SIZEOF '(' type_name ')' ;
sizeof_expression: SIZEOF unary_expression ;
unary_minus_expression: '-' cast_expression ;
unary_plus_expression: '+' cast_expression ;
logical_negation_expression: '!' cast_expression ;
bitwise_negation_expression: '~' cast_expression ;
address_expression: '&' cast_expression ;
indirection_expression: '*' cast_expression ;
preincrement_expression: ChSeq_43_43 unary_expression ;
predecrement_expression: ChSeq_45_45 unary_expression ;
multiplicative_expression: cast_expression ;
multiplicative_expression: multiplicative_expression mult_op cast_expression ;
mult_op: '*' ;
mult_op: '/' ;
mult_op: '%' ;
additive_expression: multiplicative_expression ;
additive_expression: additive_expression add_op multiplicative_expression ;
add_op: '+' ;
add_op: '-' ;
shift_expression: additive_expression ;
shift_expression: shift_expression shift_op additive_expression ;
shift_op: ChSeq_60_60 ;
shift_op: ChSeq_62_62 ;
relational_expression: shift_expression ;
relational_expression: relational_expression relational_op shift_expression ;
relational_op: '<' ;
relational_op: ChSeq_61_60 ;
relational_op: '>' ;
relational_op: ChSeq_61_62 ;
equality_expression: relational_expression ;
equality_expression: equality_expression equality_op relational_expression ;
equality_op: ChSeq_61_61 ;
equality_op: ChSeq_61_33 ;
bitwise_or_expression: bitwise_xor_expression ;
bitwise_or_expression: bitwise_or_expression '|' bitwise_xor_expression ;
bitwise_xor_expression: bitwise_and_expression ;
bitwise_xor_expression: bitwise_xor_expression '^' bitwise_and_expression ;
bitwise_and_expression: equality_expression ;
bitwise_and_expression: bitwise_and_expression '&' equality_expression ;
logical_or_expression: logical_and_expression ;
logical_or_expression: logical_or_expression ChSeq_124_124 logical_and_expression ;
logical_and_expression: bitwise_or_expression ;
logical_and_expression: logical_and_expression ChSeq_38_38 bitwise_or_expression ;
conditional_expression: logical_or_expression ;
conditional_expression: logical_or_expression '?' expression ':' conditional_expression ;
assignment_expression: conditional_expression ;
assignment_expression: unary_expression assignment_op assignment_expression ;
assignment_op: '=' ;
assignment_op: ChSeq_61_43 ;
assignment_op: ChSeq_61_45 ;
assignment_op: ChSeq_61_42 ;
assignment_op: ChSeq_61_47 ;
assignment_op: ChSeq_61_37 ;
assignment_op: ChSeq_61_60_60 ;
assignment_op: ChSeq_61_62_62 ;
assignment_op: ChSeq_61_38 ;
assignment_op: ChSeq_61_94 ;
assignment_op: ChSeq_61_124 ;
comma_expression: assignment_expression ;
comma_expression: comma_expression ',' assignment_expression ;
expression: comma_expression ;
constant_expression: conditional_expression ;
statement: expression_statement ;
statement: labeled_statement ;
statement: compound_statement ;
statement: conditional_statement ;
statement: iterative_statement ;
statement: switch_statement ;
statement: break_statement ;
statement: continue_statement ;
statement: return_statement ;
statement: goto_statement ;
statement: null_statement ;
expression_statement: expression ';' ;
labeled_statement: label ':' statement ;
label: named_label ;
label: case_label ;
label: default_label ;
compound_statement: '{' declaration_or_statement_list '}' ;
compound_statement: '{' '}' ;
declaration_or_statement_list: declaration_or_statement ;
declaration_or_statement_list: declaration_or_statement_list declaration_or_statement ;
declaration_or_statement: declaration ;
declaration_or_statement: statement ;
conditional_statement: if_statement ;
conditional_statement: if_else_statement ;
if_statement: IF '(' expression ')' statement ;
if_else_statement: IF '(' expression ')' statement ELSE statement ;
iterative_statement: while_statement ;
iterative_statement: do_statement ;
iterative_statement: for_statement ;
while_statement: WHILE '(' expression ')' statement ;
do_statement: DO statement WHILE '(' expression ')' ';' ;
for_statement: FOR for_expressions statement ;
for_expressions: '(' initial_clause expression ';' expression ')' ;
for_expressions: '(' initial_clause expression ';' ')' ;
for_expressions: '(' initial_clause ';' expression ')' ;
for_expressions: '(' initial_clause ';' ')' ;
initial_clause: expression ';' ;
initial_clause: ';' ;
initial_clause: declaration ;
switch_statement: SWITCH '(' expression ')' statement ;
case_label: CASE constant_expression ;
default_label: DEFAULT ;
break_statement: BREAK ';' ;
continue_statement: CONTINUE ';' ;
return_statement: RETURN expression ';' ;
return_statement: RETURN ';' ;
goto_statement: GOTO named_label ';' ;
named_label: identifier ;
null_statement: ';' ;
translation_unit: top_level_declaration ;
translation_unit: translation_unit top_level_declaration ;
top_level_declaration: declaration ;
top_level_declaration: function_definition ;
top_level_declaration: lone_comment ;
top_level_declaration: cpp_statement ;
function_definition: function_def_specifier compound_statement ;
function_def_specifier: declaration_specifiers declarator declaration_list ;
function_def_specifier: declaration_specifiers declarator ;
function_def_specifier: declarator ;
declaration_list: declaration ;
declaration_list: declaration_list declaration ;
opt_code_comment: %empty ;
opt_code_comment: code_comment ;
identifier: _ident ;
identifier: cpp_ident ;
constant: _fx ;
constant: _fl ;
constant: _ch ;
constant: _string ;
code_comment: _code_comm ;
lone_comment: _lone_comm ;
cpp_statement: cpp_stmt ;

