// Copyright (C) 2015 Matthew R. Wette
// 
// This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
// or any later version published by the Free Software Foundation.  See the
// file COPYING included with the this distribution.
%token _string
%token _ident
%token _fl
%token _fx
%token ANNOTATION
%token ']'
%token '['
%token TRUE
%token FALSE
%token ChSeq_94_46
%token '^'
%token ChSeq_47_46
%token ChSeq_42_46
%token '/'
%token ChSeq_45_46
%token ChSeq_43_46
%token '-'
%token '+'
%token ChSeq_62_60
%token ChSeq_61_61
%token ChSeq_61_62
%token '>'
%token ChSeq_61_60
%token '<'
%token NOT
%token AND
%token OR
%token CONNECT
%token ELSEWHEN
%token WHEN
%token WHILE
%token IN
%token LOOP
%token FOR
%token ELSE
%token ELSEIF
%token THEN
%token RETURN
%token BREAK
%token ALGORITHM
%token EQUATION
%token INITIAL
%token EACH
%token ChSeq_61_58
%token IF
%token OUTPUT
%token INPUT
%token CONSTANT
%token PARAMETER
%token DISCRETE
%token STREAM
%token FLOW
%token CONSTRAINEDBY
%token '}'
%token '{'
%token '*'
%token '.'
%token IMPORT
%token REPLACEABLE
%token OUTER
%token INNER
%token REDECLARE
%token EXTERNAL
%token PROTECTED
%token PUBLIC
%token ','
%token DER
%token ':'
%token ')'
%token '('
%token ENUMERATION
%token '='
%token EXTENDS
%token END
%token PURE
%token FUNCTION
%token IMPURE
%token PACKAGE
%token TYPE
%token CONNECTOR
%token EXPANDABLE
%token BLOCK
%token RECORD
%token OPERATOR
%token MODEL
%token CLASS
%token PARTIAL
%token ENCAPSULATED
%token FINAL
%token ';'
%token WITHIN
%token _end
%start stored_definition
%%
stored_definition: %empty ;
stored_definition: stored_definition_1 stored_definition_2 ;
stored_definition: stored_definition_2 ;
stored_definition_1: WITHIN name ';' ;
stored_definition_1: WITHIN ';' ;
stored_definition_2: FINAL class_definition ';' ;
stored_definition_2: class_definition ';' ;
stored_definition_2: stored_definition_2 FINAL class_definition ';' ;
stored_definition_2: stored_definition_2 class_definition ';' ;
class_definition: ENCAPSULATED class_prefixes class_specifier ;
class_definition: class_prefixes class_specifier ;
class_prefixes: PARTIAL class_prefixes_1 ;
class_prefixes: class_prefixes_1 ;
class_prefixes_1: CLASS ;
class_prefixes_1: MODEL ;
class_prefixes_1: OPERATOR RECORD ;
class_prefixes_1: RECORD ;
class_prefixes_1: BLOCK ;
class_prefixes_1: EXPANDABLE CONNECTOR ;
class_prefixes_1: CONNECTOR ;
class_prefixes_1: TYPE ;
class_prefixes_1: PACKAGE ;
class_prefixes_1: IMPURE OPERATOR FUNCTION ;
class_prefixes_1: PURE OPERATOR FUNCTION ;
class_prefixes_1: IMPURE FUNCTION ;
class_prefixes_1: PURE FUNCTION ;
class_prefixes_1: OPERATOR FUNCTION ;
class_prefixes_1: FUNCTION ;
class_prefixes_1: OPERATOR ;
class_specifier: long_class_specifier ;
class_specifier: short_class_specifier ;
class_specifier: der_class_specifier ;
long_class_specifier: ident string_comment composition END ident ;
long_class_specifier: EXTENDS ident class_modification string_comment composition END ident ;
long_class_specifier: EXTENDS ident string_comment composition END ident ;
short_class_specifier: ident '=' base_prefix name array_subscripts class_modification comment ;
short_class_specifier: ident '=' base_prefix name array_subscripts comment ;
short_class_specifier: ident '=' base_prefix name class_modification comment ;
short_class_specifier: ident '=' base_prefix name comment ;
short_class_specifier: ident '=' ENUMERATION '(' filler_1 ')' comment ;
filler_1: %empty ;
filler_1: enum_list ;
filler_1: ':' ;
der_class_specifier: ident '=' DER '(' name ',' der_class_specifier_1 ')' comment ;
der_class_specifier_1: ident ;
der_class_specifier_1: der_class_specifier_1 ';' ident ;
base_prefix: %empty ;
base_prefix: type_prefix ;
enum_list: enumeration_literal ;
enum_list: enum_list ',' enumeration_literal ;
enumeration_literal: ident comment ;
composition: element_list composition_1_list external_part opt_annotation ;
composition: element_list composition_1_list opt_annotation ;
composition: element_list external_part opt_annotation ;
composition: element_list opt_annotation ;
composition_1_list: composition_1 ;
composition_1_list: composition_1_list composition_1 ;
composition_1: PUBLIC ;
composition_1: PUBLIC element_list ;
composition_1: PROTECTED ;
composition_1: PROTECTED element_list ;
composition_1: equation_section ;
composition_1: algorithm_section ;
external_part: EXTERNAL language_specification external_function_call annotation ';' ;
external_part: EXTERNAL language_specification external_function_call ';' ;
external_part: EXTERNAL language_specification annotation ';' ;
external_part: EXTERNAL external_function_call annotation ';' ;
external_part: EXTERNAL language_specification ';' ;
external_part: EXTERNAL external_function_call ';' ;
external_part: EXTERNAL annotation ';' ;
external_part: EXTERNAL ';' ;
language_specification: string ;
external_function_call: component_reference '=' ident '(' expression_list ')' ;
external_function_call: component_reference '=' ident '(' ')' ;
external_function_call: ident '(' expression_list ')' ;
external_function_call: ident '(' ')' ;
element_list: element ';' ;
element_list: element_list element ';' ;
element: import_clause ;
element: extends_clause ;
element: REDECLARE _P1 _P2 _P3 element_1 ;
element: FINAL _P4 _P5 element_1 ;
element: INNER _P6 element_1 ;
element: OUTER element_1 ;
element: element_1 ;
_P1: %empty ;
_P1: FINAL ;
_P2: %empty ;
_P2: INNER ;
_P3: %empty ;
_P3: OUTER ;
_P4: %empty ;
_P4: INNER ;
_P5: %empty ;
_P5: OUTER ;
_P6: %empty ;
_P6: OUTER ;
element_1: class_definition ;
element_1: component_clause ;
element_1: REPLACEABLE element_2 constraining_clause comment ;
element_1: REPLACEABLE element_2 ;
element_2: class_definition ;
element_2: component_clause ;
import_clause: IMPORT import_clause_1 comment ;
import_clause_1: ident '=' name ;
import_clause_1: name '.' import_clause_2 ;
import_clause_1: name ;
import_clause_2: '*' ;
import_clause_2: '{' '}' ;
import_clause_2: '{' import_list '}' ;
import_list: ident ;
import_list: import_list ',' ident ;
extends_clause: EXTENDS name class_modification annotation ;
extends_clause: EXTENDS name class_modification ;
extends_clause: EXTENDS name annotation ;
extends_clause: EXTENDS name ;
constraining_clause: CONSTRAINEDBY name class_modification ;
constraining_clause: CONSTRAINEDBY name ;
component_clause: type_prefix type_specifier array_subscripts component_list ;
component_clause: type_prefix type_specifier component_list ;
component_clause: type_specifier array_subscripts component_list ;
component_clause: type_specifier component_list ;
type_prefix: type_prefix_1 type_prefix_2 type_prefix_3 ;
type_prefix: type_prefix_1 type_prefix_2 ;
type_prefix: type_prefix_1 type_prefix_3 ;
type_prefix: type_prefix_2 type_prefix_3 ;
type_prefix: type_prefix_1 ;
type_prefix: type_prefix_2 ;
type_prefix: type_prefix_3 ;
type_prefix_1: FLOW ;
type_prefix_1: STREAM ;
type_prefix_2: DISCRETE ;
type_prefix_2: PARAMETER ;
type_prefix_2: CONSTANT ;
type_prefix_3: INPUT ;
type_prefix_3: OUTPUT ;
type_specifier: name ;
component_list: component_declaration ;
component_list: component_list ',' component_declaration ;
component_declaration: declaration condition_attribute comment ;
component_declaration: declaration comment ;
condition_attribute: IF expression ;
declaration: ident _P7 _P8 ;
_P7: %empty ;
_P7: array_subscripts ;
_P8: %empty ;
_P8: modification ;
modification: class_modification '=' expression ;
modification: class_modification ;
modification: '=' expression ;
modification: ChSeq_61_58 expression ;
class_modification: '(' argument_list ')' ;
class_modification: '(' ')' ;
argument_list: argument ;
argument_list: argument_list ',' argument ;
argument: element_modification_or_replaceable ;
argument: element_redeclaration ;
element_modification_or_replaceable: EACH FINAL elt_mod_or_repl_1 ;
element_modification_or_replaceable: EACH elt_mod_or_repl_1 ;
element_modification_or_replaceable: FINAL elt_mod_or_repl_1 ;
element_modification_or_replaceable: elt_mod_or_repl_1 ;
elt_mod_or_repl_1: element_modification ;
elt_mod_or_repl_1: element_replaceable ;
element_modification: name _P9 string_comment ;
_P9: %empty ;
_P9: modification ;
element_redeclaration: REDECLARE _P10 _P11 elt_redecl_1 ;
_P10: %empty ;
_P10: EACH ;
_P11: %empty ;
_P11: FINAL ;
elt_redecl_1: short_class_definition ;
elt_redecl_1: component_clause1 ;
elt_redecl_1: element_replaceable ;
element_replaceable: REPLACEABLE short_class_definition component_clause1 constraining_clause ;
element_replaceable: REPLACEABLE short_class_definition component_clause1 ;
component_clause1: type_prefix type_specifier declaration comment ;
short_class_definition: class_prefixes short_class_specifier ;
equation_section: INITIAL EQUATION equation_list ;
equation_section: EQUATION equation_list ;
equation_section: INITIAL EQUATION ;
equation_section: EQUATION ;
algorithm_section: INITIAL ALGORITHM statement_list ;
algorithm_section: ALGORITHM statement_list ;
algorithm_section: INITIAL ALGORITHM ;
algorithm_section: ALGORITHM ;
equation_list: equation ';' ;
equation_list: equation_list equation ';' ;
equation: equation_1 comment ;
equation_1: simple_expression '=' expression ;
equation_1: if_equation ;
equation_1: for_equation ;
equation_1: connect_clause ;
equation_1: when_equation ;
equation_1: name function_call_args ;
statement_list: statement ';' ;
statement_list: statement_list statement ';' ;
statement: statement_1 comment ;
statement_1: component_reference ChSeq_61_58 expression ;
statement_1: component_reference function_call_args ;
statement_1: '(' output_expression_list ')' ChSeq_61_58 component_reference function_call_args ;
statement_1: BREAK ;
statement_1: RETURN ;
statement_1: if_statement ;
statement_1: for_statement ;
statement_1: while_statement ;
statement_1: when_statement ;
if_equation: IF expression then_eq_part elseif_eq_list else_eq_part END IF ;
if_equation: IF expression then_eq_part elseif_eq_list END IF ;
if_equation: IF expression then_eq_part else_eq_part END IF ;
if_equation: IF expression then_eq_part END IF ;
then_eq_part: THEN equation_list ;
then_eq_part: THEN ;
elseif_eq_list: elseif_eq_part ;
elseif_eq_list: elseif_eq_list elseif_eq_part ;
elseif_eq_part: ELSEIF equation_list ;
elseif_eq_part: ELSEIF ;
else_eq_part: ELSE equation_list ;
else_eq_part: ELSE ;
if_statement: IF expression then_st_part elseif_st_list else_st_part END IF ;
if_statement: IF expression then_st_part elseif_st_list END IF ;
if_statement: IF expression then_st_part else_st_part END IF ;
if_statement: IF expression then_st_part END IF ;
then_st_part: THEN statement_list ;
then_st_part: THEN ;
elseif_st_list: elseif_st_part ;
elseif_st_list: elseif_st_list elseif_st_part ;
elseif_st_part: ELSEIF statement_list ;
elseif_st_part: ELSEIF ;
else_st_part: ELSE statement_list ;
else_st_part: ELSE ;
for_equation: FOR for_indices LOOP equation_list END FOR ;
for_equation: FOR for_indices LOOP END FOR ;
for_statement: FOR for_indices LOOP statement_list END FOR ;
for_statement: FOR for_indices LOOP END FOR ;
for_indices: for_index ;
for_indices: for_indices ',' for_index ;
for_index: ident IN expression ;
for_index: ident ;
while_statement: WHILE expression LOOP statement_list END WHILE ;
while_statement: WHILE expression LOOP END WHILE ;
when_equation: WHEN expression then_eq_part elsewhen_eq_list END WHEN ;
elsewhen_eq_list: elsewhen_eq_part ;
elsewhen_eq_list: elsewhen_eq_list elsewhen_eq_part ;
elsewhen_eq_part: ELSEWHEN expression THEN ;
elsewhen_eq_part: ELSEWHEN expression THEN expression_list ;
when_statement: WHEN expression then_st_part elsewhen_st_list END WHEN ;
elsewhen_st_list: elsewhen_st_part ;
elsewhen_st_list: elsewhen_st_list elsewhen_st_part ;
elsewhen_st_part: ELSEWHEN expression THEN ;
elsewhen_st_part: ELSEWHEN expression THEN statement_list ;
connect_clause: CONNECT '(' component_reference ',' component_reference ')' ;
expression: simple_expression ;
expression: IF expression THEN expression elseif_ex_list ELSE expression ;
expression: IF expression THEN expression ELSE expression ;
elseif_ex_list: ELSEIF expression THEN expression ;
elseif_ex_list: elseif_ex_list ELSEIF expression THEN expression ;
simple_expression: logical_expression ;
simple_expression: logical_expression ':' logical_expression ':' logical_expression ;
simple_expression: logical_expression ':' logical_expression ;
logical_expression: logical_term ;
logical_expression: logical_expression OR logical_term ;
logical_term: logical_factor ;
logical_term: logical_term AND logical_factor ;
logical_factor: relation ;
logical_factor: NOT relation ;
relation: arithmetic_expression ;
relation: relation rel_op arithmetic_expression ;
rel_op: '<' ;
rel_op: ChSeq_61_60 ;
rel_op: '>' ;
rel_op: ChSeq_61_62 ;
rel_op: ChSeq_61_61 ;
rel_op: ChSeq_62_60 ;
arithmetic_expression: term ;
arithmetic_expression: arithmetic_expression add_op term ;
add_op: '+' ;
add_op: '-' ;
add_op: ChSeq_43_46 ;
add_op: ChSeq_45_46 ;
term: factor ;
term: term mul_op factor ;
mul_op: '*' ;
mul_op: '/' ;
mul_op: ChSeq_42_46 ;
mul_op: ChSeq_47_46 ;
factor: primary ;
factor: factor '^' primary ;
factor: factor ChSeq_94_46 primary ;
primary: unsigned_number ;
primary: string ;
primary: FALSE ;
primary: TRUE ;
primary: name function_call_args ;
primary: DER function_call_args ;
primary: name ;
primary: name array_subscripts ;
primary: '(' output_expression_list ')' ;
primary: '[' expression_list_list ']' ;
primary: '{' function_arguments '}' ;
expression_list_list: expression_list ;
expression_list_list: expression_list_list ';' expression_list ;
name: ident ;
name: '.' ident ;
name: name '.' ident ;
component_reference: component_reference_1 ;
component_reference: component_reference_1 '.' ident _P12 ;
_P12: %empty ;
_P12: array_subscripts ;
component_reference_1: ident _P13 ;
component_reference_1: '.' ident _P14 ;
_P13: %empty ;
_P13: array_subscripts ;
_P14: %empty ;
_P14: array_subscripts ;
function_call_args: '(' function_arguments ')' ;
function_call_args: '(' ')' ;
function_arguments: function_argument function_argument_1 ;
function_arguments: named_arguments ;
function_argument_1: ',' function_arguments ;
function_argument_1: FOR for_indices ;
named_arguments: named_argument ;
named_arguments: named_arguments ',' named_argument ;
named_argument: ident '=' function_argument ;
function_argument: FUNCTION name '(' named_arguments ')' ;
function_argument: FUNCTION name '(' ')' ;
function_argument: expression ;
output_expression_list: ',' ;
output_expression_list: expression ;
output_expression_list: output_expression_list ',' expression ;
expression_list: expression ;
expression_list: expression_list ',' expression ;
array_subscripts: '[' array_subscript_list ']' ;
array_subscript_list: subscript ;
array_subscript_list: array_subscript_list ',' subscript ;
subscript: ':' ;
subscript: expression ;
comment: string_comment annotation ;
comment: string_comment ;
string_comment: %empty ;
string_comment: string_cat ;
string_cat: string ;
string_cat: string_cat '+' string ;
opt_annotation: %empty ;
opt_annotation: annotation ';' ;
annotation: ANNOTATION class_modification ;
unsigned_number: _fx ;
unsigned_number: _fl ;
ident: _ident ;
string: _string ;

