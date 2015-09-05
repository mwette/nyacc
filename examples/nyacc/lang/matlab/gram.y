// Copyright (C) 2015 Matthew R. Wette
// 
// This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
// or any later version published by the Free Software Foundation.  See the
// file COPYING included with the this distribution.
%token _lone_comm
%token _string
%token _float
%token _fixed
%token _ident
%token ';'
%token ChSeq_39_46
%token '\''
%token '~'
%token ChSeq_94_46
%token ChSeq_92_46
%token ChSeq_47_46
%token ChSeq_42_46
%token '^'
%token '\\'
%token '/'
%token '*'
%token '-'
%token '+'
%token ChSeq_61_62
%token ChSeq_61_60
%token '>'
%token '<'
%token ChSeq_61_126
%token ChSeq_61_61
%token '&'
%token '|'
%token ':'
%token CASE
%token ELSEIF
%token CLEAR
%token GLOBAL
%token RETURN
%token OTHERWISE
%token SWITCH
%token ELSE
%token IF
%token WHILE
%token FOR
%token ','
%token ')'
%token '('
%token '='
%token ']'
%token '['
%token FUNCTION
%token '\n'
%token END
%token _end
%start mfile
%%
mfile: statement_list ;
mfile: function_file ;
function_file: function_defn ;
function_file: function_file function_defn ;
function_defn: function_decl non_comment_statement statement_list opt_end ;
function_defn: function_decl non_comment_statement opt_end ;
function_defn: function_decl opt_end ;
opt_end: %empty ;
opt_end: END term_list ;
function_decl: function_decl_line lone_comment_list ;
function_decl: function_decl_line ;
nl_list: '\n' ;
nl_list: nl_list '\n' ;
function_decl_line: FUNCTION '[' ident_list ']' '=' ident '(' ident_list ')' term ;
function_decl_line: FUNCTION '[' ident_list ']' '=' ident '(' ')' term ;
function_decl_line: FUNCTION ident '=' ident '(' ident_list ')' term ;
function_decl_line: FUNCTION ident '=' ident '(' ')' term ;
function_decl_line: FUNCTION ident '(' ident_list ')' term ;
function_decl_line: FUNCTION ident '(' ')' term ;
ident_list: ident ;
ident_list: ident_list ',' ident ;
statement_list: statement ;
statement_list: statement_list statement ;
statement: lone_comment ;
statement: non_comment_statement ;
non_comment_statement: term ;
non_comment_statement: ident '(' expr_list ')' term ;
non_comment_statement: lval_expr '=' expr term ;
non_comment_statement: '[' lval_expr_list ']' '=' ident '(' ')' term ;
non_comment_statement: '[' lval_expr_list ']' '=' ident '(' expr_list ')' term ;
non_comment_statement: FOR ident '=' expr term statement_list END term ;
non_comment_statement: WHILE expr term statement_list END term ;
non_comment_statement: IF expr term statement_list elseif_list ELSE statement_list END term ;
non_comment_statement: IF expr term statement_list ELSE statement_list END term ;
non_comment_statement: IF expr term statement_list END term ;
non_comment_statement: SWITCH expr term case_list OTHERWISE term statement_list END term ;
non_comment_statement: SWITCH expr term case_list END term ;
non_comment_statement: RETURN term ;
non_comment_statement: command ident_nc_list term ;
lval_expr_list: lval_expr ;
lval_expr_list: lval_expr_list ',' lval_expr ;
lval_expr: ident ;
lval_expr: ident '(' expr_list ')' ;
command: GLOBAL ;
command: CLEAR ;
ident_nc_list: ident ;
ident_nc_list: ident_nc_list ident ;
elseif_list: ELSEIF expr term statement_list ;
elseif_list: elseif_list ELSEIF expr term statement_list ;
case_list: %empty ;
case_list: case_list CASE expr term statement_list ;
expr_list: expr ;
expr_list: ':' ;
expr_list: expr_list ',' expr ;
expr_list: expr_list ',' ':' ;
expr: or_expr ;
expr: expr ':' or_expr ;
or_expr: and_expr ;
or_expr: or_expr '|' and_expr ;
and_expr: equality_expr ;
and_expr: and_expr '&' equality_expr ;
equality_expr: rel_expr ;
equality_expr: equality_expr ChSeq_61_61 rel_expr ;
equality_expr: equality_expr ChSeq_61_126 rel_expr ;
rel_expr: add_expr ;
rel_expr: rel_expr '<' add_expr ;
rel_expr: rel_expr '>' add_expr ;
rel_expr: rel_expr ChSeq_61_60 add_expr ;
rel_expr: rel_expr ChSeq_61_62 add_expr ;
add_expr: mul_expr ;
add_expr: add_expr '+' mul_expr ;
add_expr: add_expr '-' mul_expr ;
mul_expr: unary_expr ;
mul_expr: mul_expr '*' unary_expr ;
mul_expr: mul_expr '/' unary_expr ;
mul_expr: mul_expr '\\' unary_expr ;
mul_expr: mul_expr '^' unary_expr ;
mul_expr: mul_expr ChSeq_42_46 unary_expr ;
mul_expr: mul_expr ChSeq_47_46 unary_expr ;
mul_expr: mul_expr ChSeq_92_46 unary_expr ;
mul_expr: mul_expr ChSeq_94_46 unary_expr ;
unary_expr: postfix_expr ;
unary_expr: '-' postfix_expr ;
unary_expr: '+' postfix_expr ;
unary_expr: '~' postfix_expr ;
postfix_expr: primary_expr ;
postfix_expr: ident '(' expr_list ')' ;
postfix_expr: postfix_expr '\'' ;
postfix_expr: postfix_expr ChSeq_39_46 ;
primary_expr: ident ;
primary_expr: number ;
primary_expr: string ;
primary_expr: '(' expr ')' ;
primary_expr: '[' ']' ;
primary_expr: '[' matrix_row_list ']' ;
matrix_row_list: matrix_row ;
matrix_row_list: matrix_row_list row_term matrix_row ;
row_term: ';' ;
row_term: '\n' ;
matrix_row: expr ;
matrix_row: matrix_row ',' expr ;
term_list: term ;
term_list: term_list term ;
lone_comment_list: lone_comment '\n' ;
lone_comment_list: lone_comment_list lone_comment '\n' ;
term: '\n' ;
term: ';' ;
term: ',' ;
ident: _ident ;
number: _fixed ;
number: _float ;
string: _string ;
lone_comment: _lone_comm ;

