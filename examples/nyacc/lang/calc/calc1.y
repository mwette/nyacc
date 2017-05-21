%token ')'
%token '('
%token _float
%token _fixed
%token '/'
%token '*'
%token '-'
%token '+'
%token _code_comm
%token _lone_comm
%token _end
%left '+' '-'
%left '*' '/'
%define lr.default-reduction accepting
%start expr
%%
expr: expr '+' expr ;
expr: expr '-' expr ;
expr: expr '*' expr ;
expr: expr '/' expr ;
expr: '*' error ;
expr: _fixed ;
expr: _float ;
expr: '(' expr ')' ;

