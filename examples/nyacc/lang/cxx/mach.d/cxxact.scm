;; mach.d/cxxact.scm

;; 
;; This work is derived from CxxGrammar.y by E.D.Willink, which was
;; provided without copyright or license.  Adaptation to nyacc is
;; 
;; Copyright (C) 2016 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.
;; See the file COPYING included with the this distribution.

(define act-v
  (vector
   ;; $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; identifier => '$ident
   (lambda ($1 . $rest) $1)
   ;; id => identifier
   (lambda ($1 . $rest) $1)
   ;; id => identifier template-test "+" template-argument-list ">"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; id => identifier template-test "-"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; id => template-id
   (lambda ($1 . $rest) $1)
   ;; template-test => "<"
   (lambda ($1 . $rest) (template-test))
   ;; global-scope => "::"
   (lambda ($1 . $rest) $1)
   ;; global-scope => "template" global-scope
   (lambda ($2 $1 . $rest) $1)
   ;; id-scope => id "::"
   (lambda ($2 $1 . $rest) $1)
   ;; nested-id => id
   (lambda ($1 . $rest) $1)
   ;; nested-id => id-scope nested-id
   (lambda ($2 $1 . $rest) $1)
   ;; scoped-id => nested-id
   (lambda ($1 . $rest) $1)
   ;; scoped-id => global-scope nested-id
   (lambda ($2 $1 . $rest) $1)
   ;; destructor-id => "~" id
   (lambda ($2 $1 . $rest) $1)
   ;; destructor-id => "template" destructor-id
   (lambda ($2 $1 . $rest) $1)
   ;; special-function-id => conversion-function-id
   (lambda ($1 . $rest) $1)
   ;; special-function-id => operator-function-id
   (lambda ($1 . $rest) $1)
   ;; special-function-id => "template" special-function-id
   (lambda ($2 $1 . $rest) $1)
   ;; nested-special-function-id => special-function-id
   (lambda ($1 . $rest) $1)
   ;; nested-special-function-id => id-scope destructor-id
   (lambda ($2 $1 . $rest) $1)
   ;; nested-special-function-id => id-scope nested-special-function-id
   (lambda ($2 $1 . $rest) $1)
   ;; scoped-special-function-id => nested-special-function-id
   (lambda ($1 . $rest) $1)
   ;; scoped-special-function-id => global-scope nested-special-function-id
   (lambda ($2 $1 . $rest) $1)
   ;; declarator-id => scoped-id
   (lambda ($1 . $rest) $1)
   ;; declarator-id => scoped-special-function-id
   (lambda ($1 . $rest) $1)
   ;; declarator-id => destructor-id
   (lambda ($1 . $rest) $1)
   ;; built-in-type-id => built-in-type-specifier
   (lambda ($1 . $rest) $1)
   ;; built-in-type-id => built-in-type-id built-in-type-specifier
   (lambda ($2 $1 . $rest) $1)
   ;; pseudo-destructor-id => built-in-type-id "::" "~" built-in-type-id
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; pseudo-destructor-id => "~" built-in-type-id
   (lambda ($2 $1 . $rest) $1)
   ;; pseudo-destructor-id => "template" pseudo-destructor-id
   (lambda ($2 $1 . $rest) $1)
   ;; nested-pseudo-destructor-id => pseudo-destructor-id
   (lambda ($1 . $rest) $1)
   ;; nested-pseudo-destructor-id => id-scope nested-pseudo-destructor-id
   (lambda ($2 $1 . $rest) $1)
   ;; scoped-pseudo-destructor-id => nested-pseudo-destructor-id
   (lambda ($1 . $rest) $1)
   ;; scoped-pseudo-destructor-id => global-scope scoped-pseudo-destructor-id
   (lambda ($2 $1 . $rest) $1)
   ;; string => StringLiteral
   (lambda ($1 . $rest) (make-tl 'string $1))
   ;; literal => IntegerLiteral
   (lambda ($1 . $rest) $1)
   ;; literal => CharacterLiteral
   (lambda ($1 . $rest) $1)
   ;; literal => FloatingLiteral
   (lambda ($1 . $rest) $1)
   ;; literal => string
   (lambda ($1 . $rest) $1)
   ;; literal => boolean-literal
   (lambda ($1 . $rest) $1)
   ;; IntegerLiteral => '$fixed
   (lambda ($1 . $rest) $1)
   ;; CharacterLiteral => '$chlit
   (lambda ($1 . $rest) $1)
   ;; FloatingLiteral => '$float
   (lambda ($1 . $rest) $1)
   ;; StringLiteral => '$string
   (lambda ($1 . $rest) (make-tl 'string $1))
   ;; StringLiteral => StringLiteral '$string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; boolean-literal => "false"
   (lambda ($1 . $rest) `(boolean ,$1))
   ;; boolean-literal => "true"
   (lambda ($1 . $rest) `(boolean ,$1))
   ;; translation-unit => declaration-seq.opt
   (lambda ($1 . $rest) $1)
   ;; primary-expression => literal
   (lambda ($1 . $rest) $1)
   ;; primary-expression => "this"
   (lambda ($1 . $rest) $1)
   ;; primary-expression => suffix-decl-specified-ids
   (lambda ($1 . $rest) $1)
   ;; primary-expression => abstract-expression
   (lambda ($1 . $rest) $1)
   ;; abstract-expression => parenthesis-clause
   (lambda ($1 . $rest) $1)
   ;; abstract-expression => "[" expression.opt "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; abstract-expression => "template" abstract-expression
   (lambda ($2 $1 . $rest) $1)
   ;; type1-parameters => parameter-declaration-list ";"
   (lambda ($2 $1 . $rest) $1)
   ;; type1-parameters => type1-parameters parameter-declaration-list ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; mark-type1 => 
   (lambda $rest (mark-type1) (yyclearin))
   ;; postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postfix-expression parenthesis-clause mark-type...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression parenthesis-clause mark-type...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     (yyerrok)
     (remark-type1)
     (unmark)
     (unmark))
   ;; postfix-expression => postfix-expression parenthesis-clause mark-type...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (yyerrok)
     (remark-type1)
     (unmark)
     (unmark))
   ;; postfix-expression => postfix-expression parenthesis-clause mark-type...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (yyerrok)
     (yyclearin)
     (remark-type1)
     (unmark))
   ;; postfix-expression => postfix-expression "[" expression.opt "]"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "." declarator-id
   (lambda ($3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "." scoped-pseudo-destructor-id
   (lambda ($3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "->" declarator-id
   (lambda ($3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "->" scoped-pseudo-destructo...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) $1)
   ;; postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) $1)
   ;; postfix-expression => "dynamic_cast" "<" type-id ">" "(" expression ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => "static_cast" "<" type-id ">" "(" expression ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => "reinterpret_cast" "<" type-id ">" "(" expressi...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => "const_cast" "<" type-id ">" "(" expression ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; postfix-expression => "typeid" parameters-clause
   (lambda ($2 $1 . $rest) $1)
   ;; expression-list.opt => 
   (lambda $rest (list))
   ;; expression-list.opt => expression-list
   (lambda ($1 . $rest) $1)
   ;; expression-list => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; expression-list => expression-list "," assignment-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => "++" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => "--" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => ptr-operator cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => suffix-decl-specified-scope star-ptr-operator cas...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; unary-expression => "+" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => "-" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => "!" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => "~" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => new-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => global-scope new-expression
   (lambda ($2 $1 . $rest) $1)
   ;; unary-expression => delete-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => global-scope delete-expression
   (lambda ($2 $1 . $rest) $1)
   ;; delete-expression => "delete" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; new-expression => "new" new-type-id new-initializer.opt
   (lambda ($3 $2 $1 . $rest) $1)
   ;; new-expression => "new" parameters-clause new-type-id new-initializer...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; new-expression => "new" parameters-clause
   (lambda ($2 $1 . $rest) $1)
   ;; new-expression => "new" parameters-clause parameters-clause new-initi...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; new-type-id => type-specifier ptr-operator-seq.opt
   (lambda ($2 $1 . $rest) $1)
   ;; new-type-id => type-specifier new-declarator
   (lambda ($2 $1 . $rest) $1)
   ;; new-type-id => type-specifier new-type-id
   (lambda ($2 $1 . $rest) $1)
   ;; new-declarator => ptr-operator new-declarator
   (lambda ($2 $1 . $rest) $1)
   ;; new-declarator => direct-new-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-new-declarator => "[" expression "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; direct-new-declarator => direct-new-declarator "[" constant-expressio...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; new-initializer.opt => 
   (lambda $rest (list))
   ;; new-initializer.opt => "(" expression-list.opt ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; cast-expression => abstract-expression cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; pm-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; pm-expression => pm-expression ".*" cast-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; pm-expression => pm-expression "->*" cast-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; multiplicative-expression => pm-expression
   (lambda ($1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression star-ptr-opera...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression "/" pm-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression "%" pm-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; and-expression => and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; exclusive-or-expression => and-expression
   (lambda ($1 . $rest) $1)
   ;; exclusive-or-expression => exclusive-or-expression "^" and-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; inclusive-or-expression => exclusive-or-expression
   (lambda ($1 . $rest) $1)
   ;; inclusive-or-expression => inclusive-or-expression "|" exclusive-or-e...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; logical-and-expression => inclusive-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-and-expression => logical-and-expression "&&" inclusive-or-ex...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; conditional-expression => logical-or-expression "?" expression ":" as...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; assignment-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; assignment-expression => logical-or-expression assignment-operator as...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; assignment-expression => logical-or-expression "=" braced-initializer
   (lambda ($3 $2 $1 . $rest) $1)
   ;; assignment-expression => throw-expression
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "+="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "&="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "/="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "%="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "*="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "|="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "<<="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => ">>="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "-="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "^="
   (lambda ($1 . $rest) $1)
   ;; expression.opt => 
   (lambda $rest (list))
   ;; expression.opt => expression
   (lambda ($1 . $rest) $1)
   ;; expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; expression => expression-list "," assignment-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; constant-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; templated-relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; templated-relational-expression => templated-relational-expression "<...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-relational-expression => templated-relational-expression "<...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-relational-expression => templated-relational-expression ">...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-equality-expression => templated-relational-expression
   (lambda ($1 . $rest) $1)
   ;; templated-equality-expression => templated-equality-expression "==" t...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-equality-expression => templated-equality-expression "!=" t...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-and-expression => templated-equality-expression
   (lambda ($1 . $rest) $1)
   ;; templated-and-expression => templated-and-expression "&" templated-eq...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-exclusive-or-expression => templated-and-expression
   (lambda ($1 . $rest) $1)
   ;; templated-exclusive-or-expression => templated-exclusive-or-expressio...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-inclusive-or-expression => templated-exclusive-or-expression
   (lambda ($1 . $rest) $1)
   ;; templated-inclusive-or-expression => templated-inclusive-or-expressio...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-logical-and-expression => templated-inclusive-or-expression
   (lambda ($1 . $rest) $1)
   ;; templated-logical-and-expression => templated-logical-and-expression ...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-logical-or-expression => templated-logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; templated-logical-or-expression => templated-logical-or-expression "|...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-conditional-expression => templated-logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; templated-conditional-expression => templated-logical-or-expression "...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; templated-assignment-expression => templated-conditional-expression
   (lambda ($1 . $rest) $1)
   ;; templated-assignment-expression => templated-logical-or-expression as...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-assignment-expression => templated-throw-expression
   (lambda ($1 . $rest) $1)
   ;; templated-expression => templated-assignment-expression
   (lambda ($1 . $rest) $1)
   ;; templated-expression => templated-expression-list "," templated-assig...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-expression-list => templated-assignment-expression
   (lambda ($1 . $rest) $1)
   ;; templated-expression-list => templated-expression-list "," templated-...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looping-statement => start-search looped-statement
   (lambda ($2 $1 . $rest) (end-search))
   ;; looped-statement => statement
   (lambda ($1 . $rest) $1)
   ;; looped-statement => advance-search "+" looped-statement
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looped-statement => advance-search "-"
   (lambda ($2 $1 . $rest) $1)
   ;; statement => control-statement
   (lambda ($1 . $rest) $1)
   ;; statement => compound-statement
   (lambda ($1 . $rest) $1)
   ;; statement => declaration-statement
   (lambda ($1 . $rest) $1)
   ;; statement => try-block
   (lambda ($1 . $rest) $1)
   ;; control-statement => labeled-statement
   (lambda ($1 . $rest) $1)
   ;; control-statement => selection-statement
   (lambda ($1 . $rest) $1)
   ;; control-statement => iteration-statement
   (lambda ($1 . $rest) $1)
   ;; control-statement => jump-statement
   (lambda ($1 . $rest) $1)
   ;; labeled-statement => identifier ":" looping-statement
   (lambda ($3 $2 $1 . $rest) $1)
   ;; labeled-statement => "case" constant-expression ":" looping-statement
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; labeled-statement => "default" ":" looping-statement
   (lambda ($3 $2 $1 . $rest) $1)
   ;; compound-statement => "{" statement-seq.opt "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; compound-statement => "{" statement-seq.opt looping-statement "#" ban...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad statement-seq."))
   ;; statement-seq.opt => 
   (lambda $rest (list))
   ;; statement-seq.opt => statement-seq.opt looping-statement
   (lambda ($2 $1 . $rest) $1)
   ;; statement-seq.opt => statement-seq.opt looping-statement "#" bang '$e...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad statement."))
   ;; selection-statement => "if" "(" condition ")" looping-statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; selection-statement => "if" "(" condition ")" looping-statement "else...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; selection-statement => "switch" "(" condition ")" looping-statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; condition.opt => 
   (lambda $rest (list))
   ;; condition.opt => condition
   (lambda ($1 . $rest) $1)
   ;; condition => parameter-declaration-list
   (lambda ($1 . $rest) $1)
   ;; iteration-statement => "while" "(" condition ")" looping-statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; iteration-statement => "do" looping-statement "while" "(" expression ...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; iteration-statement => "for" "(" for-init-statement condition.opt ";"...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; for-init-statement => simple-declaration
   (lambda ($1 . $rest) $1)
   ;; jump-statement => "break" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; jump-statement => "continue" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; jump-statement => "return" expression.opt ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; jump-statement => "goto" identifier ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; declaration-statement => block-declaration
   (lambda ($1 . $rest) $1)
   ;; compound-declaration => "{" nest declaration-seq.opt "}"
   (lambda ($4 $3 $2 $1 . $rest) (unnest))
   ;; compound-declaration => "{" nest declaration-seq.opt util looping-dec...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     (unnest)
     (UNBANG "Bad declaration-seq."))
   ;; declaration-seq.opt => 
   (lambda $rest (list))
   ;; declaration-seq.opt => declaration-seq.opt util looping-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; declaration-seq.opt => declaration-seq.opt util looping-declaration "...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad declaration."))
   ;; looping-declaration => start-search1 looped-declaration
   (lambda ($2 $1 . $rest) (end-search))
   ;; looped-declaration => declaration
   (lambda ($1 . $rest) $1)
   ;; looped-declaration => advance-search "+" looped-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looped-declaration => advance-search "-"
   (lambda ($2 $1 . $rest) $1)
   ;; declaration => block-declaration
   (lambda ($1 . $rest) $1)
   ;; declaration => function-definition
   (lambda ($1 . $rest) $1)
   ;; declaration => template-declaration
   (lambda ($1 . $rest) $1)
   ;; declaration => explicit-specialization
   (lambda ($1 . $rest) $1)
   ;; declaration => specialised-declaration
   (lambda ($1 . $rest) $1)
   ;; specialised-declaration => linkage-specification
   (lambda ($1 . $rest) $1)
   ;; specialised-declaration => namespace-definition
   (lambda ($1 . $rest) $1)
   ;; specialised-declaration => "template" specialised-declaration
   (lambda ($2 $1 . $rest) $1)
   ;; block-declaration => simple-declaration
   (lambda ($1 . $rest) $1)
   ;; block-declaration => specialised-block-declaration
   (lambda ($1 . $rest) $1)
   ;; specialised-block-declaration => asm-definition
   (lambda ($1 . $rest) $1)
   ;; specialised-block-declaration => namespace-alias-definition
   (lambda ($1 . $rest) $1)
   ;; specialised-block-declaration => using-declaration
   (lambda ($1 . $rest) $1)
   ;; specialised-block-declaration => using-directive
   (lambda ($1 . $rest) $1)
   ;; specialised-block-declaration => "template" specialised-block-declara...
   (lambda ($2 $1 . $rest) $1)
   ;; simple-declaration => ";"
   (lambda ($1 . $rest) $1)
   ;; simple-declaration => init-declaration ";"
   (lambda ($2 $1 . $rest) $1)
   ;; simple-declaration => init-declarations ";"
   (lambda ($2 $1 . $rest) $1)
   ;; simple-declaration => decl-specifier-prefix simple-declaration
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-built-in-decl-specifier.raw => built-in-type-specifier
   (lambda ($1 . $rest) $1)
   ;; suffix-built-in-decl-specifier.raw => suffix-built-in-decl-specifier....
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-built-in-decl-specifier.raw => suffix-built-in-decl-specifier....
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-built-in-decl-specifier => suffix-built-in-decl-specifier.raw
   (lambda ($1 . $rest) $1)
   ;; suffix-built-in-decl-specifier => "template" suffix-built-in-decl-spe...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-named-decl-specifier => scoped-id
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifier => elaborate-type-specifier
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifier => suffix-named-decl-specifier decl-speci...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-named-decl-specifier.bi => suffix-named-decl-specifier
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifier.bi => suffix-named-decl-specifier suffix-...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-named-decl-specifiers => suffix-named-decl-specifier.bi
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifiers => suffix-named-decl-specifiers suffix-n...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-named-decl-specifiers.sf => scoped-special-function-id
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifiers.sf => suffix-named-decl-specifiers
   (lambda ($1 . $rest) $1)
   ;; suffix-named-decl-specifiers.sf => suffix-named-decl-specifiers scope...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-decl-specified-ids => suffix-built-in-decl-specifier
   (lambda ($1 . $rest) $1)
   ;; suffix-decl-specified-ids => suffix-built-in-decl-specifier suffix-na...
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-decl-specified-ids => suffix-named-decl-specifiers.sf
   (lambda ($1 . $rest) $1)
   ;; suffix-decl-specified-scope => suffix-named-decl-specifiers "::"
   (lambda ($2 $1 . $rest) $1)
   ;; suffix-decl-specified-scope => suffix-built-in-decl-specifier suffix-...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; suffix-decl-specified-scope => suffix-built-in-decl-specifier "::"
   (lambda ($2 $1 . $rest) $1)
   ;; decl-specifier-affix => storage-class-specifier
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-affix => function-specifier
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-affix => "friend"
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-affix => "typedef"
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-affix => cv-qualifier
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-suffix => decl-specifier-affix
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-prefix => decl-specifier-affix
   (lambda ($1 . $rest) $1)
   ;; decl-specifier-prefix => "template" decl-specifier-prefix
   (lambda ($2 $1 . $rest) $1)
   ;; storage-class-specifier => "register"
   (lambda ($1 . $rest) $1)
   ;; storage-class-specifier => "static"
   (lambda ($1 . $rest) $1)
   ;; storage-class-specifier => "mutable"
   (lambda ($1 . $rest) $1)
   ;; storage-class-specifier => "extern"
   (lambda ($1 . $rest) $1)
   ;; storage-class-specifier => "auto"
   (lambda ($1 . $rest) $1)
   ;; function-specifier => "explicit"
   (lambda ($1 . $rest) $1)
   ;; function-specifier => "inline"
   (lambda ($1 . $rest) $1)
   ;; function-specifier => "virtual"
   (lambda ($1 . $rest) $1)
   ;; type-specifier => simple-type-specifier
   (lambda ($1 . $rest) $1)
   ;; type-specifier => elaborate-type-specifier
   (lambda ($1 . $rest) $1)
   ;; type-specifier => cv-qualifier
   (lambda ($1 . $rest) $1)
   ;; elaborate-type-specifier => class-specifier
   (lambda ($1 . $rest) $1)
   ;; elaborate-type-specifier => enum-specifier
   (lambda ($1 . $rest) $1)
   ;; elaborate-type-specifier => elaborated-type-specifier
   (lambda ($1 . $rest) $1)
   ;; elaborate-type-specifier => "template" elaborate-type-specifier
   (lambda ($2 $1 . $rest) $1)
   ;; simple-type-specifier => scoped-id
   (lambda ($1 . $rest) $1)
   ;; simple-type-specifier => built-in-type-specifier
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "char"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "wchar_t"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "bool"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "short"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "int"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "long"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "signed"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "unsigned"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "float"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "double"
   (lambda ($1 . $rest) $1)
   ;; built-in-type-specifier => "void"
   (lambda ($1 . $rest) $1)
   ;; elaborated-type-specifier => elaborated-class-specifier
   (lambda ($1 . $rest) $1)
   ;; elaborated-type-specifier => elaborated-enum-specifier
   (lambda ($1 . $rest) $1)
   ;; elaborated-type-specifier => "typename" scoped-id
   (lambda ($2 $1 . $rest) $1)
   ;; elaborated-enum-specifier => "enum" scoped-id
   (lambda ($2 $1 . $rest) $1)
   ;; enum-specifier => "enum" scoped-id enumerator-clause
   (lambda ($3 $2 $1 . $rest) $1)
   ;; enum-specifier => "enum" enumerator-clause
   (lambda ($2 $1 . $rest) $1)
   ;; enumerator-clause => "{" enumerator-list-ecarb
   (lambda ($2 $1 . $rest) $1)
   ;; enumerator-clause => "{" enumerator-list enumerator-list-ecarb
   (lambda ($3 $2 $1 . $rest) $1)
   ;; enumerator-clause => "{" enumerator-list "," enumerator-definition-ecarb
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; enumerator-list-ecarb => "}"
   (lambda ($1 . $rest) $1)
   ;; enumerator-list-ecarb => bang '$error "}"
   (lambda ($3 $2 $1 . $rest)
     (UNBANG "Bad enumerator-list."))
   ;; enumerator-definition-ecarb => "}"
   (lambda ($1 . $rest) $1)
   ;; enumerator-definition-ecarb => bang '$error "}"
   (lambda ($3 $2 $1 . $rest)
     (UNBANG "Bad enumerator-definition."))
   ;; enumerator-definition-filler => 
   (lambda $rest (list))
   ;; enumerator-definition-filler => bang '$error ","
   (lambda ($3 $2 $1 . $rest)
     (UNBANG "Bad enumerator-definition."))
   ;; enumerator-list-head => enumerator-definition-filler
   (lambda ($1 . $rest) $1)
   ;; enumerator-list-head => enumerator-list "," enumerator-definition-filler
   (lambda ($3 $2 $1 . $rest) $1)
   ;; enumerator-list => enumerator-list-head enumerator-definition
   (lambda ($2 $1 . $rest) $1)
   ;; enumerator-definition => enumerator
   (lambda ($1 . $rest) $1)
   ;; enumerator-definition => enumerator "=" constant-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; enumerator => identifier
   (lambda ($1 . $rest) $1)
   ;; namespace-definition => "namespace" scoped-id compound-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; namespace-definition => "namespace" compound-declaration
   (lambda ($2 $1 . $rest) $1)
   ;; namespace-alias-definition => "namespace" scoped-id "=" scoped-id ";"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; using-declaration => "using" declarator-id ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; using-declaration => "using" "typename" declarator-id ";"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; using-directive => "using" "namespace" scoped-id ";"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; asm-definition => "asm" "(" string ")" ";"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; linkage-specification => "extern" string looping-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; linkage-specification => "extern" string compound-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; init-declarations => assignment-expression "," init-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; init-declarations => init-declarations "," init-declaration
   (lambda ($3 $2 $1 . $rest) $1)
   ;; init-declaration => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; star-ptr-operator => "*"
   (lambda ($1 . $rest) $1)
   ;; star-ptr-operator => star-ptr-operator cv-qualifier
   (lambda ($2 $1 . $rest) $1)
   ;; nested-ptr-operator => star-ptr-operator
   (lambda ($1 . $rest) $1)
   ;; nested-ptr-operator => id-scope nested-ptr-operator
   (lambda ($2 $1 . $rest) $1)
   ;; ptr-operator => "&"
   (lambda ($1 . $rest) $1)
   ;; ptr-operator => nested-ptr-operator
   (lambda ($1 . $rest) $1)
   ;; ptr-operator => global-scope nested-ptr-operator
   (lambda ($2 $1 . $rest) $1)
   ;; ptr-operator-seq => ptr-operator
   (lambda ($1 . $rest) $1)
   ;; ptr-operator-seq => ptr-operator ptr-operator-seq
   (lambda ($2 $1 . $rest) $1)
   ;; ptr-operator-seq.opt => 
   (lambda $rest (list))
   ;; ptr-operator-seq.opt => ptr-operator ptr-operator-seq.opt
   (lambda ($2 $1 . $rest) $1)
   ;; cv-qualifier-seq.opt => 
   (lambda $rest (list))
   ;; cv-qualifier-seq.opt => cv-qualifier-seq.opt cv-qualifier
   (lambda ($2 $1 . $rest) $1)
   ;; cv-qualifier => "const"
   (lambda ($1 . $rest) $1)
   ;; cv-qualifier => "volatile"
   (lambda ($1 . $rest) $1)
   ;; type-id => type-specifier abstract-declarator.opt
   (lambda ($2 $1 . $rest) $1)
   ;; type-id => type-specifier type-id
   (lambda ($2 $1 . $rest) $1)
   ;; abstract-declarator.opt => 
   (lambda $rest (list))
   ;; abstract-declarator.opt => ptr-operator abstract-declarator.opt
   (lambda ($2 $1 . $rest) $1)
   ;; abstract-declarator.opt => direct-abstract-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-abstract-declarator.opt => 
   (lambda $rest (list))
   ;; direct-abstract-declarator.opt => direct-abstract-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-abstract-declarator => direct-abstract-declarator.opt parenthe...
   (lambda ($2 $1 . $rest) $1)
   ;; direct-abstract-declarator => direct-abstract-declarator.opt "[" "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; direct-abstract-declarator => direct-abstract-declarator.opt "[" cons...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; parenthesis-clause => parameters-clause cv-qualifier-seq.opt
   (lambda ($2 $1 . $rest) $1)
   ;; parenthesis-clause => parameters-clause cv-qualifier-seq.opt exceptio...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; parameters-clause => "(" parameter-declaration-clause ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; parameter-declaration-clause => 
   (lambda $rest (list))
   ;; parameter-declaration-clause => parameter-declaration-list
   (lambda ($1 . $rest) $1)
   ;; parameter-declaration-clause => parameter-declaration-list "..."
   (lambda ($2 $1 . $rest) $1)
   ;; parameter-declaration-list => parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; parameter-declaration-list => parameter-declaration-list "," paramete...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; abstract-pointer-declaration => ptr-operator-seq
   (lambda ($1 . $rest) $1)
   ;; abstract-pointer-declaration => multiplicative-expression star-ptr-op...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; abstract-parameter-declaration => abstract-pointer-declaration
   (lambda ($1 . $rest) $1)
   ;; abstract-parameter-declaration => and-expression "&"
   (lambda ($2 $1 . $rest) $1)
   ;; abstract-parameter-declaration => and-expression "&" abstract-pointer...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; special-parameter-declaration => abstract-parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; special-parameter-declaration => abstract-parameter-declaration "=" a...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; special-parameter-declaration => "..."
   (lambda ($1 . $rest) $1)
   ;; parameter-declaration => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; parameter-declaration => special-parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; parameter-declaration => decl-specifier-prefix parameter-declaration
   (lambda ($2 $1 . $rest) $1)
   ;; templated-parameter-declaration => templated-assignment-expression
   (lambda ($1 . $rest) $1)
   ;; templated-parameter-declaration => templated-abstract-declaration
   (lambda ($1 . $rest) $1)
   ;; templated-parameter-declaration => templated-abstract-declaration "="...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; templated-parameter-declaration => decl-specifier-prefix templated-pa...
   (lambda ($2 $1 . $rest) $1)
   ;; templated-abstract-declaration => abstract-pointer-declaration
   (lambda ($1 . $rest) $1)
   ;; templated-abstract-declaration => templated-and-expression "&"
   (lambda ($2 $1 . $rest) $1)
   ;; templated-abstract-declaration => templated-and-expression "&" abstra...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; function-definition => ctor-definition
   (lambda ($1 . $rest) $1)
   ;; function-definition => func-definition
   (lambda ($1 . $rest) $1)
   ;; func-definition => assignment-expression function-try-block
   (lambda ($2 $1 . $rest) $1)
   ;; func-definition => assignment-expression function-body
   (lambda ($2 $1 . $rest) $1)
   ;; func-definition => decl-specifier-prefix func-definition
   (lambda ($2 $1 . $rest) $1)
   ;; ctor-definition => constructor-head function-try-block
   (lambda ($2 $1 . $rest) $1)
   ;; ctor-definition => constructor-head function-body
   (lambda ($2 $1 . $rest) $1)
   ;; ctor-definition => decl-specifier-prefix ctor-definition
   (lambda ($2 $1 . $rest) $1)
   ;; constructor-head => bit-field-init-declaration
   (lambda ($1 . $rest) $1)
   ;; constructor-head => constructor-head "," assignment-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; function-try-block => "try" function-block handler-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; function-block => ctor-initializer.opt function-body
   (lambda ($2 $1 . $rest) $1)
   ;; function-body => compound-statement
   (lambda ($1 . $rest) $1)
   ;; initializer-clause => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; initializer-clause => braced-initializer
   (lambda ($1 . $rest) $1)
   ;; braced-initializer => "{" initializer-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; braced-initializer => "{" initializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; braced-initializer => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; braced-initializer => "{" looping-initializer-clause "#" bang '$error...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad initializer-clause."))
   ;; braced-initializer => "{" initializer-list "," looping-initializer-cl...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad initializer-clause."))
   ;; initializer-list => looping-initializer-clause
   (lambda ($1 . $rest) $1)
   ;; initializer-list => initializer-list "," looping-initializer-clause
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looping-initializer-clause => start-search looped-initializer-clause
   (lambda ($2 $1 . $rest) (end-search))
   ;; looped-initializer-clause => initializer-clause
   (lambda ($1 . $rest) $1)
   ;; looped-initializer-clause => advance-search "+" looped-initializer-cl...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looped-initializer-clause => advance-search "-"
   (lambda ($2 $1 . $rest) $1)
   ;; colon-mark => ":"
   (lambda ($1 . $rest) (mark))
   ;; elaborated-class-specifier => class-key scoped-id
   (lambda ($2 $1 . $rest) $1)
   ;; elaborated-class-specifier => class-key scoped-id colon-mark '$error
   (lambda ($4 $3 $2 $1 . $rest) (rewind-colon))
   ;; class-specifier-head => class-key scoped-id colon-mark base-specifier...
   (lambda ($5 $4 $3 $2 $1 . $rest) (unmark))
   ;; class-specifier-head => class-key ":" base-specifier-list "{"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; class-specifier-head => class-key scoped-id "{"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; class-specifier-head => class-key "{"
   (lambda ($2 $1 . $rest) $1)
   ;; class-key => "class"
   (lambda ($1 . $rest) $1)
   ;; class-key => "struct"
   (lambda ($1 . $rest) $1)
   ;; class-key => "union"
   (lambda ($1 . $rest) $1)
   ;; class-specifier => class-specifier-head member-specification.opt "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; class-specifier => class-specifier-head member-specification.opt util...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad member-specification.opt."))
   ;; member-specification.opt => 
   (lambda $rest (list))
   ;; member-specification.opt => member-specification.opt util looping-mem...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; member-specification.opt => member-specification.opt util looping-mem...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (UNBANG "Bad member-declaration."))
   ;; looping-member-declaration => start-search looped-member-declaration
   (lambda ($2 $1 . $rest) (end-search))
   ;; looped-member-declaration => member-declaration
   (lambda ($1 . $rest) $1)
   ;; looped-member-declaration => advance-search "+" looped-member-declara...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; looped-member-declaration => advance-search "-"
   (lambda ($2 $1 . $rest) $1)
   ;; member-declaration => accessibility-specifier
   (lambda ($1 . $rest) $1)
   ;; member-declaration => simple-member-declaration
   (lambda ($1 . $rest) $1)
   ;; member-declaration => function-definition
   (lambda ($1 . $rest) $1)
   ;; member-declaration => using-declaration
   (lambda ($1 . $rest) $1)
   ;; member-declaration => template-declaration
   (lambda ($1 . $rest) $1)
   ;; simple-member-declaration => ";"
   (lambda ($1 . $rest) $1)
   ;; simple-member-declaration => assignment-expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; simple-member-declaration => constructor-head ";"
   (lambda ($2 $1 . $rest) $1)
   ;; simple-member-declaration => member-init-declarations ";"
   (lambda ($2 $1 . $rest) $1)
   ;; simple-member-declaration => decl-specifier-prefix simple-member-decl...
   (lambda ($2 $1 . $rest) $1)
   ;; member-init-declarations => assignment-expression "," member-init-dec...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; member-init-declarations => constructor-head "," bit-field-init-decla...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; member-init-declarations => member-init-declarations "," member-init-...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; member-init-declaration => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; member-init-declaration => bit-field-init-declaration
   (lambda ($1 . $rest) $1)
   ;; accessibility-specifier => access-specifier ":"
   (lambda ($2 $1 . $rest) $1)
   ;; bit-field-declaration => assignment-expression ":" bit-field-width
   (lambda ($3 $2 $1 . $rest) $1)
   ;; bit-field-declaration => ":" bit-field-width
   (lambda ($2 $1 . $rest) $1)
   ;; bit-field-width => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; bit-field-width => logical-or-expression "?" bit-field-width ":" bit-...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; bit-field-init-declaration => bit-field-declaration
   (lambda ($1 . $rest) $1)
   ;; bit-field-init-declaration => bit-field-declaration "=" initializer-c...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; base-specifier-list => base-specifier
   (lambda ($1 . $rest) $1)
   ;; base-specifier-list => base-specifier-list "," base-specifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; base-specifier => scoped-id
   (lambda ($1 . $rest) $1)
   ;; base-specifier => access-specifier base-specifier
   (lambda ($2 $1 . $rest) $1)
   ;; base-specifier => "virtual" base-specifier
   (lambda ($2 $1 . $rest) $1)
   ;; access-specifier => "private"
   (lambda ($1 . $rest) $1)
   ;; access-specifier => "protected"
   (lambda ($1 . $rest) $1)
   ;; access-specifier => "public"
   (lambda ($1 . $rest) $1)
   ;; conversion-function-id => "operator" conversion-type-id
   (lambda ($2 $1 . $rest) $1)
   ;; conversion-type-id => type-specifier ptr-operator-seq.opt
   (lambda ($2 $1 . $rest) $1)
   ;; conversion-type-id => type-specifier conversion-type-id
   (lambda ($2 $1 . $rest) $1)
   ;; ctor-initializer.opt => 
   (lambda $rest (list))
   ;; ctor-initializer.opt => ctor-initializer
   (lambda ($1 . $rest) $1)
   ;; ctor-initializer => ":" mem-initializer-list
   (lambda ($2 $1 . $rest) $1)
   ;; ctor-initializer => ":" mem-initializer-list bang '$error
   (lambda ($4 $3 $2 $1 . $rest)
     (UNBANG "Bad ctor-initializer."))
   ;; mem-initializer-list => mem-initializer
   (lambda ($1 . $rest) $1)
   ;; mem-initializer-list => mem-initializer-list-head mem-initializer
   (lambda ($2 $1 . $rest) $1)
   ;; mem-initializer-list-head => mem-initializer-list ","
   (lambda ($2 $1 . $rest) $1)
   ;; mem-initializer-list-head => mem-initializer-list bang '$error ","
   (lambda ($4 $3 $2 $1 . $rest)
     (UNBANG "Bad mem-initializer."))
   ;; mem-initializer => mem-initializer-id "(" expression-list.opt ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; mem-initializer-id => scoped-id
   (lambda ($1 . $rest) $1)
   ;; operator-function-id => "operator" operator
   (lambda ($2 $1 . $rest) $1)
   ;; operator => "new"
   (lambda ($1 . $rest) $1)
   ;; operator => "delete"
   (lambda ($1 . $rest) $1)
   ;; operator => "+"
   (lambda ($1 . $rest) `(add ,$1))
   ;; operator => "-"
   (lambda ($1 . $rest) `(sub ,$1))
   ;; operator => "*"
   (lambda ($1 . $rest) `(mul ,$1))
   ;; operator => "/"
   (lambda ($1 . $rest) `(div ,$1))
   ;; operator => "%"
   (lambda ($1 . $rest) `(mod ,$1))
   ;; operator => "^"
   (lambda ($1 . $rest) `(bw-xor ,$1))
   ;; operator => "&"
   (lambda ($1 . $rest) `(bw-and ,$1))
   ;; operator => "|"
   (lambda ($1 . $rest) `(bw-or ,$1))
   ;; operator => "~"
   (lambda ($1 . $rest) `(bw-not ,$1))
   ;; operator => "!"
   (lambda ($1 . $rest) `(not ,$1))
   ;; operator => "="
   (lambda ($1 . $rest) `(assn ,$1))
   ;; operator => "<"
   (lambda ($1 . $rest) `(lt ,$1))
   ;; operator => ">"
   (lambda ($1 . $rest) `(gt ,$1))
   ;; operator => "+="
   (lambda ($1 . $rest) `(add-assn ,$1))
   ;; operator => "-="
   (lambda ($1 . $rest) `(sub-assn ,$1))
   ;; operator => "*="
   (lambda ($1 . $rest) `(mul-assn ,$1))
   ;; operator => "/="
   (lambda ($1 . $rest) `(div-assn ,$1))
   ;; operator => "%="
   (lambda ($1 . $rest) `(mod-assn ,$1))
   ;; operator => "^="
   (lambda ($1 . $rest) `(xor-assn ,$1))
   ;; operator => "&="
   (lambda ($1 . $rest) `(and-assn ,$1))
   ;; operator => "|="
   (lambda ($1 . $rest) `(or-assn ,$1))
   ;; operator => "<<"
   (lambda ($1 . $rest) `(shl ,$1))
   ;; operator => ">>"
   (lambda ($1 . $rest) `(shr ,$1))
   ;; operator => ">>="
   (lambda ($1 . $rest) `(rshift-assn ,$1))
   ;; operator => "<<="
   (lambda ($1 . $rest) `(lshift-assn ,$1))
   ;; operator => "=="
   (lambda ($1 . $rest) `(eq ,$1))
   ;; operator => "!="
   (lambda ($1 . $rest) `(ne ,$1))
   ;; operator => "<="
   (lambda ($1 . $rest) `(le ,$1))
   ;; operator => ">="
   (lambda ($1 . $rest) `(ge ,$1))
   ;; operator => "&&"
   (lambda ($1 . $rest) `(and ,$1))
   ;; operator => "||"
   (lambda ($1 . $rest) `(or ,$1))
   ;; operator => "++"
   (lambda ($1 . $rest) `(inc ,$1))
   ;; operator => "--"
   (lambda ($1 . $rest) `(dec ,$1))
   ;; operator => ","
   (lambda ($1 . $rest) `(comma ,$1))
   ;; operator => "->*"
   (lambda ($1 . $rest) `(arrow-star ,$1))
   ;; operator => "->"
   (lambda ($1 . $rest) `(i-sel ,$1))
   ;; operator => "(" ")"
   (lambda ($2 $1 . $rest) `(todo1 ,$1))
   ;; operator => "[" "]"
   (lambda ($2 $1 . $rest) `(todo2 ,$1))
   ;; template-declaration => template-parameter-clause declaration
   (lambda ($2 $1 . $rest) $1)
   ;; template-declaration => "export" template-declaration
   (lambda ($2 $1 . $rest) $1)
   ;; template-parameter-clause => "template" "<" template-parameter-list ">"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; template-parameter-list => template-parameter
   (lambda ($1 . $rest) $1)
   ;; template-parameter-list => template-parameter-list "," template-param...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; template-parameter => simple-type-parameter
   (lambda ($1 . $rest) $1)
   ;; template-parameter => simple-type-parameter "=" type-id
   (lambda ($3 $2 $1 . $rest) $1)
   ;; template-parameter => templated-type-parameter
   (lambda ($1 . $rest) $1)
   ;; template-parameter => templated-type-parameter "=" identifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; template-parameter => templated-parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; template-parameter => bang '$error
   (lambda ($2 $1 . $rest)
     (UNBANG "Bad template-parameter."))
   ;; simple-type-parameter => "class"
   (lambda ($1 . $rest) $1)
   ;; simple-type-parameter => "typename"
   (lambda ($1 . $rest) $1)
   ;; templated-type-parameter => template-parameter-clause "class"
   (lambda ($2 $1 . $rest) $1)
   ;; templated-type-parameter => template-parameter-clause "class" identifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; template-id => "template" identifier "<" template-argument-list ">"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; template-id => "template" template-id
   (lambda ($2 $1 . $rest) $1)
   ;; template-argument-list => template-argument
   (lambda ($1 . $rest) $1)
   ;; template-argument-list => template-argument-list "," template-argument
   (lambda ($3 $2 $1 . $rest) $1)
   ;; template-argument => templated-parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; explicit-specialization => "template" "<" ">" declaration
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; try-block => "try" compound-statement handler-seq
   (lambda ($3 $2 $1 . $rest) $1)
   ;; handler-seq => handler
   (lambda ($1 . $rest) $1)
   ;; handler-seq => handler handler-seq
   (lambda ($2 $1 . $rest) $1)
   ;; handler => "catch" "(" exception-declaration ")" compound-statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; exception-declaration => parameter-declaration
   (lambda ($1 . $rest) $1)
   ;; throw-expression => "throw"
   (lambda ($1 . $rest) $1)
   ;; throw-expression => "throw" assignment-expression
   (lambda ($2 $1 . $rest) $1)
   ;; templated-throw-expression => "throw"
   (lambda ($1 . $rest) $1)
   ;; templated-throw-expression => "throw" templated-assignment-expression
   (lambda ($2 $1 . $rest) $1)
   ;; exception-specification => "throw" "(" ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; exception-specification => "throw" "(" type-id-list ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; type-id-list => type-id
   (lambda ($1 . $rest) $1)
   ;; type-id-list => type-id-list "," type-id
   (lambda ($3 $2 $1 . $rest) $1)
   ;; advance-search => '$error
   (lambda ($1 . $rest)
     (yyerrok)
     (yyclearin)
     (error)
     (advance-search))
   ;; bang => 
   (lambda $rest (BANG))
   ;; mark => 
   (lambda $rest (mark))
   ;; nest => 
   (lambda $rest (nest))
   ;; start-search => 
   (lambda $rest (start-search #f))
   ;; start-search1 => 
   (lambda $rest (start-search #t))
   ;; util => 
   (lambda $rest (list))
   ))

;;; end tables
