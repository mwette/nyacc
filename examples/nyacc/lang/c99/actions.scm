;; actions.scm

;; Copyright (C) 2015 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define act-v
  (vector
   ;; $start => translation-unit-proxy
   (lambda ($1 . $rest) $1)
   ;; translation-unit-proxy => translation-unit
   (lambda ($1 . $rest) (tl->list $1))
   ;; declaration => declaration-specifiers initialized-declarator-list $P1...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $5) (append $3 (list $5)) $3))
   ;; declaration => structure-type-reference ";"
   (lambda ($2 $1 . $rest) $1)
   ;; declaration => union-type-reference ";"
   (lambda ($2 $1 . $rest) $1)
   ;; $P1 => 
   (lambda ($2 $1 . $rest)
     (save-typenames
       `(decl ,(tl->list $1) ,(tl->list $2))))
   ;; declaration-specifiers => storage-class-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => storage-class-specifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => type-specifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => type-qualifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => function-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list `(fctn-spec ,$1)))
   ;; declaration-specifiers => function-specifier declaration-specifiers
   (lambda ($2 $1 . $rest)
     (tl-insert $2 `(fctn-spec ,$1)))
   ;; initialized-declarator-list => initialized-declarator
   (lambda ($1 . $rest)
     (make-tl 'init-declr-list $1))
   ;; initialized-declarator-list => initialized-declarator-list "," initia...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; initialized-declarator => declarator
   (lambda ($1 . $rest) `(init-declr ,$1))
   ;; initialized-declarator => declarator "=" initializer
   (lambda ($3 $2 $1 . $rest) `(init-declr ,$1 ,$3))
   ;; storage-class-specifier => "auto"
   (lambda ($1 . $rest) '(stor-spec (auto)))
   ;; storage-class-specifier => "extern"
   (lambda ($1 . $rest) '(stor-spec (extern)))
   ;; storage-class-specifier => "register"
   (lambda ($1 . $rest) '(stor-spec (register)))
   ;; storage-class-specifier => "static"
   (lambda ($1 . $rest) '(stor-spec (static)))
   ;; storage-class-specifier => "typedef"
   (lambda ($1 . $rest) '(stor-spec (typedef)))
   ;; function-specifier => "inline"
   (lambda ($1 . $rest) $1)
   ;; type-specifier => enumeration-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => floating-point-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => integer-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => structure-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => typedef-name
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => union-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => void-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-qualifier => "const"
   (lambda ($1 . $rest) '(type-qual (const)))
   ;; type-qualifier => "volatile"
   (lambda ($1 . $rest) '(type-qual (volatile)))
   ;; type-qualifier => "restrict"
   (lambda ($1 . $rest) '(type-qual (restrict)))
   ;; declarator => pointer-declarator
   (lambda ($1 . $rest) $1)
   ;; declarator => direct-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-declarator => simple-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-declarator => "(" declarator ")"
   (lambda ($3 $2 $1 . $rest) `(scope ,$2))
   ;; direct-declarator => function-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-declarator => array-declarator
   (lambda ($1 . $rest) $1)
   ;; simple-declarator => identifier
   (lambda ($1 . $rest) $1)
   ;; pointer-declarator => pointer direct-declarator
   (lambda ($2 $1 . $rest) `(ptr-declr ,$1 ,$2))
   ;; pointer => "*" type-qualifier-list
   (lambda ($2 $1 . $rest) `(pointer ,$2))
   ;; pointer => "*"
   (lambda ($1 . $rest) '(pointer))
   ;; pointer => "*" type-qualifier-list pointer
   (lambda ($3 $2 $1 . $rest)
     `(pointer ,(tl->list $2) ,$3))
   ;; pointer => "*" pointer
   (lambda ($2 $1 . $rest) `(pointer ,$2))
   ;; type-qualifier-list => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'type-qual-list $1))
   ;; type-qualifier-list => type-qualifier-list type-qualifier
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; array-declarator => direct-declarator "[" array-qualifier-list array-...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$2 ,$3))
   ;; array-declarator => direct-declarator "[" array-qualifier-list "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3))
   ;; array-declarator => direct-declarator "[" array-size-expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3))
   ;; array-declarator => direct-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(array-of ,$1))
   ;; array-declarator => direct-declarator "[" array-qualifier-list "*" "]"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3 "*???"))
   ;; array-declarator => direct-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 "*???"))
   ;; array-qualifier-list => array-qualifier
   (lambda ($1 . $rest)
     (make-tl 'array-qual-list $1))
   ;; array-qualifier-list => array-qualifier-list array-qualifier
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; array-qualifier => "static"
   (lambda ($1 . $rest) '(static))
   ;; array-qualifier => "restrict"
   (lambda ($1 . $rest) '(restrict))
   ;; array-qualifier => "const"
   (lambda ($1 . $rest) '(const))
   ;; array-qualifier => "volatile"
   (lambda ($1 . $rest) '(volatile))
   ;; array-size-expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; function-declarator => direct-declarator "(" parameter-type-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,(tl->list $3)))
   ;; function-declarator => direct-declarator "(" identifier-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,(tl->list $3)))
   ;; function-declarator => direct-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest) `(ftn-declr ,$1))
   ;; parameter-type-list => parameter-list
   (lambda ($1 . $rest) $1)
   ;; parameter-type-list => parameter-list "," "..."
   (lambda ($3 $2 $1 . $rest) $1)
   ;; parameter-list => parameter-declaration
   (lambda ($1 . $rest) (make-tl 'param-list $1))
   ;; parameter-list => parameter-list "," parameter-declaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; parameter-declaration => declaration-specifiers declarator
   (lambda ($2 $1 . $rest)
     `(param-decln ,(tl->list $1) ,$2))
   ;; parameter-declaration => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest)
     `(param-decln ,(tl->list $1) ,$2))
   ;; parameter-declaration => declaration-specifiers
   (lambda ($1 . $rest)
     `(param-decln ,(tl->list $1)))
   ;; identifier-list => identifier
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; identifier-list => identifier-list "," identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; initializer => assignment-expression
   (lambda ($1 . $rest) `(initzer ,$1))
   ;; initializer => "{" initializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; initializer => "{" initializer-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; initializer-list => initializer
   (lambda ($1 . $rest) (make-tl 'initzer-list $1))
   ;; initializer-list => initializer-list "," initializer
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; initializer-list => designation initializer
   (lambda ($2 $1 . $rest)
     (make-tl 'initzer-list $1 $2))
   ;; initializer-list => initializer-list "," designation initializer
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 $3 $4))
   ;; designation => designator-list "="
   (lambda ($2 $1 . $rest) `(desig ,$1))
   ;; designator-list => designator
   (lambda ($1 . $rest) (make-tl 'desgr-list $1))
   ;; designator-list => designator-list designator
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; designator => "[" constant-expression "]"
   (lambda ($3 $2 $1 . $rest) (list 'array-dsgr $2))
   ;; designator => "." identifier
   (lambda ($2 $1 . $rest) (list 'sel-dsgr $2))
   ;; integer-type-specifier => signed-type-specifier
   (lambda ($1 . $rest) $1)
   ;; integer-type-specifier => unsigned-type-specifier
   (lambda ($1 . $rest) $1)
   ;; integer-type-specifier => character-type-specifier
   (lambda ($1 . $rest) $1)
   ;; integer-type-specifier => bool-type-specifier
   (lambda ($1 . $rest) $1)
   ;; signed-type-specifier => "short"
   (lambda ($1 . $rest) '(fixed-type "short"))
   ;; signed-type-specifier => "short" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "short int"))
   ;; signed-type-specifier => "signed" "short"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed short"))
   ;; signed-type-specifier => "signed" "short" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed short int"))
   ;; signed-type-specifier => "int"
   (lambda ($1 . $rest) '(fixed-type "int"))
   ;; signed-type-specifier => "signed"
   (lambda ($1 . $rest) '(fixed-type "signed"))
   ;; signed-type-specifier => "signed" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed int"))
   ;; signed-type-specifier => "long"
   (lambda ($1 . $rest) '(fixed-type "long"))
   ;; signed-type-specifier => "long" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "long int"))
   ;; signed-type-specifier => "signed" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed long"))
   ;; signed-type-specifier => "signed" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed long int"))
   ;; signed-type-specifier => "long" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "long long"))
   ;; signed-type-specifier => "long" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "long long int"))
   ;; signed-type-specifier => "signed" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed long long"))
   ;; signed-type-specifier => "signed" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "signed long long int"))
   ;; unsigned-type-specifier => "unsigned" "short" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned short int"))
   ;; unsigned-type-specifier => "unsigned" "short"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; unsigned-type-specifier => "unsigned" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned int"))
   ;; unsigned-type-specifier => "unsigned"
   (lambda ($1 . $rest) '(fixed-type "unsigned"))
   ;; unsigned-type-specifier => "unsigned" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; unsigned-type-specifier => "unsigned" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; unsigned-type-specifier => "unsigned" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "unsigned long long int"))
   ;; unsigned-type-specifier => "unsigned" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long long"))
   ;; character-type-specifier => "char"
   (lambda ($1 . $rest) '(fixed-type "char"))
   ;; character-type-specifier => "signed" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed char"))
   ;; character-type-specifier => "unsigned" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned char"))
   ;; bool-type-specifier => "_Bool"
   (lambda ($1 . $rest) '(fixed-type "_Bool"))
   ;; floating-point-type-specifier => "float"
   (lambda ($1 . $rest) '(float-type "float"))
   ;; floating-point-type-specifier => "double"
   (lambda ($1 . $rest) '(float-type "double"))
   ;; floating-point-type-specifier => "long" "double"
   (lambda ($2 $1 . $rest)
     '(float-type "long double"))
   ;; floating-point-type-specifier => complex-type-specifier
   (lambda ($1 . $rest) $1)
   ;; complex-type-specifier => "_Complex"
   (lambda ($1 . $rest) '(complex-type "_Complex"))
   ;; complex-type-specifier => "float" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "float _Complex"))
   ;; complex-type-specifier => "double" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "double _Complex"))
   ;; complex-type-specifier => "long" "double" "_Complex"
   (lambda ($3 $2 $1 . $rest)
     '(complex-type "long double _Complex"))
   ;; enumeration-type-specifier => enumeration-type-definition
   (lambda ($1 . $rest) $1)
   ;; enumeration-type-specifier => enumeration-type-reference
   (lambda ($1 . $rest) $1)
   ;; enumeration-type-definition => "enum" enumeration-tag "{" enumeration...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$1 ,(tl->list $4)))
   ;; enumeration-type-definition => "enum" "{" enumeration-definition-list...
   (lambda ($4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; enumeration-type-definition => "enum" enumeration-tag "{" enumeration...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$1 ,(tl->list $4)))
   ;; enumeration-type-definition => "enum" "{" enumeration-definition-list...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; enumeration-type-reference => "enum" enumeration-tag
   (lambda ($2 $1 . $rest) `(enum-ref ,$2))
   ;; enumeration-tag => identifier
   (lambda ($1 . $rest) $1)
   ;; enumeration-definition-list => enumeration-constant-definition
   (lambda ($1 . $rest) (make-tl 'enum-def-list $1))
   ;; enumeration-definition-list => enumeration-definition-list "," enumer...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; enumeration-constant-definition => enumeration-constant
   (lambda ($1 . $rest) `(enum-defn ,$1))
   ;; enumeration-constant-definition => enumeration-constant "=" constant-...
   (lambda ($3 $2 $1 . $rest) `(enum-defn ,$1 ,$3))
   ;; enumeration-constant => identifier
   (lambda ($1 . $rest) $1)
   ;; structure-type-specifier => structure-type-definition
   (lambda ($1 . $rest) $1)
   ;; structure-type-specifier => structure-type-reference
   (lambda ($1 . $rest) $1)
   ;; structure-type-definition => "struct" structure-tag "{" opt-lone-comm...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(struct-def ,$1 ,(tl->list (tl-insert $5 $4))))
   ;; structure-type-definition => "struct" "{" opt-lone-comment field-list...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(struct-def ,(tl->list (tl-insert $4 $3))))
   ;; structure-type-reference => "struct" structure-tag
   (lambda ($2 $1 . $rest) `(struct-ref ,$2))
   ;; structure-tag => identifier
   (lambda ($1 . $rest) $1)
   ;; field-list => component-declaration
   (lambda ($1 . $rest) (make-tl 'field-list $1))
   ;; field-list => field-list component-declaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; field-list => field-list lone-comment
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; component-declaration => type-specifier component-declarator-list ";"...
   (lambda ($4 $3 $2 $1 . $rest)
     (if (pair? $4)
       `(comp-decln ,$1 ,(tl->list $2) ,$4)
       `(comp-decln ,$1 ,(tl->list $2))))
   ;; component-declarator-list => component-declarator
   (lambda ($1 . $rest)
     (make-tl 'comp-declr-list $1))
   ;; component-declarator-list => component-declarator-list "," component-...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; component-declarator => simple-component
   (lambda ($1 . $rest) $1)
   ;; component-declarator => bit-field
   (lambda ($1 . $rest) $1)
   ;; simple-component => declarator
   (lambda ($1 . $rest) $1)
   ;; bit-field => declarator ":" width
   (lambda ($3 $2 $1 . $rest) `(bit-field ,$1 ,$3))
   ;; bit-field => ":" width
   (lambda ($2 $1 . $rest) `(bit-field ,$2))
   ;; width => constant-expression
   (lambda ($1 . $rest) $1)
   ;; union-type-specifier => union-type-definition
   (lambda ($1 . $rest) $1)
   ;; union-type-specifier => union-type-reference
   (lambda ($1 . $rest) $1)
   ;; union-type-definition => "union" union-tag "{" field-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(union-def ,$1 ,(tl->list $4)))
   ;; union-type-definition => "union" "{" field-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(union-def ,(tl->list $3)))
   ;; union-type-reference => "union" union-tag
   (lambda ($2 $1 . $rest) `(union-ref ,$1))
   ;; union-tag => identifier
   (lambda ($1 . $rest) $1)
   ;; void-type-specifier => "void"
   (lambda ($1 . $rest) '(void))
   ;; typedef-name => 'typename
   (lambda ($1 . $rest) `(typename ,$1))
   ;; type-name => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest)
     `(type-name ,(tl->list $1) ,$2))
   ;; type-name => declaration-specifiers
   (lambda ($1 . $rest) `(type-name ,(tl->list $1)))
   ;; abstract-declarator => pointer
   (lambda ($1 . $rest) `(abs-declr ,$1))
   ;; abstract-declarator => pointer direct-abstract-declarator
   (lambda ($2 $1 . $rest) `(abs-declr ,$1 ,$2))
   ;; abstract-declarator => direct-abstract-declarator
   (lambda ($1 . $rest) `(abs-declr ,$1))
   ;; direct-abstract-declarator => "(" abstract-declarator ")"
   (lambda ($3 $2 $1 . $rest) `(declr-scope ,$2))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(declr-array ,$1))
   ;; direct-abstract-declarator => "[" "]"
   (lambda ($2 $1 . $rest) $1)
   ;; direct-abstract-declarator => direct-abstract-declarator "[" expressi...
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-arry ,$1 ,$3))
   ;; direct-abstract-declarator => "[" expression "]"
   (lambda ($3 $2 $1 . $rest)
     `(declr-anon-arry ,$2))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest) `(declr-STAR ,$1))
   ;; direct-abstract-declarator => "[" "*" "]"
   (lambda ($3 $2 $1 . $rest) '(declr-STAR))
   ;; direct-abstract-declarator => direct-abstract-declarator "(" paramete...
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-fctn ,$1 ,(tl->list $3)))
   ;; direct-abstract-declarator => direct-abstract-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest) `(declr-fctn ,$1))
   ;; direct-abstract-declarator => "(" parameter-type-list ")"
   (lambda ($3 $2 $1 . $rest)
     `(declr-anon-fctn ,(tl->list $2)))
   ;; direct-abstract-declarator => "(" ")"
   (lambda ($2 $1 . $rest) '(declr-anon-fctn))
   ;; primary-expression => identifier
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => constant
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => parenthesized-expression
   (lambda ($1 . $rest) $1)
   ;; parenthesized-expression => "(" expression ")"
   (lambda ($3 $2 $1 . $rest) `(p-expr ,$2))
   ;; postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => subscript-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => component-selection-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => function-call
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postincrement-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postdecrement-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => compound-literal
   (lambda ($1 . $rest) $1)
   ;; subscript-expression => postfix-expression "[" expression "]"
   (lambda ($4 $3 $2 $1 . $rest) '(FIX))
   ;; component-selection-expression => direct-component-selection
   (lambda ($1 . $rest) $1)
   ;; component-selection-expression => indirect-component-selection
   (lambda ($1 . $rest) $1)
   ;; direct-component-selection => postfix-expression "." identifier
   (lambda ($3 $2 $1 . $rest) `(d-sel ,$3 ,$1))
   ;; indirect-component-selection => postfix-expression "->" identifier
   (lambda ($3 $2 $1 . $rest) `(i-sel ,$3 ,$1))
   ;; function-call => postfix-expression "(" expression-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-call ,$1 ,$2))
   ;; function-call => postfix-expression "(" ")"
   (lambda ($3 $2 $1 . $rest) `(fctn-call ,$1))
   ;; expression-list => assignment-expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expression-list => expression-list "," assignment-expression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; postincrement-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; postdecrement-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; compound-literal => "(" type-name ")" "{" initializer-list "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(comp-literal ,$2 ,(tl->list $5)))
   ;; compound-literal => "(" type-name ")" "{" initializer-list "," "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(comp-literal ,$2 ,(tl->list $5)))
   ;; cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; cast-expression => "(" type-name ")" cast-expression
   (lambda ($4 $3 $2 $1 . $rest) `(cast ,$2 ,$4))
   ;; unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => sizeof-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => unary-minus-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => unary-plus-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => logical-negation-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => bitwise-negation-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => address-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => indirection-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => preincrement-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => predecrement-expression
   (lambda ($1 . $rest) $1)
   ;; sizeof-expression => "sizeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(sizeof-type ,$3))
   ;; sizeof-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) `(sizeof-expr ,$2))
   ;; unary-minus-expression => "-" cast-expression
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-plus-expression => "+" cast-expression
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; logical-negation-expression => "!" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; bitwise-negation-expression => "~" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; address-expression => "&" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; indirection-expression => "*" cast-expression
   (lambda ($2 $1 . $rest) $1)
   ;; preincrement-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; predecrement-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; multiplicative-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression mult-op cast-e...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; mult-op => "*"
   (lambda ($1 . $rest) 'mul)
   ;; mult-op => "/"
   (lambda ($1 . $rest) 'div)
   ;; mult-op => "%"
   (lambda ($1 . $rest) 'mod)
   ;; additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; additive-expression => additive-expression add-op multiplicative-expr...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; add-op => "+"
   (lambda ($1 . $rest) 'add)
   ;; add-op => "-"
   (lambda ($1 . $rest) 'sub)
   ;; shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; shift-expression => shift-expression shift-op additive-expression
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; shift-op => "<<"
   (lambda ($1 . $rest) 'lshift)
   ;; shift-op => ">>"
   (lambda ($1 . $rest) 'rshift)
   ;; relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; relational-expression => relational-expression relational-op shift-ex...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; relational-op => "<"
   (lambda ($1 . $rest) 'lt)
   ;; relational-op => "<="
   (lambda ($1 . $rest) 'le)
   ;; relational-op => ">"
   (lambda ($1 . $rest) 'gt)
   ;; relational-op => ">="
   (lambda ($1 . $rest) 'ge)
   ;; equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; equality-expression => equality-expression equality-op relational-exp...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; equality-op => "=="
   (lambda ($1 . $rest) 'eq)
   ;; equality-op => "!="
   (lambda ($1 . $rest) 'ne)
   ;; bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-xor ,$1 ,$3))
   ;; bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-and ,$1 ,$3))
   ;; logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; conditional-expression => logical-or-expression "?" expression ":" co...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr $1 $2 $3))
   ;; assignment-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; assignment-expression => unary-expression assignment-op assignment-ex...
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; assignment-op => "="
   (lambda ($1 . $rest) 'assign)
   ;; assignment-op => "+="
   (lambda ($1 . $rest) 'add-assign)
   ;; assignment-op => "-="
   (lambda ($1 . $rest) 'sub-assign)
   ;; assignment-op => "*="
   (lambda ($1 . $rest) 'mul-assign)
   ;; assignment-op => "/="
   (lambda ($1 . $rest) 'div-assign)
   ;; assignment-op => "%="
   (lambda ($1 . $rest) 'mod-assign)
   ;; assignment-op => "<<="
   (lambda ($1 . $rest) 'lshift-assign)
   ;; assignment-op => ">>="
   (lambda ($1 . $rest) 'rshift-assign)
   ;; assignment-op => "&="
   (lambda ($1 . $rest) 'and-assign)
   ;; assignment-op => "^="
   (lambda ($1 . $rest) 'xor-assign)
   ;; assignment-op => "|="
   (lambda ($1 . $rest) 'or-assign)
   ;; comma-expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; comma-expression => comma-expression "," assignment-expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; expression => comma-expression
   (lambda ($1 . $rest) $1)
   ;; constant-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; statement => expression-statement
   (lambda ($1 . $rest) $1)
   ;; statement => labeled-statement
   (lambda ($1 . $rest) $1)
   ;; statement => compound-statement
   (lambda ($1 . $rest) $1)
   ;; statement => conditional-statement
   (lambda ($1 . $rest) $1)
   ;; statement => iterative-statement
   (lambda ($1 . $rest) $1)
   ;; statement => switch-statement
   (lambda ($1 . $rest) $1)
   ;; statement => break-statement
   (lambda ($1 . $rest) $1)
   ;; statement => continue-statement
   (lambda ($1 . $rest) $1)
   ;; statement => return-statement
   (lambda ($1 . $rest) $1)
   ;; statement => goto-statement
   (lambda ($1 . $rest) $1)
   ;; statement => null-statement
   (lambda ($1 . $rest) $1)
   ;; expression-statement => expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; labeled-statement => label ":" statement
   (lambda ($3 $2 $1 . $rest) $1)
   ;; label => named-label
   (lambda ($1 . $rest) $1)
   ;; label => case-label
   (lambda ($1 . $rest) $1)
   ;; label => default-label
   (lambda ($1 . $rest) $1)
   ;; compound-statement => "{" declaration-or-statement-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; compound-statement => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; declaration-or-statement-list => declaration-or-statement
   (lambda ($1 . $rest) $1)
   ;; declaration-or-statement-list => declaration-or-statement-list declar...
   (lambda ($2 $1 . $rest) $1)
   ;; declaration-or-statement => declaration
   (lambda ($1 . $rest) $1)
   ;; declaration-or-statement => statement
   (lambda ($1 . $rest) $1)
   ;; conditional-statement => if-statement
   (lambda ($1 . $rest) $1)
   ;; conditional-statement => if-else-statement
   (lambda ($1 . $rest) $1)
   ;; if-statement => "if" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$3 ,$5))
   ;; if-else-statement => "if" "(" expression ")" statement "else" statement
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$3 ,$5 ,7))
   ;; iterative-statement => while-statement
   (lambda ($1 . $rest) $1)
   ;; iterative-statement => do-statement
   (lambda ($1 . $rest) $1)
   ;; iterative-statement => for-statement
   (lambda ($1 . $rest) $1)
   ;; while-statement => "while" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; do-statement => "do" statement "while" "(" expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(do-while ,$2 ,$5))
   ;; for-statement => "for" for-expressions statement
   (lambda ($3 $2 $1 . $rest) `(for ,@$2 $3))
   ;; for-expressions => "(" initial-clause expression ";" expression ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (list $2 $3 $5))
   ;; for-expressions => "(" initial-clause expression ";" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (list $2 $3 '(expr)))
   ;; for-expressions => "(" initial-clause ";" expression ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (list $2 '(expr) $4))
   ;; for-expressions => "(" initial-clause ";" ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (list $2 '(expr) '(expr)))
   ;; initial-clause => expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; initial-clause => ";"
   (lambda ($1 . $rest) '(expr))
   ;; initial-clause => declaration
   (lambda ($1 . $rest) $1)
   ;; switch-statement => "switch" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; case-label => "case" constant-expression
   (lambda ($2 $1 . $rest) $1)
   ;; default-label => "default"
   (lambda ($1 . $rest) $1)
   ;; break-statement => "break" ";"
   (lambda ($2 $1 . $rest) '(break))
   ;; continue-statement => "continue" ";"
   (lambda ($2 $1 . $rest) '(continue))
   ;; return-statement => "return" expression ";"
   (lambda ($3 $2 $1 . $rest) `(return $2))
   ;; return-statement => "return" ";"
   (lambda ($2 $1 . $rest) `(return (expr)))
   ;; goto-statement => "goto" named-label ";"
   (lambda ($3 $2 $1 . $rest) `(goto $2))
   ;; named-label => identifier
   (lambda ($1 . $rest) $1)
   ;; null-statement => ";"
   (lambda ($1 . $rest) '(null-stmt))
   ;; translation-unit => top-level-declaration
   (lambda ($1 . $rest) (make-tl 'trans-unit $1))
   ;; translation-unit => translation-unit top-level-declaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; top-level-declaration => declaration
   (lambda ($1 . $rest) $1)
   ;; top-level-declaration => function-definition
   (lambda ($1 . $rest) $1)
   ;; top-level-declaration => lone-comment
   (lambda ($1 . $rest) $1)
   ;; top-level-declaration => cpp-statement
   (lambda ($1 . $rest) $1)
   ;; top-level-declaration => "extern" '$string "{" translation-unit "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $4)
   ;; function-definition => function-def-specifier compound-statement
   (lambda ($2 $1 . $rest) $1)
   ;; function-def-specifier => declaration-specifiers declarator declarati...
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn1a ,(tl->list $1) ,$2 ,(tl->list $3)))
   ;; function-def-specifier => declaration-specifiers declarator
   (lambda ($2 $1 . $rest)
     `(fctn-defn1b ,(tl->list $1) ,$2))
   ;; function-def-specifier => declarator
   (lambda ($1 . $rest) `(fctn-defn1c ,$1))
   ;; declaration-list => declaration
   (lambda ($1 . $rest) (make-tl $1))
   ;; declaration-list => declaration-list declaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; opt-code-comment => 
   (lambda $rest (list))
   ;; opt-code-comment => code-comment
   (lambda ($1 . $rest) $1)
   ;; opt-lone-comment => 
   (lambda $rest (list))
   ;; opt-lone-comment => lone-comment
   (lambda ($1 . $rest) $1)
   ;; identifier => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; identifier => 'cpp-ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; constant => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; constant => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; constant => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; constant => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; code-comment => '$code-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; cpp-statement => 'cpp-stmt
   (lambda ($1 . $rest) `(cpp-stmt ,$1))
   ))

;;; end tables
