;; ./mach.d/mo-act.scm

;; Copyright (c) 2015-2018 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING.LESSER included with the this distribution.

(define mo-act-v
  (vector
   ;; $start => stored-definition
   (lambda ($1 . $rest) $1)
   ;; stored-definition => 
   (lambda $rest `(stored-defn))
   ;; stored-definition => stored-definition-1 stored-definition-2
   (lambda ($2 $1 . $rest) `(stored-defn ,$1 ,$2))
   ;; stored-definition => stored-definition-2
   (lambda ($1 . $rest) (tl->list $1))
   ;; stored-definition-1 => "within" name ";"
   (lambda ($3 $2 $1 . $rest) `(within ,$2))
   ;; stored-definition-1 => "within" ";"
   (lambda ($2 $1 . $rest) '(within))
   ;; stored-definition-2 => "final" class-definition ";"
   (lambda ($3 $2 $1 . $rest)
     (make-tl
       'stored-defn
       (sx-attr-add $2 'final "yes")))
   ;; stored-definition-2 => class-definition ";"
   (lambda ($2 $1 . $rest)
     (make-tl 'stored-defn $1))
   ;; stored-definition-2 => stored-definition-2 "final" class-definition ";"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $3 (sx-attr-add $3 'final "yes")))
   ;; stored-definition-2 => stored-definition-2 class-definition ";"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; class-definition => "encapsulated" class-prefixes class-specifier
   (lambda ($3 $2 $1 . $rest)
     (append
       (tl->list (tl+attr $2 'encapsulated "yes"))
       $3))
   ;; class-definition => class-prefixes class-specifier
   (lambda ($2 $1 . $rest)
     (append (tl->list $1) $2))
   ;; class-prefixes => "partial" class-prefixes-1
   (lambda ($2 $1 . $rest)
     (tl+attr (make-tl $2) 'partial "yes"))
   ;; class-prefixes => class-prefixes-1
   (lambda ($1 . $rest) (make-tl $1))
   ;; class-prefixes-1 => "class"
   (lambda ($1 . $rest) 'class)
   ;; class-prefixes-1 => "model"
   (lambda ($1 . $rest) 'model)
   ;; class-prefixes-1 => "operator" "record"
   (lambda ($2 $1 . $rest) 'operator-record)
   ;; class-prefixes-1 => "record"
   (lambda ($1 . $rest) 'record)
   ;; class-prefixes-1 => "block"
   (lambda ($1 . $rest) 'block)
   ;; class-prefixes-1 => "expandable" "connector"
   (lambda ($2 $1 . $rest) 'expandable-connector)
   ;; class-prefixes-1 => "connector"
   (lambda ($1 . $rest) 'connector)
   ;; class-prefixes-1 => "type"
   (lambda ($1 . $rest) 'type)
   ;; class-prefixes-1 => "package"
   (lambda ($1 . $rest) 'package)
   ;; class-prefixes-1 => "impure" "operator" "function"
   (lambda ($3 $2 $1 . $rest)
     'impure-operator-function)
   ;; class-prefixes-1 => "pure" "operator" "function"
   (lambda ($3 $2 $1 . $rest)
     'pure-operator-function)
   ;; class-prefixes-1 => "impure" "function"
   (lambda ($2 $1 . $rest) 'impure-function)
   ;; class-prefixes-1 => "pure" "function"
   (lambda ($2 $1 . $rest) 'pure-function)
   ;; class-prefixes-1 => "operator" "function"
   (lambda ($2 $1 . $rest) 'operator-function)
   ;; class-prefixes-1 => "function"
   (lambda ($1 . $rest) 'function)
   ;; class-prefixes-1 => "operator"
   (lambda ($1 . $rest) 'operator)
   ;; class-specifier => long-class-specifier
   (lambda ($1 . $rest) $1)
   ;; class-specifier => short-class-specifier
   (lambda ($1 . $rest) $1)
   ;; class-specifier => der-class-specifier
   (lambda ($1 . $rest) $1)
   ;; long-class-specifier => ident string-comment composition "end" ident
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (check-ids $1 $5)
     (list $1 $3))
   ;; long-class-specifier => "extends" ident class-modification string-com...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (check-ids $1 $5)
     (list '(@ extends . "yes") $2 $3 $4 $5))
   ;; long-class-specifier => "extends" ident string-comment composition "e...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (check-ids $1 $5)
     (list '(@ extends . "yes") $2 $3 $4))
   ;; short-class-specifier => ident "=" base-prefix type-specifier array-s...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(short-class-spec ,$1 ,$3 ,$4 ,$5 ,$6))
   ;; short-class-specifier => ident "=" base-prefix type-specifier array-s...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(short-class-spec ,$1 ,$3 ,$4 ,$5))
   ;; short-class-specifier => ident "=" base-prefix type-specifier class-m...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(short-class-spec ,$1 ,$3 ,$4 ,$5))
   ;; short-class-specifier => ident "=" base-prefix type-specifier comment
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(short-class-spec ,$1 ,$3 ,$4))
   ;; short-class-specifier => ident "=" "enumeration" "(" enum-list ")" co...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(short-class-enum-spec ,$1 ,$5))
   ;; short-class-specifier => ident "=" "enumeration" "(" ":" ")" comment
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(short-class-enum-spec ,$1 ,$5))
   ;; der-class-specifier => ident "=" "der" "(" type-specifier "," der-cla...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(der-class-specifier ,$1 ,$5 ,$7))
   ;; der-class-specifier-1 => ident-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ident-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; ident-list-1 => ident-list-1 ";" ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; base-prefix => "input"
   (lambda ($1 . $rest) $1)
   ;; base-prefix => "output"
   (lambda ($1 . $rest) $1)
   ;; enum-list => enumeration-literal
   (lambda ($1 . $rest) (make-tl 'enum-list $1))
   ;; enum-list => enum-list "," enumeration-literal
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; enumeration-literal => ident comment
   (lambda ($2 $1 . $rest) $1)
   ;; composition => element-list composition-1-list external-part opt-anno...
   (lambda ($4 $3 $2 $1 . $rest)
     (if (pair? $4)
       `(composition ,$1 ,$2 ,$3 ,$4)
       `(composition ,$1 ,$2 ,$3)))
   ;; composition => element-list composition-1-list opt-annotation
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $3)
       `(composition ,$1 ,$2 ,$3)
       `(composition ,$1 ,$2)))
   ;; composition => element-list external-part opt-annotation
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $3)
       `(composition ,$1 ,$2 ,$3)
       `(composition ,$1 ,$2)))
   ;; composition => element-list opt-annotation
   (lambda ($2 $1 . $rest)
     (if (pair? $2)
       `(composition ,$1 ,$2)
       `(composition ,$1)))
   ;; composition-1-list => composition-1-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; composition-1-list-1 => composition-1
   (lambda ($1 . $rest)
     (make-tl 'composition-list $1))
   ;; composition-1-list-1 => composition-1-list composition-1
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; composition-1 => "public"
   (lambda ($1 . $rest) $1)
   ;; composition-1 => "public" element-list
   (lambda ($2 $1 . $rest) $1)
   ;; composition-1 => "protected"
   (lambda ($1 . $rest) $1)
   ;; composition-1 => "protected" element-list
   (lambda ($2 $1 . $rest) $1)
   ;; composition-1 => equation-section
   (lambda ($1 . $rest) $1)
   ;; composition-1 => algorithm-section
   (lambda ($1 . $rest) $1)
   ;; external-part => "external" language-specification external-function-...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; external-part => "external" language-specification external-function-...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; external-part => "external" language-specification annotation ";"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; external-part => "external" external-function-call annotation ";"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; external-part => "external" language-specification ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; external-part => "external" external-function-call ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; external-part => "external" annotation ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; external-part => "external" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; language-specification => string
   (lambda ($1 . $rest) $1)
   ;; external-function-call => component-reference "=" ident "(" expressio...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ext-fctn-call ,$1 ,$3 ,$5))
   ;; external-function-call => component-reference "=" ident "(" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ext-fctn-call ,$1 ,$3 '(expr-list)))
   ;; external-function-call => ident "(" expression-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ext-fctn-call ,$1 ,$3))
   ;; external-function-call => ident "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(ext-fctn-call ,$1 '(expr-list)))
   ;; element-list => element-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; element-list-1 => element ";"
   (lambda ($2 $1 . $rest)
     (make-tl 'element-list $1))
   ;; element-list-1 => element-list-1 element ";"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; element => import-clause
   (lambda ($1 . $rest) $1)
   ;; element => extends-clause
   (lambda ($1 . $rest) $1)
   ;; element => "redeclare" $P1 $P2 $P3 element-1
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; element => "final" $P4 $P5 element-1
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; element => "inner" $P6 element-1
   (lambda ($3 $2 $1 . $rest) $1)
   ;; element => "outer" element-1
   (lambda ($2 $1 . $rest) $1)
   ;; element => element-1
   (lambda ($1 . $rest) $1)
   ;; $P1 => 
   (lambda $rest (list))
   ;; $P1 => "final"
   (lambda ($1 . $rest) $1)
   ;; $P2 => 
   (lambda $rest (list))
   ;; $P2 => "inner"
   (lambda ($1 . $rest) $1)
   ;; $P3 => 
   (lambda $rest (list))
   ;; $P3 => "outer"
   (lambda ($1 . $rest) $1)
   ;; $P4 => 
   (lambda $rest (list))
   ;; $P4 => "inner"
   (lambda ($1 . $rest) $1)
   ;; $P5 => 
   (lambda $rest (list))
   ;; $P5 => "outer"
   (lambda ($1 . $rest) $1)
   ;; $P6 => 
   (lambda $rest (list))
   ;; $P6 => "outer"
   (lambda ($1 . $rest) $1)
   ;; element-1 => element-2
   (lambda ($1 . $rest) $1)
   ;; element-1 => "replaceable" element-2 constraining-clause comment
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; element-1 => "replaceable" element-2
   (lambda ($2 $1 . $rest) $1)
   ;; element-2 => class-definition
   (lambda ($1 . $rest) $1)
   ;; element-2 => component-clause
   (lambda ($1 . $rest) $1)
   ;; import-clause => "import" import-clause-1 comment
   (lambda ($3 $2 $1 . $rest) $1)
   ;; import-clause-1 => ident "=" name
   (lambda ($3 $2 $1 . $rest) $1)
   ;; import-clause-1 => name "." import-clause-2
   (lambda ($3 $2 $1 . $rest) $1)
   ;; import-clause-1 => name
   (lambda ($1 . $rest) $1)
   ;; import-clause-2 => "*"
   (lambda ($1 . $rest) $1)
   ;; import-clause-2 => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; import-clause-2 => "{" import-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; import-list => ident
   (lambda ($1 . $rest) $1)
   ;; import-list => import-list "," ident
   (lambda ($3 $2 $1 . $rest) $1)
   ;; extends-clause => "extends" name class-modification annotation
   (lambda ($4 $3 $2 $1 . $rest) `(extends ,$2 ,$3))
   ;; extends-clause => "extends" name class-modification
   (lambda ($3 $2 $1 . $rest) `(extends ,$2 ,$3))
   ;; extends-clause => "extends" name annotation
   (lambda ($3 $2 $1 . $rest) `(extends ,$2))
   ;; extends-clause => "extends" name
   (lambda ($2 $1 . $rest) `(extends ,$2))
   ;; constraining-clause => "constrainedby" name class-modification
   (lambda ($3 $2 $1 . $rest)
     `(constrained-by ,$1 ,$2))
   ;; constraining-clause => "constrainedby" name
   (lambda ($2 $1 . $rest) `(constrained-by ,$1))
   ;; component-clause => type-prefix type-specifier array-subscripts compo...
   (lambda ($4 $3 $2 $1 . $rest)
     `(component-clause ,$1 ,$2 ,$3 ,$4))
   ;; component-clause => type-prefix type-specifier component-list
   (lambda ($3 $2 $1 . $rest)
     `(component-clause ,$1 ,$2 ,$3))
   ;; component-clause => type-specifier array-subscripts component-list
   (lambda ($3 $2 $1 . $rest)
     `(component-clause ,$1 ,$2 ,$3))
   ;; component-clause => type-specifier component-list
   (lambda ($2 $1 . $rest)
     `(component-clause ,$1 ,$2))
   ;; type-prefix => type-prefix-1 type-prefix-2 type-prefix-3
   (lambda ($3 $2 $1 . $rest)
     `(type-prefix ,$1 ,$2 ,$3))
   ;; type-prefix => type-prefix-1 type-prefix-2
   (lambda ($2 $1 . $rest) `(type-prefix ,$1 ,$2))
   ;; type-prefix => type-prefix-1 type-prefix-3
   (lambda ($2 $1 . $rest) `(type-prefix ,$1 ,$2))
   ;; type-prefix => type-prefix-2 type-prefix-3
   (lambda ($2 $1 . $rest) `(type-prefix ,$1 ,$2))
   ;; type-prefix => type-prefix-1
   (lambda ($1 . $rest) `(type-prefix ,$1))
   ;; type-prefix => type-prefix-2
   (lambda ($1 . $rest) `(type-prefix ,$1))
   ;; type-prefix => type-prefix-3
   (lambda ($1 . $rest) `(type-prefix ,$1))
   ;; type-prefix-1 => "flow"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-1 => "stream"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-2 => "discrete"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-2 => "parameter"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-2 => "constant"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-3 => "input"
   (lambda ($1 . $rest) $1)
   ;; type-prefix-3 => "output"
   (lambda ($1 . $rest) $1)
   ;; type-specifier => name
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; component-list => component-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; component-list-1 => component-declaration
   (lambda ($1 . $rest) (make-tl 'comp-list $1))
   ;; component-list-1 => component-list-1 "," component-declaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; component-declaration => declaration condition-attribute comment
   (lambda ($3 $2 $1 . $rest) `(comp-decl ,$1 ,$2))
   ;; component-declaration => declaration comment
   (lambda ($2 $1 . $rest) $1)
   ;; condition-attribute => "if" expression
   (lambda ($2 $1 . $rest) `(if ,$2))
   ;; declaration => ident $P7 $P8
   (lambda ($3 $2 $1 . $rest)
     (make-sx 'decl #f $1 $2 $3))
   ;; $P7 => 
   (lambda $rest (list))
   ;; $P7 => array-subscripts
   (lambda ($1 . $rest) $1)
   ;; $P8 => 
   (lambda $rest (list))
   ;; $P8 => modification
   (lambda ($1 . $rest) $1)
   ;; modification => class-modification "=" expression
   (lambda ($3 $2 $1 . $rest) `(class-mod ,$1 ,$3))
   ;; modification => class-modification
   (lambda ($1 . $rest) `(class-mod ,$1))
   ;; modification => "=" expression
   (lambda ($2 $1 . $rest) `(eqv-mod ,$2))
   ;; modification => ":=" expression
   (lambda ($2 $1 . $rest) `(def-mod ,$2))
   ;; class-modification => "(" argument-list ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; class-modification => "(" ")"
   (lambda ($2 $1 . $rest) '(arg-list))
   ;; argument-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arg-list-1 => argument
   (lambda ($1 . $rest) (make-tl 'arg-list $1))
   ;; arg-list-1 => arg-list-1 "," argument
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; argument => element-modification-or-replaceable
   (lambda ($1 . $rest) $1)
   ;; argument => element-redeclaration
   (lambda ($1 . $rest) $1)
   ;; element-modification-or-replaceable => "each" "final" elt-mod-or-repl-1
   (lambda ($3 $2 $1 . $rest) $3)
   ;; element-modification-or-replaceable => "each" elt-mod-or-repl-1
   (lambda ($2 $1 . $rest) $2)
   ;; element-modification-or-replaceable => "final" elt-mod-or-repl-1
   (lambda ($2 $1 . $rest) $2)
   ;; element-modification-or-replaceable => elt-mod-or-repl-1
   (lambda ($1 . $rest) $1)
   ;; elt-mod-or-repl-1 => element-modification
   (lambda ($1 . $rest) $1)
   ;; elt-mod-or-repl-1 => element-replaceable
   (lambda ($1 . $rest) $1)
   ;; element-modification => name $P9 string-comment
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $2) `(elt-mod ,$1 ,$2) `(elt-mod ,$1)))
   ;; $P9 => 
   (lambda $rest (list))
   ;; $P9 => modification
   (lambda ($1 . $rest) $1)
   ;; element-redeclaration => "redeclare" $P10 $P11 elt-redecl-1
   (lambda ($4 $3 $2 $1 . $rest) $4)
   ;; $P10 => 
   (lambda $rest (list))
   ;; $P10 => "each"
   (lambda ($1 . $rest) $1)
   ;; $P11 => 
   (lambda $rest (list))
   ;; $P11 => "final"
   (lambda ($1 . $rest) $1)
   ;; elt-redecl-1 => short-class-definition
   (lambda ($1 . $rest) $1)
   ;; elt-redecl-1 => component-clause1
   (lambda ($1 . $rest) $1)
   ;; elt-redecl-1 => element-replaceable
   (lambda ($1 . $rest) $1)
   ;; element-replaceable => "replaceable" short-class-definition component...
   (lambda ($4 $3 $2 $1 . $rest)
     `(elt-repl $2 $3 $4))
   ;; element-replaceable => "replaceable" short-class-definition component...
   (lambda ($3 $2 $1 . $rest) `(elt-repl $2 $3))
   ;; component-clause1 => type-prefix type-specifier declaration comment
   (lambda ($4 $3 $2 $1 . $rest)
     (list $1 $2 (append $3 (list $4))))
   ;; short-class-definition => class-prefixes short-class-specifier
   (lambda ($2 $1 . $rest)
     `(short-class-def ,(append $1 (list $2))))
   ;; equation-section => "initial" "equation" equation-list
   (lambda ($3 $2 $1 . $rest)
     `(init-eqn-section unquote (cdr $3)))
   ;; equation-section => "equation" equation-list
   (lambda ($2 $1 . $rest)
     `(eqn-section unquote (cdr $2)))
   ;; equation-section => "initial" "equation"
   (lambda ($2 $1 . $rest) `(init-eqn-section))
   ;; equation-section => "equation"
   (lambda ($1 . $rest) `(eqn-section))
   ;; algorithm-section => "initial" "algorithm" statement-list
   (lambda ($3 $2 $1 . $rest)
     `(init-alg-section unquote (cdr $3)))
   ;; algorithm-section => "algorithm" statement-list
   (lambda ($2 $1 . $rest)
     `(alg-section unquote (cdr $2)))
   ;; algorithm-section => "initial" "algorithm"
   (lambda ($2 $1 . $rest) `(init-alg-section))
   ;; algorithm-section => "algorithm"
   (lambda ($1 . $rest) `(alg-section))
   ;; equation-list => equation-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; equation-list-1 => equation ";"
   (lambda ($2 $1 . $rest) (make-tl 'eqn-list $1))
   ;; equation-list-1 => equation-list-1 equation ";"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; equation => equation-1 comment
   (lambda ($2 $1 . $rest) $1)
   ;; equation-1 => simple-expression "=" expression
   (lambda ($3 $2 $1 . $rest) `(equate ,$1 ,$3))
   ;; equation-1 => if-equation
   (lambda ($1 . $rest) $1)
   ;; equation-1 => for-equation
   (lambda ($1 . $rest) $1)
   ;; equation-1 => connect-clause
   (lambda ($1 . $rest) $1)
   ;; equation-1 => when-equation
   (lambda ($1 . $rest) $1)
   ;; equation-1 => name function-call-args
   (lambda ($2 $1 . $rest) `(fctn ,$1 ,$2))
   ;; statement-list => statement ";"
   (lambda ($2 $1 . $rest) (make-tl 'stmt-list $1))
   ;; statement-list => statement-list statement ";"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; statement => statement-1 comment
   (lambda ($2 $1 . $rest) $1)
   ;; statement-1 => component-reference ":=" expression
   (lambda ($3 $2 $1 . $rest) `(assign ,$1 ,$3))
   ;; statement-1 => component-reference function-call-args
   (lambda ($2 $1 . $rest) `(call ,$1 ,$2))
   ;; statement-1 => "(" output-expression-list ")" ":=" component-referenc...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(multi-assign ,$2 ,$5 ,$6))
   ;; statement-1 => "break"
   (lambda ($1 . $rest) '(break-stmt))
   ;; statement-1 => "return"
   (lambda ($1 . $rest) '(return-stmt))
   ;; statement-1 => if-statement
   (lambda ($1 . $rest) $1)
   ;; statement-1 => for-statement
   (lambda ($1 . $rest) $1)
   ;; statement-1 => while-statement
   (lambda ($1 . $rest) $1)
   ;; statement-1 => when-statement
   (lambda ($1 . $rest) $1)
   ;; if-equation => "if" expression then-eq-part elseif-eq-list else-eq-pa...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if-eq ,$2 ,$3 ,@(cdr (tl->list $4)) ,$5))
   ;; if-equation => "if" expression then-eq-part elseif-eq-list "end" "if"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if-eq ,$2 ,$3 ,@(cdr (tl->list $4))))
   ;; if-equation => "if" expression then-eq-part else-eq-part "end" "if"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if-eq ,$2 ,$3 ,$4))
   ;; if-equation => "if" expression then-eq-part "end" "if"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(if-eq ,$2 ,$3))
   ;; then-eq-part => "then" equation-list
   (lambda ($2 $1 . $rest) $2)
   ;; then-eq-part => "then"
   (lambda ($1 . $rest) '(eqn-list))
   ;; elseif-eq-list => elseif-eq-part
   (lambda ($1 . $rest) (make-tl 'l $1))
   ;; elseif-eq-list => elseif-eq-list elseif-eq-part
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; elseif-eq-part => "elseif" equation-list
   (lambda ($2 $1 . $rest) `(elseif-eq ,$2))
   ;; elseif-eq-part => "elseif"
   (lambda ($1 . $rest) `(else-eq (eqn-list)))
   ;; else-eq-part => "else" equation-list
   (lambda ($2 $1 . $rest) `(else-eq ,$2))
   ;; else-eq-part => "else"
   (lambda ($1 . $rest) `(else-st (eqn-list)))
   ;; if-statement => "if" expression then-st-part elseif-st-list else-st-p...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if-st ,$2 ,$3 ,@(cdr (tl->list $4)) ,$5))
   ;; if-statement => "if" expression then-st-part elseif-st-list "end" "if"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if-st ,$2 ,$3 ,@(cdr (tl->list $4))))
   ;; if-statement => "if" expression then-st-part else-st-part "end" "if"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if-st ,$2 ,$3 ,$4))
   ;; if-statement => "if" expression then-st-part "end" "if"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(if-st ,$2 ,$3))
   ;; then-st-part => "then" statement-list
   (lambda ($2 $1 . $rest) $2)
   ;; then-st-part => "then"
   (lambda ($1 . $rest) '(stmt-list))
   ;; elseif-st-list => elseif-st-part
   (lambda ($1 . $rest) (make-tl 'l $1))
   ;; elseif-st-list => elseif-st-list elseif-st-part
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; elseif-st-part => "elseif" statement-list
   (lambda ($2 $1 . $rest) `(elseif-st ,$2))
   ;; elseif-st-part => "elseif"
   (lambda ($1 . $rest) `(else-st (stmt-list)))
   ;; else-st-part => "else" statement-list
   (lambda ($2 $1 . $rest) `(else-st ,$2))
   ;; else-st-part => "else"
   (lambda ($1 . $rest) `(else-st (stmt-list)))
   ;; for-equation => "for" for-indices "loop" equation-list "end" "for"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(for-eq ,$2 ,$4))
   ;; for-equation => "for" for-indices "loop" "end" "for"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(for-eq ,$2 (eqn-list)))
   ;; for-statement => "for" for-indices "loop" statement-list "end" "for"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(for-st ,$2 ,$4))
   ;; for-statement => "for" for-indices "loop" "end" "for"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(for-st ,$2 (stmt-list)))
   ;; for-indices => for-index
   (lambda ($1 . $rest) (make-tl 'for-indices $1))
   ;; for-indices => for-indices "," for-index
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; for-index => ident "in" expression
   (lambda ($3 $2 $1 . $rest) `(for-index ,$1 ,$3))
   ;; for-index => ident
   (lambda ($1 . $rest) `(for-index ,$1))
   ;; while-statement => "while" expression "loop" statement-list "end" "wh...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(while-st ,$2 ,(tl->list $4)))
   ;; while-statement => "while" expression "loop" "end" "while"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while-st ,$2 (stmt-list)))
   ;; when-equation => "when" expression then-eq-part elsewhen-eq-list "end...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(when-eq ,$2 ,$3 ,@(cdr (tl->list $4))))
   ;; elsewhen-eq-list => elsewhen-eq-part
   (lambda ($1 . $rest) (make-tl 'l $1))
   ;; elsewhen-eq-list => elsewhen-eq-list elsewhen-eq-part
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; elsewhen-eq-part => "elsewhen" expression "then"
   (lambda ($3 $2 $1 . $rest)
     `(elsewhen ,$2 (expr-list)))
   ;; elsewhen-eq-part => "elsewhen" expression "then" expression-list
   (lambda ($4 $3 $2 $1 . $rest)
     `(elsewhen ,$2 ,$4))
   ;; when-statement => "when" expression then-st-part elsewhen-st-list "en...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(when-st ,$2 ,$3 ,@(cdr (tl->list $4))))
   ;; elsewhen-st-list => elsewhen-st-part
   (lambda ($1 . $rest) (make-tl 'l $1))
   ;; elsewhen-st-list => elsewhen-st-list elsewhen-st-part
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; elsewhen-st-part => "elsewhen" expression "then"
   (lambda ($3 $2 $1 . $rest)
     `(elsewhen ,$2 (stmt-list)))
   ;; elsewhen-st-part => "elsewhen" expression "then" statement-list
   (lambda ($4 $3 $2 $1 . $rest)
     `(elsewhen ,$2 ,$4))
   ;; connect-clause => "connect" "(" component-reference "," component-ref...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(connect ,$3 ,$5))
   ;; expression => simple-expression
   (lambda ($1 . $rest) $1)
   ;; expression => "if" expression "then" expression elseif-ex-list "else"...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr (tl->list $5)) (else ,$7)))
   ;; expression => "if" expression "then" expression "else" expression
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 (else ,$6)))
   ;; elseif-ex-list => "elseif" expression "then" expression
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl 'l `(elseif ,$2 ,$4)))
   ;; elseif-ex-list => elseif-ex-list "elseif" expression "then" expression
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$2 ,$4)))
   ;; simple-expression => logical-expression
   (lambda ($1 . $rest) $1)
   ;; simple-expression => logical-expression ":" logical-expression ":" lo...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon ,$1 ,$5 ,$3))
   ;; simple-expression => logical-expression ":" logical-expression
   (lambda ($3 $2 $1 . $rest) `(colon ,$1 ,$3))
   ;; logical-expression => logical-term
   (lambda ($1 . $rest) $1)
   ;; logical-expression => logical-expression "or" logical-term
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; logical-term => logical-factor
   (lambda ($1 . $rest) $1)
   ;; logical-term => logical-term "and" logical-factor
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; logical-factor => relation
   (lambda ($1 . $rest) $1)
   ;; logical-factor => "not" relation
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; relation => arithmetic-expression
   (lambda ($1 . $rest) $1)
   ;; relation => relation rel-op arithmetic-expression
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; rel-op => "<"
   (lambda ($1 . $rest) 'lt)
   ;; rel-op => "<="
   (lambda ($1 . $rest) 'le)
   ;; rel-op => ">"
   (lambda ($1 . $rest) 'gt)
   ;; rel-op => ">="
   (lambda ($1 . $rest) 'ge)
   ;; rel-op => "=="
   (lambda ($1 . $rest) 'eq)
   ;; rel-op => "<>"
   (lambda ($1 . $rest) 'ne)
   ;; arithmetic-expression => term
   (lambda ($1 . $rest) $1)
   ;; arithmetic-expression => arithmetic-expression add-op term
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; add-op => "+"
   (lambda ($1 . $rest) 'add)
   ;; add-op => "-"
   (lambda ($1 . $rest) 'sub)
   ;; add-op => ".+"
   (lambda ($1 . $rest) 'dot-add)
   ;; add-op => ".-"
   (lambda ($1 . $rest) 'dot-sub)
   ;; term => factor
   (lambda ($1 . $rest) $1)
   ;; term => term mul-op factor
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; mul-op => "*"
   (lambda ($1 . $rest) 'mul)
   ;; mul-op => "/"
   (lambda ($1 . $rest) 'div)
   ;; mul-op => ".*"
   (lambda ($1 . $rest) 'dot-mul)
   ;; mul-op => "./"
   (lambda ($1 . $rest) 'dot-div)
   ;; factor => unary-expr
   (lambda ($1 . $rest) $1)
   ;; factor => factor "^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$2))
   ;; factor => factor ".^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$2))
   ;; unary-expr => primary
   (lambda ($1 . $rest) $1)
   ;; unary-expr => "+" primary
   (lambda ($2 $1 . $rest) `(pos ,$2))
   ;; unary-expr => "-" primary
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; primary => unsigned-number
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary => string
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary => "false"
   (lambda ($1 . $rest) `(p-expr '(false)))
   ;; primary => "true"
   (lambda ($1 . $rest) `(p-expr '(true)))
   ;; primary => name function-call-args
   (lambda ($2 $1 . $rest) `(fctn-call ,$1 ,$2))
   ;; primary => "der" function-call-args
   (lambda ($2 $1 . $rest) `(der ,$2))
   ;; primary => name
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary => name array-subscripts
   (lambda ($2 $1 . $rest) `(array-elt ,$1 ,$2))
   ;; primary => "(" output-expression-list ")"
   (lambda ($3 $2 $1 . $rest) `(p-expr ,$2))
   ;; primary => "[" expression-list-list "]"
   (lambda ($3 $2 $1 . $rest)
     `(matrix
        ,(map (lambda (row) (cons 'row (cdr row)))
              (cdr (tl->list $2)))))
   ;; primary => "{" function-arguments "}"
   (lambda ($3 $2 $1 . $rest) `(??? ,$2))
   ;; expression-list-list => expression-list
   (lambda ($1 . $rest) (make-tl 'rows $1))
   ;; expression-list-list => expression-list-list ";" expression-list
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; name => ident
   (lambda ($1 . $rest) $1)
   ;; name => "." ident
   (lambda ($2 $1 . $rest) `(sel ,$2))
   ;; name => name "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; component-reference => component-reference-1
   (lambda ($1 . $rest) `(comp-ref ,$1))
   ;; component-reference-1 => component-reference-2
   (lambda ($1 . $rest) $1)
   ;; component-reference-1 => "." component-reference-2
   (lambda ($2 $1 . $rest) `(sel ,$2))
   ;; component-reference-1 => component-reference-1 "." component-reference-2
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; component-reference-2 => ident
   (lambda ($1 . $rest) $1)
   ;; component-reference-2 => ident array-subscripts
   (lambda ($2 $1 . $rest) `(ary-ref ,$2 ,$1))
   ;; function-call-args => "(" function-arguments ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; function-call-args => "(" ")"
   (lambda ($2 $1 . $rest) '(ftn-args))
   ;; function-arguments => function-arguments-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; function-arguments => named-only-arguments-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; function-arguments-1 => function-argument
   (lambda ($1 . $rest) (make-tl 'ftn-args $1))
   ;; function-arguments-1 => function-arguments-1 "," function-argument
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; named-arguments => named-only-arguments-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; named-only-arguments-1 => named-argument
   (lambda ($1 . $rest) (make-tl 'ftn-args $1))
   ;; named-only-arguments-1 => named-only-arguments-1 "," named-argument
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; named-only-arguments-1 => function-arguments-1 "," named-argument
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; named-argument => ident "=" function-argument
   (lambda ($3 $2 $1 . $rest) `(named-arg ,$1 ,$3))
   ;; function-argument => "function" name "(" named-arguments ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-arg ,$2 ,$4))
   ;; function-argument => "function" name "(" ")"
   (lambda ($4 $3 $2 $1 . $rest) `(fctn-arg ,$2))
   ;; function-argument => expression
   (lambda ($1 . $rest) $1)
   ;; output-expression-list => ","
   (lambda ($1 . $rest) $1)
   ;; output-expression-list => expression
   (lambda ($1 . $rest) $1)
   ;; output-expression-list => output-expression-list "," expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; expression-list => expression-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; expression-list-1 => expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expression-list-1 => expression-list-1 "," expression
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; array-subscripts => "[" array-subscript-list "]"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; array-subscript-list => subscript
   (lambda ($1 . $rest)
     (make-tl 'array-subscripts $1))
   ;; array-subscript-list => array-subscript-list "," subscript
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; subscript => ":"
   (lambda ($1 . $rest) $1)
   ;; subscript => expression
   (lambda ($1 . $rest) $1)
   ;; comment => string-comment annotation
   (lambda ($2 $1 . $rest)
     (if (pair? $1) `(comment ,$1 ,$2) `(comment ,$2)))
   ;; comment => string-comment
   (lambda ($1 . $rest)
     (if (pair? $1) `(comment ,$1) '()))
   ;; string-comment => 
   (lambda $rest (list))
   ;; string-comment => string-cat
   (lambda ($1 . $rest) $1)
   ;; string-cat => string-cat-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; string-cat-1 => string
   (lambda ($1 . $rest)
     (make-tl 'string-comment $1))
   ;; string-cat-1 => string-cat-1 "+" string
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; opt-annotation => 
   (lambda $rest (list))
   ;; opt-annotation => annotation ";"
   (lambda ($2 $1 . $rest) $1)
   ;; annotation => "annotation" class-modification
   (lambda ($2 $1 . $rest) $1)
   ;; unsigned-number => '$fixed
   (lambda ($1 . $rest) `(unsigned-number ,$1))
   ;; unsigned-number => '$float
   (lambda ($1 . $rest) `(unsigned-number ,$1))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ))

;;; end tables
