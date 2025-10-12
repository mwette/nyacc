;; mlang-act.scm

;; Copyright 2015-2025 Matthew Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING included with the this distribution.

(define mlang-act-v
  (vector
   ;; $start => translation-unit
   (lambda ($1 . $rest) $1)
   ;; translation-unit => triv-stmt-list nontrivial-statement mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(script ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; translation-unit => triv-stmt-list function-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(function-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; translation-unit => triv-stmt-list class-defn mlang-item-list
   (lambda ($3 $2 $1 . $rest)
     `(classdef-file ,@(sx-tail $1) ,$2 ,@(sx-tail $3)))
   ;; translation-unit => nontrivial-statement mlang-item-list
   (lambda ($2 $1 . $rest)
     `(script ,$1 ,@(sx-tail $2)))
   ;; translation-unit => function-defn mlang-item-list
   (lambda ($2 $1 . $rest)
     `(function-file ,$1 ,@(sx-tail $2)))
   ;; translation-unit => class-defn mlang-item-list
   (lambda ($2 $1 . $rest)
     `(classdef-file ,$1 ,@(sx-tail $2)))
   ;; mlang-item-list => mlang-item-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; mlang-item-list-1 => 
   (lambda $rest (make-tl 'mitem-list))
   ;; mlang-item-list-1 => mlang-item-list-1 mlang-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; mlang-item => function-defn
   (lambda ($1 . $rest) $1)
   ;; mlang-item => statement
   (lambda ($1 . $rest) $1)
   ;; class-defn => "classdef" "(" attr-list ")" ident "<" supers term clas...
   (lambda ($10 $9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$5 ,$7 ,$3 ,@(cdr $9)))
   ;; class-defn => "classdef" "(" attr-list ")" ident term class-parts "end"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$5 ,$3 ,@(cdr $7)))
   ;; class-defn => "classdef" ident "<" supers term class-parts "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$2 ,$4 ,@(cdr $6)))
   ;; class-defn => "classdef" ident term class-parts "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(class-defn ,$2 ,@(cdr $4)))
   ;; supers => supers-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; supers-1 => ident
   (lambda ($1 . $rest) (make-tl 'supers $1))
   ;; supers-1 => supers-1 "&" ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; class-parts => class-parts-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; class-parts-1 => 
   (lambda $rest (make-tl 'seq))
   ;; class-parts-1 => class-parts-1 "properties" "(" attr-list ")" prop-li...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(properties ,$4 ,@(cdr $6))))
   ;; class-parts-1 => class-parts-1 "properties" prop-list "end"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(properties ,@(cdr $3))))
   ;; class-parts-1 => class-parts-1 "methods" "(" attr-list ")" method-lis...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(methods ,$4 ,@(cdr $6))))
   ;; class-parts-1 => class-parts-1 "methods" method-list "end"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(methods ,@(cdr $3))))
   ;; attr-list => attr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; attr-list-1 => attr
   (lambda ($1 . $rest) (make-tl 'attr-list $1))
   ;; attr-list-1 => attr-list-1 "," attr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; attr => ident
   (lambda ($1 . $rest) `(attr ,$1))
   ;; attr => ident "=" expr
   (lambda ($3 $2 $1 . $rest) `(attr ,$1 ,$3))
   ;; prop-list => prop-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; prop-list-1 => 
   (lambda $rest (make-tl 'properties))
   ;; prop-list-1 => prop-list-1 prop
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; prop => ident term
   (lambda ($2 $1 . $rest) `(property ,$1))
   ;; method-list => method-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; method-list-1 => method-item
   (lambda ($1 . $rest) (make-tl 'methods $1))
   ;; method-list-1 => method-list-1 method-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; method-item => function-defn
   (lambda ($1 . $rest) $1)
   ;; method-item => ident
   (lambda ($1 . $rest) $1)
   ;; method-item => function-sig
   (lambda ($1 . $rest) `(function-sig ,@(cdr $1)))
   ;; function-defn => function-decl non-comment-statement stmt-list the-end
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(if $2 `(stmt-list ,$2 unquote (cdr $3)) $3)))
   ;; function-defn => function-decl non-comment-statement the-end
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn
        ,$1
        ,(if $2 `(stmt-list ,$2) '(stmt-list))))
   ;; function-defn => function-decl the-end
   (lambda ($2 $1 . $rest)
     `(fctn-defn ,$1 (stmt-list)))
   ;; the-end => "end" term
   (lambda ($2 $1 . $rest) $1)
   ;; function-decl => "function" function-sig term lone-comment-list
   (lambda ($4 $3 $2 $1 . $rest)
     (append $2 (list $4)))
   ;; function-decl => "function" function-sig term
   (lambda ($3 $2 $1 . $rest) $2)
   ;; function-sig => "[" ident-list "]" "=" ident "(" ident-list ")"
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$5 ,$7 ,$2))
   ;; function-sig => "[" ident-list "]" "=" ident "(" ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$5 (ident-list) ,$2))
   ;; function-sig => ident "=" ident "(" ident-list ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 ,$5 (ident-list ,$1)))
   ;; function-sig => ident "=" ident "(" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(fctn-decl ,$3 (ident-list) (ident-list ,$1)))
   ;; function-sig => ident "(" ident-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-decl ,$1 ,$3 (ident-list)))
   ;; function-sig => ident "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(fctn-decl ,$1 (ident-list) (ident-list)))
   ;; ident-list => ident-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ident-list-1 => ident
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; ident-list-1 => ident-list-1 "," ident
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; stmt-list => stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; stmt-list-1 => statement
   (lambda ($1 . $rest)
     (if $1
       (make-tl 'stmt-list $1)
       (make-tl 'stmt-list)))
   ;; stmt-list-1 => stmt-list-1 statement
   (lambda ($2 $1 . $rest)
     (if $2 (tl-append $1 $2) $1))
   ;; triv-stmt-list => triv-stmt-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; triv-stmt-list-1 => trivial-statement
   (lambda ($1 . $rest)
     (make-tl 'triv-stmt-list $1))
   ;; triv-stmt-list-1 => triv-stmt-list-1 trivial-statement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; statement => trivial-statement
   (lambda ($1 . $rest) $1)
   ;; statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; non-comment-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; non-comment-statement => nontrivial-statement
   (lambda ($1 . $rest) $1)
   ;; trivial-statement => lone-comment "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; trivial-statement => term
   (lambda ($1 . $rest) '(empty-stmt))
   ;; nontrivial-statement => nontrivial-statement-1 term
   (lambda ($2 $1 . $rest)
     (sx-attr-add $1 'term $2))
   ;; nontrivial-statement-1 => expr
   (lambda ($1 . $rest) `(expr-stmt ,$1))
   ;; nontrivial-statement-1 => expr "=" expr
   (lambda ($3 $2 $1 . $rest) `(assn ,$1 ,$3))
   ;; nontrivial-statement-1 => "for" ident "=" expr term stmt-list "end"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$2 ,$4 ,$6))
   ;; nontrivial-statement-1 => "while" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$2 ,$4))
   ;; nontrivial-statement-1 => "if" expr term stmt-list elseif-list "else"...
   (lambda ($9 $8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5) (else ,$8)))
   ;; nontrivial-statement-1 => "if" expr term stmt-list elseif-list "end"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 ,@(cdr $5)))
   ;; nontrivial-statement-1 => "if" expr term stmt-list "else" term stmt-l...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$2 ,$4 (else ,$7)))
   ;; nontrivial-statement-1 => "if" expr term stmt-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$2 ,$4))
   ;; nontrivial-statement-1 => "switch" expr term case-list "otherwise" te...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4) (otherwise ,$7)))
   ;; nontrivial-statement-1 => "switch" expr term case-list "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$2 ,@(cdr $4)))
   ;; nontrivial-statement-1 => "return"
   (lambda ($1 . $rest) '(return))
   ;; nontrivial-statement-1 => command arg-list
   (lambda ($2 $1 . $rest) (append $1 (cdr $2)))
   ;; nontrivial-statement-1 => command "(" arg-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (append $1 (cdr $3)))
   ;; command => command-name
   (lambda ($1 . $rest) `(command ,$1))
   ;; command-name => "clc"
   (lambda ($1 . $rest) $1)
   ;; command-name => "doc"
   (lambda ($1 . $rest) $1)
   ;; command-name => "format"
   (lambda ($1 . $rest) $1)
   ;; command-name => "global"
   (lambda ($1 . $rest) $1)
   ;; command-name => "grid"
   (lambda ($1 . $rest) $1)
   ;; command-name => "help"
   (lambda ($1 . $rest) $1)
   ;; command-name => "hold"
   (lambda ($1 . $rest) $1)
   ;; command-name => "load"
   (lambda ($1 . $rest) $1)
   ;; command-name => "rotate3d"
   (lambda ($1 . $rest) $1)
   ;; command-name => "save"
   (lambda ($1 . $rest) $1)
   ;; command-name => "uiimport"
   (lambda ($1 . $rest) $1)
   ;; command-name => "ver"
   (lambda ($1 . $rest) $1)
   ;; arg-list => arg-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arg-list-1 => ident
   (lambda ($1 . $rest)
     (make-tl 'arg-list (cons 'arg (cdr $1))))
   ;; arg-list-1 => arg-list-1 ident
   (lambda ($2 $1 . $rest)
     (tl-append $1 (cons 'arg $2)))
   ;; elseif-list => elseif-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; elseif-list-1 => "elseif" expr term stmt-list
   (lambda ($4 $3 $2 $1 . $rest)
     (make-tl 'elseif-list `(elseif ,$2 ,$4)))
   ;; elseif-list-1 => elseif-list-1 "elseif" expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(elseif ,$3 ,$5)))
   ;; case-list => 
   (lambda $rest (make-tl 'case-list))
   ;; case-list => case-list "case" case-expr term stmt-list
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (tl-append $1 `(case ,$3 ,$5)))
   ;; case-expr => fixed
   (lambda ($1 . $rest) $1)
   ;; case-expr => string
   (lambda ($1 . $rest) $1)
   ;; case-expr => "{" fixed-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; case-expr => "{" string-list "}"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; fixed-list => fixed
   (lambda ($1 . $rest) (make-tl 'fixed-list $1))
   ;; fixed-list => fixed-list fixed
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; string-list => string
   (lambda ($1 . $rest) (make-tl 'string-list $1))
   ;; string-list => string-list string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; expr-list => expr-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; expr-list-1 => expr
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; expr-list-1 => expr-list-1 "," expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; expr => or-expr
   (lambda ($1 . $rest) $1)
   ;; expr => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; expr => or-expr ":" or-expr
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; expr => or-expr ":" or-expr ":" or-expr
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; expr => or-expr ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; expr => or-expr ":" or-expr ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; expr-nosp => or-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; expr-nosp => ":"
   (lambda ($1 . $rest) `(colon-expr))
   ;; expr-nosp => or-expr-nosp ":" or-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(colon-expr ,$1 ,$3))
   ;; expr-nosp => or-expr-nosp ":" or-expr-nosp ":" or-expr-nosp
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 ,$5))
   ;; expr-nosp => or-expr-nosp ":" "end"
   (lambda ($3 $2 $1 . $rest)
     `(colon-expr ,$1 (end)))
   ;; expr-nosp => or-expr-nosp ":" or-expr-nosp ":" "end"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(colon-expr ,$1 ,$3 (end)))
   ;; or-expr => and-expr
   (lambda ($1 . $rest) $1)
   ;; or-expr => or-expr "|" and-expr
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; or-expr => or-expr "||" and-expr
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; or-expr-nosp => and-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; or-expr-nosp => or-expr-nosp "|" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; or-expr-nosp => or-expr-nosp "||" and-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-or ,$1 ,$3))
   ;; and-expr => equality-expr
   (lambda ($1 . $rest) $1)
   ;; and-expr => and-expr "&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; and-expr => and-expr "&&" equality-expr
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; and-expr-nosp => equality-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; and-expr-nosp => and-expr-nosp "&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; and-expr-nosp => and-expr-nosp "&&" equality-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ss-and ,$1 ,$3))
   ;; equality-expr => rel-expr
   (lambda ($1 . $rest) $1)
   ;; equality-expr => equality-expr "==" rel-expr
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expr => equality-expr "~=" rel-expr
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; equality-expr-nosp => rel-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; equality-expr-nosp => equality-expr-nosp "==" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expr-nosp => equality-expr-nosp "~=" rel-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; rel-expr => add-expr
   (lambda ($1 . $rest) $1)
   ;; rel-expr => rel-expr "<" add-expr
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; rel-expr => rel-expr ">" add-expr
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; rel-expr => rel-expr "<=" add-expr
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; rel-expr => rel-expr ">=" add-expr
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; rel-expr-nosp => add-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; rel-expr-nosp => rel-expr-nosp "<" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; rel-expr-nosp => rel-expr-nosp ">" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; rel-expr-nosp => rel-expr-nosp "<=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; rel-expr-nosp => rel-expr-nosp ">=" add-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; add-expr => mul-expr
   (lambda ($1 . $rest) $1)
   ;; add-expr => add-expr "+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; add-expr => add-expr "-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; add-expr => add-expr ".+" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; add-expr => add-expr ".-" mul-expr
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; add-expr-nosp => mul-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; add-expr-nosp => add-expr-nosp "+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; add-expr-nosp => add-expr-nosp "-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; add-expr-nosp => add-expr-nosp ".+" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-add ,$1 ,$3))
   ;; add-expr-nosp => add-expr-nosp ".-" mul-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-sub ,$1 ,$3))
   ;; mul-expr => unary-expr
   (lambda ($1 . $rest) $1)
   ;; mul-expr => mul-expr "*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; mul-expr => mul-expr "/" unary-expr
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; mul-expr => mul-expr "\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; mul-expr => mul-expr "^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; mul-expr => mul-expr ".*" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; mul-expr => mul-expr "./" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; mul-expr => mul-expr ".\\" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; mul-expr => mul-expr ".^" unary-expr
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; mul-expr-nosp => unary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; mul-expr-nosp => mul-expr-nosp "*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp "/" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp "\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(ldiv ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp "^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(pow ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp ".*" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-mul ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp "./" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-div ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp ".\\" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-ldiv ,$1 ,$3))
   ;; mul-expr-nosp => mul-expr-nosp ".^" unary-expr-nosp
   (lambda ($3 $2 $1 . $rest) `(dot-pow ,$1 ,$3))
   ;; unary-expr => postfix-expr
   (lambda ($1 . $rest) $1)
   ;; unary-expr => "-" postfix-expr
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-expr => "+" postfix-expr
   (lambda ($2 $1 . $rest) `(pos $2))
   ;; unary-expr => "~" postfix-expr
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; unary-expr => "@" postfix-expr
   (lambda ($2 $1 . $rest) `(handle ,$2))
   ;; unary-expr-nosp => postfix-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; unary-expr-nosp => "-" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(neg ,$2))
   ;; unary-expr-nosp => "+" postfix-expr-nosp
   (lambda ($2 $1 . $rest) $2)
   ;; unary-expr-nosp => "~" postfix-expr-nosp
   (lambda ($2 $1 . $rest) `(not ,$2))
   ;; postfix-expr => primary-expr
   (lambda ($1 . $rest) $1)
   ;; postfix-expr => postfix-expr "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; postfix-expr => postfix-expr ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; postfix-expr => postfix-expr "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; postfix-expr => postfix-expr "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; postfix-expr => postfix-expr "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; postfix-expr => postfix-expr "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; postfix-expr-nosp => primary-expr-nosp
   (lambda ($1 . $rest) $1)
   ;; postfix-expr-nosp => postfix-expr-nosp "'"
   (lambda ($2 $1 . $rest) `(transpose ,$1))
   ;; postfix-expr-nosp => postfix-expr-nosp ".'"
   (lambda ($2 $1 . $rest) `(conj-transpose ,$1))
   ;; postfix-expr-nosp => postfix-expr-nosp "(" expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(aref-or-call ,$1 ,$3))
   ;; postfix-expr-nosp => postfix-expr-nosp "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(aref-or-call ,$1 (expr-list)))
   ;; postfix-expr-nosp => postfix-expr-nosp "{" expr-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(cell-ref ,$1 ,$3))
   ;; postfix-expr-nosp => postfix-expr-nosp "." ident
   (lambda ($3 $2 $1 . $rest) `(sel ,$3 ,$1))
   ;; primary-expr => ident
   (lambda ($1 . $rest) $1)
   ;; primary-expr => number
   (lambda ($1 . $rest) $1)
   ;; primary-expr => string
   (lambda ($1 . $rest) $1)
   ;; primary-expr => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expr => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; primary-expr => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expr => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; primary-expr => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; primary-expr-nosp => ident
   (lambda ($1 . $rest) $1)
   ;; primary-expr-nosp => number
   (lambda ($1 . $rest) $1)
   ;; primary-expr-nosp => string
   (lambda ($1 . $rest) $1)
   ;; primary-expr-nosp => "(" expr ")"
   (lambda ($3 $2 $1 . $rest) `(wrap ,$2))
   ;; primary-expr-nosp => "[" "]"
   (lambda ($2 $1 . $rest) '(matrix))
   ;; primary-expr-nosp => "[" matrix-row-list "]"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expr-nosp => "{" "}"
   (lambda ($2 $1 . $rest) '(cell-array))
   ;; primary-expr-nosp => "{" matrix-row-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(cell-array unquote (cdr $2)))
   ;; matrix-row-list => matrix-row-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; matrix-row-list-1 => matrix-row
   (lambda ($1 . $rest) (make-tl 'matrix $1))
   ;; matrix-row-list-1 => matrix-row-list-1 row-term matrix-row
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; row-term => ";"
   (lambda ($1 . $rest) $1)
   ;; row-term => "\n"
   (lambda ($1 . $rest) $1)
   ;; matrix-row => matrix-row-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; matrix-row-1 => expr-nosp
   (lambda ($1 . $rest) (make-tl 'row $1))
   ;; matrix-row-1 => matrix-row-1 "," expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; matrix-row-1 => matrix-row-1 'sp expr-nosp
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; term-list => term
   (lambda ($1 . $rest) $1)
   ;; term-list => term-list term
   (lambda ($2 $1 . $rest) $1)
   ;; term => ";" '$code-comm "\n"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; term => ";" "\n"
   (lambda ($2 $1 . $rest) $1)
   ;; term => ";"
   (lambda ($1 . $rest) $1)
   ;; term => ","
   (lambda ($1 . $rest) $1)
   ;; term => "\n"
   (lambda ($1 . $rest) $1)
   ;; lone-comment-list => lone-comment-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; lone-comment-list-1 => lone-comment "\n"
   (lambda ($2 $1 . $rest) (make-tl 'comm-list $1))
   ;; lone-comment-list-1 => lone-comment-list-1 lone-comment "\n"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $2))
   ;; ident => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; fixed => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; float => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; number => fixed
   (lambda ($1 . $rest) $1)
   ;; number => float
   (lambda ($1 . $rest) $1)
   ;; string => '$string
   (lambda ($1 . $rest) `(string ,$1))
   ;; lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comm ,$1))
   ))

;;; end tables
