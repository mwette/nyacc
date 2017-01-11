;;; lang/c99/mach.scm
;;;
;;; Copyright (C) 2015-2017 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; C parser generator: based on ISO-C99; with comments and CPP statements

(define-module (nyacc lang c99 mach)
  #:export (c99-spec c99-mach dev-parse-c dev-parse-c99
	    gen-c99-files gen-c99x-files)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((srfi srfi-43) #:select (vector-map))
  )

;; @item c99-spec
;; This variable is the specification a-list for the hacked ISO C99 language.
;; Run this through @code{make-lalr-machine} to get an a-list for the
;; automaton.  The grammar is modified to parse CPP statements and comments.
;; The output of the end parser will be a SXML tree (w/o the @code{*TOP*} node.
(define c99-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2016,2017 Matthew R. Wette"
			  lang-crn-lic))
   (prec< 'then "else")	       ; "then/else" SR-conflict resolution
   (prec< 'imp		       ; "implied type" SR-conflict resolution
	  "char" "short" "int" "long"
	  "float" "double" "_Complex")
   (start translation-unit)
   (grammar

    ;; expressions
    (primary-expression			; S 6.5.1
     (identifier ($$ `(p-expr ,$1)))
     (constant ($$ `(p-expr ,$1)))
     (string-literal ($$ `(p-expr ,(tl->list $1))))
     ("(" expression ")" ($$ $2))
     )

    (postfix-expression			; S 6.5.2
     (primary-expression)
     (postfix-expression "[" expression "]" ($$ `(array-ref ,$3 ,$1)))
     (postfix-expression "(" argument-expression-list ")"
			 ($$ `(fctn-call ,$1 ,(tl->list $3))))
     (postfix-expression "(" ")" ($$ `(fctn-call ,$1 (expr-list))))
     (postfix-expression "." identifier ($$ `(d-sel ,$3 ,$1)))
     (postfix-expression "->" identifier ($$ `(i-sel ,$3 ,$1)))
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1)))
     ("(" type-name ")" "{" initializer-list "}"
      ($$ `(comp-lit ,$2 ,(tl->list $5))))
     ("(" type-name ")" "{" initializer-list "," "}"
      ($$ `(comp-lit ,$2 ,(tl->list $5))))
     )

    (argument-expression-list
     (assignment-expression ($$ (make-tl 'expr-list $1)))
     (argument-expression-list "," assignment-expression ($$ (tl-append $1 $3)))
     ;; The following is a modification to deal with using abstract declarations
     ;; as arguments to CPP macros (e.g., see offsetof in <stddef.h>).
     (arg-expr-hack ($$ (make-tl 'expr-list $1)))
     (argument-expression-list "," arg-expr-hack ($$ (tl-append $1 $3)))
     )
    (arg-expr-hack
     (declaration-specifiers
      abstract-declarator ($$ `(param-decl ,(tl->list $1) $2)))
     (declaration-specifiers ($$ `(param-decl ,(tl->list $1)))))

    (unary-expression
     (postfix-expression)		; S 6.5.3
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2)))
     (unary-operator cast-expression ($$ (list $1 $2)))
     ("sizeof" unary-expression ($$ `(sizeof-expr ,$2)))
     ("sizeof" "(" type-name ")" ($$ `(sizeof-type ,$3)))
     )
    (unary-operator ("&" ($$ 'ref-to)) ("*" ($$ 'de-ref))
		    ("+" ($$ 'pos)) ("-" ($$ 'neg))
		    ("~" ($$ 'bitwise-not)) ("!" ($$ 'not)))

    (cast-expression			; S 6.5.4
     (unary-expression)
     ("(" type-name ")" cast-expression ($$ `(cast ,$2 ,$4)))
     )

    (multiplicative-expression		; S 6.5.5
     (cast-expression)
     (multiplicative-expression "*" cast-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" cast-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" cast-expression ($$ `(mod ,$1 ,$3)))
     )

    (additive-expression		; S 6.5.6
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3)))
     )

    (shift-expression			; S 6.5.7
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3)))
     )

    (relational-expression		; S 6.5.8
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3)))
     )
    
    (equality-expression		; S 6.5.9
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3)))
     )

    ;; called AND-expression
    (bitwise-and-expression		; S 6.5.10
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3)))
     )

    ;; called exclusive-OR-expression
    (bitwise-xor-expression		; S 6.5.11
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3)))
     )

    ;; called inclusive-OR-expression
    (bitwise-or-expression		; S 6.5.12
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3)))
     )

    (logical-and-expression		; S 6.5.13
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression
			     ($$ `(and ,$1 ,$3)))
     )

    (logical-or-expression		; 6.5.14
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression
			    ($$ `(or ,$1 ,$3)))
     )
    
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" expression ":" conditional-expression
			    ($$ `(cond-expr ,$1 ,$3 ,$5)))
     )

    (assignment-expression		; S 6.5.16
     (conditional-expression)
     (unary-expression assignment-operator assignment-expression
		       ($$ `(assn-expr ,$1 (op ,$2) ,$3)))
     )
    (assignment-operator
     ("=") ("+=") ("-=") ("*=") ("/=") ("%=")
     ("<<=") (">>=") ("&=") ("^=") ("|="))

    (expression				; S 6.5.17
     (assignment-expression)
     (expression "," assignment-expression
		 ($$ (if (eqv? 'comma-expr (sx-tag $1))
			 (append $1 (list $3))
			 `(comma-expr ,$1 ,$3))))
     )

    (constant-expression		; S 6.6
     (conditional-expression)
     )

    ;; declarations
    (declaration			; S 6.7
     (declaration-specifiers
      init-declarator-list
      ($$ (save-typenames `(decl ,(tl->list $1) ,(tl->list $2))))
      ";" opt-code-comment
      ($$ (if (pair? $5) (append $3 (list $5)) $3)))
     (declaration-specifiers
      ";" opt-code-comment
      ($$ (if (pair? $3)
	      `(decl ,(tl->list $1) ,(list $3))
	      `(decl ,(tl->list $1)))))
     )

    (declaration-specifiers		; S 6.7
     ;; storage-class-specifier declaration-specifiers_opt
     (storage-class-specifier ($$ (make-tl 'decl-spec-list $1)))
     (storage-class-specifier declaration-specifiers ($$ (tl-insert $2 $1)))
     ;; type-specifier declaration-specifiers_opt
     (type-specifier ($$ (make-tl 'decl-spec-list $1)))
     (type-specifier declaration-specifiers ($$ (tl-insert $2 $1)))
     ;; type-qualifier declaration-specifiers_opt
     (type-qualifier ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier declaration-specifiers ($$ (tl-insert $2 $1)))
     ;; function-specifier declaration-specifiers_opt
     (function-specifier ($$ (make-tl 'decl-spec-list $1)))
     (function-specifier declaration-specifiers ($$ (tl-insert $2 $1)))
     )

    (init-declarator-list		; S 6.7
     (init-declarator ($$ (make-tl 'init-declr-list $1)))
     (init-declarator-list "," init-declarator ($$ (tl-append $1 $3)))
     )

    (init-declarator			; S 6.7
     (declarator ($$ `(init-declr ,$1)))
     (declarator "=" initializer ($$ `(init-declr ,$1 ,$3)))
     )

    (storage-class-specifier		; S 6.7.1
     ("auto" ($$ '(stor-spec (auto))))
     ("extern" ($$ '(stor-spec (extern))))
     ("register" ($$ '(stor-spec (register))))
     ("static" ($$ '(stor-spec (static))))
     ("typedef" ($$ '(stor-spec (typedef))))
     )

    (type-specifier			; S 6.7.2
     ("void" ($$ '(type-spec (void))))
     (fixed-type-specifier ($$ `(type-spec ,$1))) ; I made this up
     (float-type-specifier ($$ `(type-spec ,$1))) ; I made this up
     ("_Bool" ($$/ref 's5.1.5-01 '(type-spec (fixed-type "_Bool"))))
     (complex-type-specifier ($$ `(type-spec ,$1))) ; I made this up
     (struct-or-union-specifier ($$ `(type-spec ,$1)))
     (enum-specifier ($$ `(type-spec ,$1)))
     (typedef-name ($$ `(type-spec ,$1)))
     )

    (fixed-type-specifier
     ("short" ($prec 'imp) ($$ '(fixed-type "short")))
     ("short" "int" ($$ '(fixed-type "short int")))
     ("signed" "short" ($prec 'imp) ($$ '(fixed-type "signed short")))
     ("signed" "short" "int" ($$ '(fixed-type "signed short int")))
     ("int" ($$ '(fixed-type "int")))
     ("signed" ($prec 'imp) ($$ '(fixed-type "signed")))
     ("signed" "int" ($$ '(fixed-type "signed int")))
     ("long" ($prec 'imp) ($$ '(fixed-type "long")))
     ("long" "int" ($$ '(fixed-type "long int")))
     ("signed" "long" ($prec 'imp) ($$ '(fixed-type "signed long")))
     ("signed" "long" "int" ($$ '(fixed-type "signed long int")))
     ("long" "long" ($prec 'imp) ($$ '(fixed-type "long long")))
     ("long" "long" "int" ($$ '(fixed-type "long long int")))
     ("signed" "long" "long" ($prec 'imp)
      ($$ '(fixed-type "signed long long")))
     ("signed" "long" "long" "int" ($$ '(fixed-type "signed long long int")))
     ("unsigned" "short" "int" ($$ '(fixed-type "unsigned short int")))
     ("unsigned" "short" ($prec 'imp) ($$ '(fixed-type "unsigned short")))
     ("unsigned" "int" ($$ '(fixed-type "unsigned int")))
     ("unsigned" ($prec 'imp) ($$ '(fixed-type "unsigned")))
     ("unsigned" "long" "int" ($$ '(fixed-type "unsigned long")))
     ("unsigned" "long" ($prec 'imp) ($$ '(fixed-type "unsigned long")))
     ("unsigned" "long" "long" "int"
      ($$ '(fixed-type "unsigned long long int")))
     ("unsigned" "long" "long" ($prec 'imp)
      ($$ '(fixed-type "unsigned long long")))
     ("char" ($$ '(fixed-type "char")))
     ("signed" "char" ($$ '(fixed-type "signed char")))
     ("unsigned" "char" ($$ '(fixed-type "unsigned char"))))
    (float-type-specifier
     ("float" ($prec 'imp) ($$ '(float-type "float")))
     ("double" ($prec 'imp) ($$ '(float-type "double")))
     ("long" "double" ($$ '(float-type "long double"))))
    (complex-type-specifier
     ("_Complex" ($$ '(complex-type "_Complex")))
     ("float" "_Complex" ($$ '(complex-type "float _Complex")))
     ("double" "_Complex" ($$ '(complex-type "double _Complex")))
     ("long" "double" "_Complex" ($$ '(complex-type "long double _Complex")))
     )

    ;; This one modified: split out struct-or-union = "struct"|"union"
    (struct-or-union-specifier		; S 6.7.2.1
     ("struct" ident-like "{" struct-declaration-list "}"
      ($$ `(struct-def ,$2 ,(tl->list $4))))
     ("struct" "{" struct-declaration-list "}"
      ($$ `(struct-def ,(tl->list $3))))
     ("struct" ident-like ($$ `(struct-ref ,$2)))
     ("union" ident-like "{" struct-declaration-list "}"
      ($$ `(union-def ,$2 ,(tl->list $4))))
     ("union" "{" struct-declaration-list "}"
      ($$ `(union-def ,(tl->list $3))))
     ("union" ident-like ($$ `(union-ref ,$2)))
     )
    ;; because name following struct/union can be indentifier or typeref
    (ident-like (identifier) (typedef-name ($$ `(ident ,(cdr $1)))))

    ;; Calling this field-list in the parse tree.
    (struct-declaration-list		; S 6.7.2.1
     (struct-declaration ($$ (make-tl 'field-list $1)))
     (lone-comment ($$ (make-tl 'field-list $1)))
     (struct-declaration-list struct-declaration ($$ (tl-append $1 $2)))
     (struct-declaration-list lone-comment ($$ (tl-append $1 $2)))
     )

    (struct-declaration			; S 6.7.2.1
     (specifier-qualifier-list
      struct-declarator-list ";" opt-code-comment
      ($$ (if (pair? $4)
	      `(comp-decl ,(tl->list $1) ,(tl->list $2) ,$4)
	      `(comp-decl ,(tl->list $1) ,(tl->list $2)))))
     )
     
    (specifier-qualifier-list		; S 6.7.2.1
     (type-specifier specifier-qualifier-list ($$ (tl-insert $2 $1)))
     (type-specifier ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier specifier-qualifier-list ($$ (tl-insert $2 $1)))
     (type-qualifier ($$ (make-tl 'decl-spec-list $1)))
     )

    (struct-declarator-list		; S 6.7.2.1
     (struct-declarator ($$ (make-tl 'comp-declr-list $1)))
     (struct-declarator-list "," struct-declarator ($$ (tl-append $1 $3)))
     )

    (struct-declarator			; S 6.7.2.1
     (declarator ($$ `(comp-declr ,$1)))
     (declarator ":" constant-expression ($$ `(comp-declr (bit-field ,$1 ,$3))))
     (":" constant-expression ($$ `(comp-declr (bit-field ,$2))))
     )

    (enum-specifier			; S 6.7.2.2
     ("enum" ident-like "{" enumerator-list "}"
      ($$ `(enum-def ,$2 ,(tl->list $4))))
     ("enum" ident-like "{" enumerator-list "," "}"
      ($$ `(enum-def ,$2 ,(tl->list $4))))
     ("enum" "{" enumerator-list "}" ($$ `(enum-def ,(tl->list $3))))
     ("enum" "{" enumerator-list "," "}" ($$ `(enum-def ,(tl->list $3))))
     ("enum" ident-like ($$ `(enum-ref ,$2)))
     )

    ;; keeping old enum-def-list in parse tree
    (enumerator-list			; S 6.7.2.2
     (enumerator ($$ (make-tl 'enum-def-list $1)))
     (enumerator-list "," enumerator ($$ (tl-append $1 $3)))
     )

    ;; Had to change enumeration-constant => identifier
    (enumerator				; S 6.7.2.2
     (identifier ($$ `(enum-defn ,$1)))
     (identifier "=" constant-expression ($$ `(enum-defn ,$1 ,$3)))
     )

    (type-qualifier
     ("const" ($$ `(type-qual ,$1)))
     ("volatile" ($$ `(type-qual ,$1)))
     ("restrict" ($$ `(type-qual ,$1)))
     )

    (function-specifier ("inline" ($$ `(fctn-spec ,$1))))
    
    (declarator
     (pointer direct-declarator ($$ `(ptr-declr ,$1 ,$2)))
     (direct-declarator)
     )

    (direct-declarator			; S 6.7.6
     (identifier ($$ $1))
     ("(" declarator ")" ($$ `(scope ,$2)))

     (direct-declarator
      "[" type-qualifier-list assignment-expression "]"
      ($$ `(array-of ,$1 ,$3 ,$4)))
     (direct-declarator "[" type-qualifier-list "]" ($$ `(array-of ,$1 ,$3)))
     (direct-declarator "[" assignment-expression "]" ($$ `(array-of ,$1 ,$3)))
     (direct-declarator "[" "]" ($$ `(array-of ,$1)))
     (direct-declarator
      "[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(array-of ,$1 ,(tl->list (tl-insert '(stor-spec "static") $4)) ,$5)))
     (direct-declarator
      "[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(array-of ,$1 ,(tl->list (tl-insert '(stor-spec "static") $3)) ,$5)))
     (direct-declarator
      "[" type-qualifier-list "*" "]"	; variable length array
      ($$ `(array-of ,$1 ,$3 (var-len))))
     (direct-declarator
      "[" "*" "]"			; variable length array
      ($$ `(array-of ,$1 (var-len))))
     (direct-declarator "(" parameter-type-list ")"
			($$ `(ftn-declr ,$1 ,(tl->list $3))))
     (direct-declarator "(" identifier-list ")"
			($$ `(ftn-declr ,$1 ,(tl->list $3))))
     (direct-declarator "(" ")" ($$ `(ftn-declr ,$1 (param-list))))
     )

    (pointer				; S 6.7.6
     ("*" type-qualifier-list ($$ `(pointer ,(tl->list $2))))
     ("*" ($$ '(pointer)))
     ("*" type-qualifier-list pointer ($$ `(pointer ,(tl->list $2) ,$3)))
     ("*" pointer ($$ `(pointer ,$2)))
     )

    ;; But put in tree as decl-spec-list
    (type-qualifier-list
     (type-qualifier ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier-list type-qualifier ($$ (tl-append $1 $2)))
     )

    (parameter-type-list
     (parameter-list ($$ $1))
     (parameter-list "," "..." ($$ (tl-append $1 '(ellipsis))))
     )

    (parameter-list
     (parameter-declaration ($$ (make-tl 'param-list $1)))
     (parameter-list "," parameter-declaration ($$ (tl-append $1 $3)))
     )

    (parameter-declaration
     (declaration-specifiers declarator
			     ($$ `(param-decl ,(tl->list $1)
					       (param-declr ,$2))))
     (declaration-specifiers abstract-declarator
			     ($$ `(param-decl ,(tl->list $1)
					       (param-declr ,$2))))
     (declaration-specifiers ($$ `(param-decl ,(tl->list $1))))
     )

    (identifier-list
     (identifier ($$ (make-tl 'ident-list $1)))
     (identifier-list "," identifier ($$ (tl-append $1 $3)))
     )

    (type-name				; S 6.7.6
     ;; e.g., (foo_t *)
     (specifier-qualifier-list abstract-declarator
			       ($$ `(type-name ,(tl->list $1) ,$2)))
     ;; e.g., (int)
     (declaration-specifiers ($$ `(type-name ,(tl->list $1)))) 
     )

    (abstract-declarator		; S 6.7.6
     (pointer ($$ `(abs-declr ,$1)))
     (pointer direct-abstract-declarator ($$ `(abs-declr ,$1 ,$2)))
     (direct-abstract-declarator ($$ `(abs-declr ,$1)))
     )

    (direct-abstract-declarator
     ("(" abstract-declarator ")" ($$ `(declr-scope ,$2)))
     (direct-abstract-declarator
      "[" type-qualifier-list assignment-expression "]"
      ($$ `(declr-array ,$3 ,$4)))
     (direct-abstract-declarator
      "[" type-qualifier-list "]"
      ($$ `(declr-array ,$1 ,$3)))
     (direct-abstract-declarator
      "[" assignment-expression "]"
      ($$ `(declr-array ,$1 ,$3)))
     (direct-abstract-declarator
      "[" "]" ($$ `(declr-array ,$1)))
     (direct-abstract-declarator
      "[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(declr-array
	    ,$1 ,(tl->list (tl-insert '(stor-spec "static") $4)) ,$5)))
     (direct-abstract-declarator
      "[" "static" type-qualifier-list "]"
      ($$ `(declr-array ,$1 ,(tl->list (tl-insert '(stor-spec "static") $4)))))
     (direct-abstract-declarator
      "[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(declr-array
	    ,$1 ,(tl->list (tl-insert '(stor-spec "static") $3)) ,$5)))
     ;;
     ("[" type-qualifier-list assignment-expression "]"
      ($$ `(declr-anon-array ,$2 ,$3)))
     ("[" type-qualifier-list "]" ($$ `(declr-anon-array ,$2)))
     ("[" assignment-expression "]" ($$ `(declr-anon-array ,$2)))
     ("[" "]" ($$ `(declr-anon-array)))
     ("[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(declr-anon-array
	    ,(tl->list (tl-insert '(stor-spec "static") $3)) ,$4)))
     ("[" "static" type-qualifier-list "]"
      ($$ `(declr-anon-array ,(tl->list (tl-insert '(stor-spec "static") $3)))))
     ("[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(declr-anon-array
	    ,(tl->list (tl-insert '(stor-spec "static") $2)) ,$4)))
     (direct-abstract-declarator "[" "*" "]" ($$ `(declr-STAR ,$1)))
     ("[" "*" "]" ($$ '(declr-STAR)))
     (direct-abstract-declarator "(" parameter-type-list ")"
				 ($$ `(declr-fctn ,$1 ,(tl->list $3))))
     (direct-abstract-declarator "(" ")" ($$ `(declr-fctn ,$1)))
     ("(" parameter-type-list ")" ($$ `(declr-anon-fctn ,(tl->list $2))))
     ("(" ")" ($$ '(declr-anon-fctn)))
     )

    ;;typedef-name must be hacked w/ the lexical analyzer
    (typedef-name ('typename ($$ `(typename ,$1))))

    (initializer			; S 6.7.9
     (assignment-expression ($$ `(initzer ,$1)))
     ("{" initializer-list "}" ($$ `(initzer ,(tl->list $2))))
     ("{" initializer-list "," "}" ($$ `(initzer ,(tl->list $2))))
     )

    ;; The designation productions are from C99.
    (initializer-list
     (designation initializer ($$ (make-tl 'initzer-list $1 $2)))
     (initializer ($$ (make-tl 'initzer-list $1)))
     (initializer-list "," designation initializer ($$ (tl-append $1 $3 $4)))
     (initializer-list "," initializer ($$ (tl-append $1 $3)))
     )

    (designation			; S 6.7.8
     (designator-list "=" ($$ `(desig ,$1)))
     )

    (designator-list
     (designator ($$ (make-tl 'desgr-list $1)))
     (designator-list designator ($$ (tl-append $1 $2)))
     )

    (designator
     ("[" constant-expression "]" ($$ (list 'array-dsgr $2)))
     ("." identifier ($$ (list 'sel-dsgr $2)))
     )

    ;; statements
    (statement
     (labeled-statement)
     (compound-statement)
     (expression-statement)
     (selection-statement)
     (iteration-statement)
     (jump-statement)
     (cpp-statement)
     )

    (labeled-statement
     (identifier ":" statement ($$ `(labeled-stmt ,$1 ,$3)))
     ("case" constant-expression ":" statement ($$ `(case ,$2 ,$4)))
     ("default" ":" statement ($$ `(default ,$3)))
     )

    (compound-statement
     ("{" block-item-list "}" 
      ($$ `(compd-stmt ,(tl->list $2))))
     ("{" "}"
      ($$ `(compd-stmt (block-item-list))))
     )

    (block-item-list
     (block-item ($$ (make-tl 'block-item-list $1)))
     (block-item-list block-item ($$ (tl-append $1 $2)))
     )

    (block-item
     (declaration)
     (statement)
     )
    
    (expression-statement
     (expression ";" ($$ `(expr-stmt ,$1)))
     (";" ($$ '(expr-stmt)))
     )

    (selection-statement
     ("if" "(" expression ")" statement ($prec 'then)
      ($$ `(if ,$3 ,$5)))
     ("if" "(" expression ")" statement "else" statement
      ($$ `(if ,$3 ,$5 ,$7)))
     ("switch" "(" expression ")" statement ($$ `(switch ,$3 ,$5)))
     )

    (iteration-statement
     ("while" "(" expression ")" statement ($$ `(while ,$3 ,$5)))
     ("do" statement "while" "(" expression ")" ";" ($$ `(do-while ,$2 ,$5)))
     ("for" "(" initial-clause opt-expression ";" opt-expression ")" statement
      ($$ `(for ,$3 ,$4 ,$6 ,$8)))
     )
    (initial-clause			; <= added for convenience
     (expression ";")
     (";" ($$ '(expr)))
     (declaration))
    (opt-expression			; <= added for convenience
     ($empty ($$ '(expr)))
     (expression))

    (jump-statement			; S 6.8.6
     ("goto" identifier ";" ($$ `(goto ,$2)))
     ("continue" ";" ($$ '(continue)))
     ("break" ";" ($$ '(break)))
     ("return" expression ";" ($$ `(return ,$2)))
     ("return" ";" ($$ `(return (expr))))
     )

    ;; external definitions
    (translation-unit			; S 6.9
     (external-declaration-list ($$ (tl->list $1)))
     )
    (external-declaration-list
     (external-declaration ($$ (make-tl 'trans-unit $1)))
     (external-declaration-list
      external-declaration
      ;; A ``kludge'' to deal with @code{extern "C" ...}:
      ($$ (if (eqv? (sx-tag $2) 'extern-block) (tl-extend $1 (sx-tail $2 2))
	      (tl-append $1 $2))))
     )

    (external-declaration		; S 6.9
     (function-definition)
     (declaration)
     (lone-comment)
     (cpp-statement)
     ("extern" $string "{" external-declaration-list "}"
      ($$ `(extern-block ,$2 (extern-begin ,$2)
			 ,@(sx-tail (tl->list $4) 1) (extern-end))))
     )
    
    (function-definition
     (declaration-specifiers
      declarator declaration-list compound-statement
      ($$ `(knr-fctn-defn ,(tl->list $1) ,$2 ,(tl->list $3) ,$4)))
     (declaration-specifiers
      declarator compound-statement
      ($$ `(fctn-defn ,(tl->list $1) ,$2 ,$3)))
     )
    
    (declaration-list
     (declaration ($$ (make-tl $1)))
     (declaration-list declaration ($$ (tl-append $1 $2)))
     )

    (opt-code-comment () (code-comment))

    ;; non-terminal leaves
    (identifier
     ($ident ($$ `(ident ,$1)))
     ('cpp-ident ($$ `(ident ,$1))))
    (constant
     ($fixed ($$ `(fixed ,$1)))		; integer-constant
     ($float ($$ `(float ,$1)))		; floating-constant
     ($chlit ($$ `(char ,$1))))		; char-constant
    (string-literal
     ($string ($$ (make-tl 'string $1))) ; string-constant
     (string-literal $string ($$ (tl-append $1 $2))))
    (code-comment ($code-comm ($$ `(comment ,$1))))
    (lone-comment ($lone-comm ($$ `(comment ,$1))))
    (cpp-statement ('cpp-stmt ($$ `(cpp-stmt ,$1))))

    )))


(define c99-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine c99-spec))))

;;; =====================================

;; The following are needed by the code in pbody.scm.
(define len-v (assq-ref c99-mach 'len-v))
(define pat-v (assq-ref c99-mach 'pat-v))
(define rto-v (assq-ref c99-mach 'rto-v))
(define mtab (assq-ref c99-mach 'mtab))
(define act-v (vector-map
	       (lambda (ix f) (eval f (current-module)))
	       (vector-map (lambda (ix actn) (wrap-action actn))
			   (assq-ref c99-mach 'act-v))))

(include-from-path "nyacc/lang/c99/body.scm")

(define raw-parser (make-lalr-parser c99-mach))

(define (run-parse)
  (let ((info (fluid-ref *info*)))
    (raw-parser (gen-c-lexer) #:debug (cpi-debug info))))

(define* (dev-parse-c99 #:key
			(cpp-defs '())	; CPP defines
			(inc-dirs '())	; include directories
			(td-dict '())	; typedef dictionary
			(mode 'file)	; mode: 'file or 'code
			(xdef? #f)	; expand def function: proc name mode
			(debug #f))	; debug
  (catch
   #t ;; 'c99-error 'cpp-error 'nyacc-error
   (lambda ()
     (let ((info (make-cpi debug cpp-defs (cons "." inc-dirs) td-dict)))
       (with-fluid*
	   *info* info
	   (lambda ()
	     (raw-parser (gen-c-lexer #:mode mode #:xdef? xdef?)
			 #:debug debug)))))
   (lambda (key fmt . rest)
     (report-error fmt rest)
     #f)))

(define dev-parse-c dev-parse-c99)

;;; =====================================

;; @item gen-c99-files [dir] => #t
;; Update or generate the files @quot{c99act.scm} and @quot{c99tab.scm}.
;; These are the tables and actions for the C99 parser.
;; If there are no changes to existing files, no update occurs.
(define (gen-c99-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions c99-mach (xtra-dir "c99act.scm.new"))
  (write-lalr-tables c99-mach (xtra-dir "c99tab.scm.new"))
  (let ((a (move-if-changed (xtra-dir "c99act.scm.new")
			    (xtra-dir "c99act.scm")))
	(b (move-if-changed (xtra-dir "c99tab.scm.new")
			    (xtra-dir "c99tab.scm"))))
    (when (or a b) 
      (system (string-append "touch " (lang-dir "parser.scm"))))))

;; @item gen-c99x-files [dir] => #t
;; Update or generate the files @quot{c99xact.scm} and @quot{c99xtab.scm}.
;; These are the tables and actions for the C99 expression parser.
;; If there are no changes to existing files, no update occurs.
(define (gen-c99x-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (let* ((cexpr-spec (restart-spec c99-mach 'expression))
	 (cexpr-mach (compact-machine
		      (hashify-machine
		       (make-lalr-machine cexpr-spec)))))
    (write-lalr-actions cexpr-mach (xtra-dir "c99xact.scm.new"))
    (write-lalr-tables cexpr-mach (xtra-dir "c99xtab.scm.new")))
    
  (let ((a (move-if-changed (xtra-dir "c99xact.scm.new")
			    (xtra-dir "c99xact.scm")))
	(b (move-if-changed (xtra-dir "c99xtab.scm.new")
			    (xtra-dir "c99xtab.scm"))))
    (when (or a b) 
      (system (string-append "touch " (lang-dir "parser.scm")))
      #;(compile-file (lang-dir "xparser.scm"))
      )))

;; --- last line ---
