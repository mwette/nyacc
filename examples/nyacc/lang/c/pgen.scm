;;; lang/c/pgen.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

;; C parser generator

(define-module (lang c pgen)
  #:export (clang-spec clang-mach dev-parse-c)
  #:use-module (lang c cpp)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (lang util)
  #:use-module ((srfi srfi-1) #:select (last remove))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module ((sxml fold) #:select (foldts*-values foldts))
  #:use-module ((sxml xpath) #:select (sxpath))
  ;; debug:
  ;;#:re-export (read-c-ident)
  #:use-module (ice-9 pretty-print)
  )

(define (fmterr fmt . args)
  (apply simple-format (current-error-port) fmt args))

;; Objective is to generate a sxml tree.
;; Strategy for building the tree:
;; @itemize
;; @item add decoration for RHS items, not LHS items
;; @example
;; Written to the grammar specified for ISO-C99 in the following URL:
;; http://slps.github.io/zoo/c/iso-9899-tc3.html
;; but modified to handle comments and CPP statements.
(define clang-spec
  (lalr-spec
   (notice lang-crn)
   (expect 1)				; expect 1 shift-reduce conflict
   (start translation-unit-proxy)
   (grammar

    (translation-unit-proxy (translation-unit ($$ (tl->list $1))))

    ;; 4.1, p 74
    (declaration
     (declaration-specifiers
      initialized-declarator-list
      ($$ (let ((decl `(decl ,(tl->list $1) ,(tl->list $2))))
	    (for-each add-typename (find-new-typenames decl))
	    decl))
      ";" opt-code-comment
      ($$ (if (pair? $5) (append $3 (list $5)) $3)))
     )

    (declaration-specifiers
     ;; storage-class-specifier declaration-specifiers_opt
     (storage-class-specifier
      ($$ (make-tl 'decl-spec-list $1)))
     (storage-class-specifier declaration-specifiers
			      ($$ (tl-insert $2 $1)))
     ;; type-specifier declaration-specifiers_opt
     (type-specifier
      ($$ (make-tl 'decl-spec-list $1)))
     (type-specifier declaration-specifiers
		     ($$ (tl-insert $2 $1)))
     ;; type-qualifier declaration-specifiers_opt
     (type-qualifier
      ($$ (make-tl 'decl-spec-list `(type-qual ,$1))))
     (type-qualifier declaration-specifiers
		     ($$ (tl-insert $2 `(type-qual ,$1))))
     ;; function-specifier declaration-specifiers_opt
     (function-specifier
      ($$ (make-tl 'decl-spec-list `(fctn-spec ,$1))))
     (function-specifier declaration-specifiers
			 ($$ (tl-insert $2 `(fctn-spec ,$1))))
     )
    
    (initialized-declarator-list
     (initialized-declarator ($$ (make-tl 'init-declr-list $1)))
     (initialized-declarator-list "," initialized-declarator
				  ($$ (tl-append $1 $3)))
     )

    (initialized-declarator
     (declarator ($$ `(init-declr ,$1)))
     (declarator "=" initializer ($$ `(init-declr ,$1 ,$3)))
     )

    ;; 4.3, p 83
    (storage-class-specifier
     ("auto" ($$ '(stor-spec (auto))))
     ("extern" ($$ '(stor-spec (extern))))
     ("register" ($$ '(stor-spec (register))))
     ("static" ($$ '(stor-spec (static))))
     ("typedef" ($$ '(stor-spec (typedef))))
     )

    ;; 4.3.3, p 86
    (function-specifier ("inline"))

    ;; 4.4, p 86
    (type-specifier
     (enumeration-type-specifier)
     (floating-point-type-specifier)
     (integer-type-specifier)
     (structure-type-specifier)
     (typedef-name)
     (union-type-specifier)
     (void-type-specifier)
     )

    (type-qualifier
     ("const" ($$ '(type-qual (const))))
     ("volatile" ($$ '(type-qual (volatile))))
     ("restrict" ($$ '(type-qual (restrict))))
     )

    ;; 4.5, p 95
    (declarator
     (pointer-declarator)
     (direct-declarator)
     )

    (direct-declarator
     (simple-declarator)
     ("(" declarator ")" ($$ `(scope ,$2)))
     (function-declarator)
     (array-declarator)
     )

    ;; 4.5.1, p 96
    (simple-declarator
     (identifier)
     )

    ;; 4.5.2, p 96
    (pointer-declarator
     (pointer direct-declarator ($$ `(pointer-declr ,$1 ,$2)))
     )

    (pointer
     ("*" type-qualifier-list ($$ `(pointer ,$2)))
     ("*" ($$ '(pointer)))
     ("*" type-qualifier-list pointer ($$ `(pointer ,(tl->list $2) ,$3)))
     ("*" pointer ($$ `(pointer ,$1)))
     )

    (type-qualifier-list ; (C89)
     (type-qualifier ($$ (make-tl 'type-qual-list $1)))
     (type-qualifier-list type-qualifier ($$ (tl-append $1 $2)))
     )

    ;; 4.5.3, p 97
    ;; The expansion here is very WAGgy.
    (array-declarator
     ;; direct-declarator "[" constant-expression_opt "]" <= pre-C99
     (direct-declarator
      "[" array-qualifier-list array-size-expression "]"
      ($$ `(array-of ,$1 ,$2 ,$3)))
     (direct-declarator
      "[" array-qualifier-list "]"
      ($$ `(array-of ,$1 ,$3)))
     (direct-declarator
      "[" array-size-expression "]"
      ($$ `(array-of ,$1 ,$3)))
      (direct-declarator
       "[" "]"
       ($$ `(array-of ,$1)))
      (direct-declarator
       "[" array-qualifier-list "*" "]"
       ($$ `(array-of ,$1 ,$3 "*???")))
      (direct-declarator
       "[" "*" "]"
       ($$ `(array-of ,$1 "*???")))
      )

    (array-qualifier-list
     (array-qualifier ($$ (make-tl 'array-qual-list (list $1))))
     (array-qualifier-list array-qualifier ($$ (tl-append $1 (list $2))))
     )

    (array-qualifier
     ("static") ("restrict") ("const") ("volatile"))

    (array-size-expression
     ;; I think the following may be an error:
     ;; It conflicts with xxx which uses constant-expression
     (assignment-expression)
     ;;(constant-expression)
     ;; I think the following may be an error:
     ;; it is handled by array-declarator.
     ;;("*")
     )

    ;; 4.5.4, p 100
    (function-declarator
     (direct-declarator
      "(" parameter-type-list ")"
      ($$ `(fctn-declr ,$1 ,(tl->list $3))))
     (direct-declarator
      "(" identifier-list ")"
      ($$ `(fctn-declr ,$1 ,(tl->list $3))))
     (direct-declarator
      "(" ")"
      ($$ `(fctn-declr ,$1)))
     )

    (parameter-type-list
     (parameter-list)
     (parameter-list "," "...")
     )

    (parameter-list
     (parameter-declaration ($$ (make-tl 'param-list $1)))
     (parameter-list "," parameter-declaration ($$ (tl-append $1 $3)))
     )

    (parameter-declaration
     (declaration-specifiers
      declarator
      ($$ `(param-decln ,(tl->list $1) ,$2)))
     (declaration-specifiers
      abstract-declarator
      ($$ `(param-decln ,(tl->list $1) ,$2)))
     (declaration-specifiers
      ($$ `(param-decln ,(tl->list $1))))
     )

    (identifier-list
     (identifier ($$ (make-tl 'ident-list $1)))
     (identifier-list "," identifier ($$ (tl-append $1 $3)))
     )

    ;; 4.6, p 103
    (initializer
     (assignment-expression ($$ `(initzer ,$1)))
     ("{" initializer-list "," "}" ($$ `(initzer ,(tl->list $2))))
     ("{" initializer-list "}" ($$ `(initzer ,(tl->list $2))))
     )

    ;; The designation productions are from C99.
    (initializer-list
     (initializer ($$ (make-tl 'initzer-list $1)))
     (initializer-list "," initializer ($$ (tl-append $1 $3)))
     (designation initializer ($$ (make-tl 'initzer-list $1 $2)))
     (initializer-list "," designation initializer ($$ (tl-append $1 $3 $4)))
     )

    (designation
     (designator-list "=")
     )

    (designator-list
     (designator ($$ (make-tl 'desgr-list $1)))
     (designator-list designator ($$ (tl-append $1 $2)))
     )

    (designator
     ("[" constant-expression "]" ($$ (list 'array-dsgr $2)))
     ("." identifier ($$ (list 'sel-dsgr $2)))
     )

    ;; 5.1, p 125
    (integer-type-specifier
     (signed-type-specifier)
     (unsigned-type-specifier)
     (character-type-specifier)
     (bool-type-specifier)
     )

    ;; 5.1.1, p 125
    ;; The productions shown in the book are not LALR1 so we will need to use
    ;; static semantics.
    (signed-type-specifier
     ("short" ($$ '(short)))
     ("int" ($$ '(int)))
     ("signed" ($$ '(int)))
     ("long" ($$ '(long)))
     )
    #| 
    (signed-type-specifier ;; Enforce with static semantics.
     ("short" ($$ '(signed (short-int))))
     ("short" "int" ($$ '(signed (short-int))))
     ("signed" "short" ($$ '(signed (short-int))))
     ("signed" "short" "int" ($$ '(signed (short-int))))
     ("int" ($$ '(signed (int))))
     ("signed" ($$ '(signed (int))))
     ("signed" "int" ($$ '(signed (int))))
     ("long" ($$ '(signed (long-int))))
     ("long" "int" ($$ '(signed (long-int))))
     ("signed" "long" ($$ '(signed (long-int))))
     ("signed" "long" "int" ($$ '(signed (long-int))))
     ("long" "long" ($$ '(signed (long-long-int))))
     ("long" "long" "int" ($$ '(signed (long-long-int))))
     ("signed" "long" "long" ($$ '(signed (long-long-int))))
     ("signed" "long" "long" "int" ($$ '(signed (long-long-int))))
     )
    |#

    ;; 5.1.2, p 128
    (unsigned-type-specifier
     ("unsigned" ($$ '(unsigned)))
     )
    #|
    (unsigned-type-specifier
     ("unsigned" "short" "int" ($$ '(unsigned (short-int))))
     ("unsigned" "short" ($$ '(unsigned (short-int))))
     ("unsigned" "int" ($$ '(unsigned (int))))
     ("unsigned" ($$ '(unsigned (int))))
     ("unsigned" "long" "int" ($$ '(unsigned (long-int))))
     ("unsigned" "long" ($$ '(unsigned (long-int))))
     ("unsigned" "long" "long" "int" ($$ '(unsigned (long-long-int))))
     ("unsigned" "long" "long" ($$ '(unsigned (long-long-int))))
     )
    |#

    ;; 5.1.3, p 129
    (character-type-specifier ;; Enforce with static semantics.
     ("char" ($$ '(char)))
     )
    #|
    (character-type-specifier ;; Enforce with static semantics.
     ("char" ($$ '(char)))
     ("signed" "char" ($$ '(signed (char))))
     ("unsigned" "char" ($$ '(unsigned (char))))
     )
    |#

    ;; 5.1.5, p 132, discussed but not defined
    (bool-type-specifier
     ("_Bool" ($$ '(bool)))
     )

    ;; 5.2, p 132
    (floating-point-type-specifier
     ("float" ($$ '(float)))
     ("double" ($$ '(double)))
     (complex-type-specifier)
     )
    #|
    (floating-point-type-specifier
     ("float" ($$ '(float)))
     ("double" ($$ '(double)))
     ("long" "double" ($$ '(long-double)))
     (complex-type-specifier)
     )
    |#

    ;; 5.2.1, p 136
    (complex-type-specifier
     ("_Complex" ($$ '(complex)))
     )
    #|
    (complex-type-specifier
     ("_Complex" ($$ '(complex)))
     ("float" "_Complex" ($$ '(complex (float))))
     ("double" "_Complex" ($$ '(complex (double))))
     ("long" "double" "_Complex" ($$ '(complex (long-double))))
     )
    |#

    ;; 5.5, p 145
    (enumeration-type-specifier
     (enumeration-type-definition)
     (enumeration-type-reference)
     )

    (enumeration-type-definition
     ("enum" enumeration-tag "{" enumeration-definition-list "}"
      ($$ `(enum-def ,$1 ,(tl->list $4))))
     ("enum" "{" enumeration-definition-list "}"
      ($$ `(enum-def ,(tl->list $4))))
     ("enum" enumeration-tag "{" enumeration-definition-list "," "}"
      ($$ `(enum-def ,$1 ,(tl->list $4))))
     ("enum" "{" enumeration-definition-list "," "}"
      ($$ `(enum-def ,(tl->list $4))))
     )
    
    (enumeration-type-reference
     ("enum" enumeration-tag ($$ `(enum-ref ,$2)))
     )

    (enumeration-tag (identifier))

    (enumeration-definition-list
     (enumeration-constant-definition ($$ (make-tl 'enum-def-list $1)))
     (enumeration-definition-list "," enumeration-constant-definition
				  ($$ (tl-append $1 $3)))
     )

    (enumeration-constant-definition
     (enumeration-constant ($$ `(enum-defn ,$1)))
     ;; I think this may be an error.  It's replacement follows.
     ;; If expression, then comma-expressions are allowed but enum uses
     ;; commas in list of enumerations.
     ;;(enumeration-constant "=" expression)  ;;  --[change to]--> 
     (enumeration-constant "=" constant-expression
			   ($$ `(enum-defn ,$1 ,$3)))
     )
    (enumeration-constant (identifier))

    ;; 5.6, p 148
    (structure-type-specifier
     (structure-type-definition)
     (structure-type-reference)
     )

    (structure-type-definition
     ("struct" structure-tag "{" field-list "}"
      ($$ `(struct-def ,$1 ,(tl->list $4))))
     ("struct" "{" field-list "}"
      ($$ `(struct-def ,(tl->list $3))))
     )

    (structure-type-reference
     ("struct" structure-tag ($$ `(struct-ref ,$1)))
     )

    (structure-tag (identifier))

    (field-list
     (component-declaration ($$ (make-tl 'field-list $1)))
     (field-list component-declaration ($$ (tl-append $1 $2)))
     (field-list lone-comment ($$ (tl-append $1 $2)))
     )

    (component-declaration
     (type-specifier
      component-declarator-list ";" opt-code-comment
      ($$ (if (pair? $4)
	      `(comp-decln ,$1 ,(tl->list $2) ,$4)
	      `(comp-decln ,$1 ,(tl->list $2)))))
     )

    (component-declarator-list
     (component-declarator ($$ (make-tl 'comp-declr-list $1)))
     (component-declarator-list "," component-declarator ($$ (tl-append $1 $3)))
     )

    (component-declarator
     (simple-component)
     (bit-field)
     )

    (simple-component (declarator))

    ;; TEST
    (bit-field
     (declarator ":" width ($$ `(bit-field ,$1 ,$3)))
     (":" width ($$ `(bit-field ,$2)))
     )

    (width (constant-expression))

    ;; 5.8, p 161
    (union-type-specifier
     (union-type-definition)
     (union-type-reference)
     )

    (union-type-definition
     ("union" union-tag "{" field-list "}" ($$ `(union-def ,$1 ,(tl->list $4))))
     ("union" "{" field-list "}" ($$ `(union-def ,(tl->list $3))))
     )

    (union-type-reference
     ("union" union-tag ($$ `(struct-ref ,$1)))
     )
    (union-tag (identifier))

    ;; 5.9, p 168
    (void-type-specifier ("void"))

    ;; 5.10, p 168
    ;;(typedef-name (identifier)) must be hacked w/ the lexical analyzer
    (typedef-name ('typename ($$ `(typename ,$1))))

    ;; 5.12, p 176
    (type-name
     (declaration-specifiers
      abstract-declarator
      ($$ `(type-name ,(tl->list $1) ,$2))) ;; ???
     (declaration-specifiers
      ($$ `(type-name ,(tl->list $1)))) ;; ???
     )

    (abstract-declarator
     (pointer)
     (pointer direct-abstract-declarator ($$ `(abs-declr ,$1 ,$2)))
     (direct-abstract-declarator)
     )

    ;; The following in 4.5.2
    ;; pointer =>
    ;; type-qualifier-list => 

    ;; I have removed the constant-expression productions below because
    ;; these will conflict with expression which expands to constant-expr.
    (direct-abstract-declarator
     ("(" abstract-declarator ")" ($$ `(declr-scope ,$2)))
     ;;(direct-abstract-declarator "[" constant-expression "]")
     (direct-abstract-declarator "[" "]" ($$ `(declr-array ,$1)))
     ;;("[" constant-expression "]")
     ("[" "]")
     ;;
     (direct-abstract-declarator "[" expression "]" ($$ `(declr-arry ,$1 ,$3)))
     ("[" expression "]" ($$ `(declr-anon-arry ,$2))) ;; ?????
     ;;
     (direct-abstract-declarator "[" "*" "]" ($$ `(declr-STAR ,$1)))
     ("[" "*" "]" ($$ '(declr-STAR)))
     ;;
     (direct-abstract-declarator "(" parameter-type-list ")"
				 ($$ `(declr-fctn ,$1 ,(tl->list $3))))
     (direct-abstract-declarator "(" ")" ($$ `(declr-fctn ,$1)))
     ("(" parameter-type-list ")" ($$ `(declr-anon-fctn ,(tl->list $2))))
     ("(" ")" ($$ '(declr-anon-fctn)))
     )

    ;; 7.3, p 207
    (primary-expression
     (identifier ($$ `(p-expr ,$1)))
     (constant ($$ `(p-expr ,$1)))
     (parenthesized-expression)
     )

    ;; 7.3.3, p 209
    (parenthesized-expression
     ("(" expression ")" ($$ `(p-expr ,$2))))

    ;; 7.4, p 210
    (postfix-expression
     (primary-expression)
     (subscript-expression)
     (component-selection-expression)
     (function-call)
     (postincrement-expression)
     (postdecrement-expression)
     (compound-literal)
     )

    ;; 7.4.1, p 210
    (subscript-expression
     (postfix-expression "[" expression "]")
     )

    ;; 7.4.2, p 212
    (component-selection-expression
     (direct-component-selection)
     (indirect-component-selection)
     )

    (direct-component-selection
     (postfix-expression "." identifier ($$ `(d-sel ,$3 ,$1)))
     )
    
    (indirect-component-selection
     (postfix-expression "->" identifier ($$ `(i-sel ,$3 ,$1)))
     )

    ;; 7.4.3, p 214
    (function-call
     (postfix-expression "(" expression-list ")" ($$ `(fctn-call ,$1 ,$2)))
     (postfix-expression "(" ")" ($$ `(fctn-call ,$1)))
     )

    (expression-list
     (assignment-expression ($$ (make-tl 'expr-list $1)))
     (expression-list "," assignment-expression ($$ (tl-append $1 $3)))
     )

    ;; 7.4.4, p 216
    (postincrement-expression
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     )
    
    (postdecrement-expression
     (postfix-expression "--" ($$ `(post-dec ,$1)))
     )

    ;; 7.4.5, p 217
    (compound-literal
     ("(" type-name ")" "{" initializer-list "}"
      ($$ `(comp-literal ,$2 ,(tl->list $5))))
     ("(" type-name ")" "{" initializer-list "," "}"
      ($$ `(comp-literal ,$2 ,(tl->list $5))))
     )

    ;; 7.5, p 219
    (cast-expression
     (unary-expression)
     ("(" type-name ")" cast-expression ($$ `(cast ,$2 ,$4)))
     )

    (unary-expression
     (postfix-expression)
     (sizeof-expression)
     (unary-minus-expression)
     (unary-plus-expression)
     (logical-negation-expression)
     (bitwise-negation-expression)
     (address-expression)
     (indirection-expression)
     (preincrement-expression)
     (predecrement-expression)
     )
    
    ;; 7.5.2, p 220
    (sizeof-expression
     ("sizeof" "(" type-name ")" ($$ `(sizeof-type ,$3)))
     ("sizeof" unary-expression ($$ `(sizeof-expr ,$2)))
     )

    ;; 7.5.3, p 222
    (unary-minus-expression
     ("-" cast-expression ($$ `(neg ,$2)))
     )

    ;; (C89)
    (unary-plus-expression
     ("+" cast-expression ($$ `(pos ,$2)))
     )

    ;; 7.5.4, p 223
    (logical-negation-expression
     ("!" cast-expression)
     )
    
    ;; 7.5.5, p 223
    (bitwise-negation-expression
     ("~" cast-expression)
     )

    ;; 7.5.6, p 224
    (address-expression
     ("&" cast-expression)
     )

    ;; 7.5.7, p 225
    (indirection-expression
     ("*" cast-expression)
     )

    ;; 7.5.8, p 226
    (preincrement-expression
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     )

    (predecrement-expression
     ("--" unary-expression ($$ `(pre-dec ,$2)))
     )

    ;; 7.6.1, p 227
    (multiplicative-expression
     (cast-expression)
     (multiplicative-expression mult-op cast-expression ($$ (list $2 $1 $3)))
     )
    (mult-op ("*" ($$ 'mul)) ("/" ($$ 'div)) ("%" ($$ 'mod)))

    ;; 7.6.2, p 229
    (additive-expression
     (multiplicative-expression)
     (additive-expression add-op multiplicative-expression
			  ($$ (list $2 $1 $3)))
     )
    (add-op ("+" ($$ 'add)) ("-" ($$ 'sub)))

    ;; 7.6.3, p 231
    (shift-expression
     (additive-expression)
     (shift-expression shift-op additive-expression ($$ (list $2 $1 $3)))
     )
    (shift-op ("<<" ($$ 'lshift)) (">>" ($$ 'rshift)))

    ;; 7.6.4, p 233
    (relational-expression
     (shift-expression)
     (relational-expression relational-op shift-expression
			    ($$ (list $2 $1 $3)))
     )
    (relational-op
     ("<" ($$ 'lt)) ("<=" ($$ 'le)) (">" ($$ 'gt)) (">=" ($$ 'ge)))

    ;; 7.6.5, p 234
    (equality-expression
     (relational-expression)
     (equality-expression equality-op relational-expression
			  ($$ (list $2 $1 $3)))
     )
    (equality-op ("==" ($$ 'eq)) ("!=" ($$ 'ne)))

    ;; 7.6.6, p 236
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3)))
     )
    
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3)))
     )
    
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3)))
     )

    ;; 7.7, p 242
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression
			    ($$ `(or ,$1 ,$3)))
     )
    
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression
			     ($$ `(and ,$1 ,$3)))
     )

    ;; 7.8, p 244
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" expression ":" conditional-expression
			    ($$ `(cond-expr $1 $2 $3)))
     )

    ;; 7.9, p 246
    (assignment-expression
     (conditional-expression)
     (unary-expression assignment-op assignment-expression
		       ($$ (list $2 $1 $3)))
     )
    (assignment-op
     ("=" ($$ 'assign)) ("+=" ($$ 'add-assign)) ("-=" ($$ 'sub-assign))
     ("*=" ($$ 'mul-assign)) ("/=" ($$ 'div-assign)) ("%=" ($$ 'mod-assign))
     ("<<=" ($$ 'lshift-assign)) (">>=" ($$ 'rshift-assign))
     ("&=" ($$ 'and-assign)) ("^=" ($$ 'xor-assign)) ("|=" ($$ 'or-assign)))

    ;; 7.10, p 249
    (comma-expression
     (assignment-expression)
     (comma-expression "," assignment-expression)
     )

    (expression
     (comma-expression))

    ;; 7.11, constant-expression explained, provided in appendix
    (constant-expression
     (conditional-expression)
     ;;('$hack)
     )
    
    ;; 8, p 259
    (statement
     (expression-statement)
     (labeled-statement)
     (compound-statement)
     (conditional-statement)
     (iterative-statement)
     (switch-statement)
     (break-statement)
     (continue-statement)
     (return-statement)
     (goto-statement)
     (null-statement)
     )

    ;; See the following productions below, in 8.5 and 8.6
    ;; conditional-statement =>
    ;; iterative-statement => 

    ;; 8.2, p 260
    (expression-statement
     (expression ";")
     )

    ;; 8.3, p 261
    (labeled-statement
     (label ":" statement)
     )
    (label (named-label) (case-label) (default-label))

    ;; 8.4, p 262
    (compound-statement
     ("{" declaration-or-statement-list "}")
     ("{" "}")
     )

    (declaration-or-statement-list
     (declaration-or-statement)
     (declaration-or-statement-list declaration-or-statement)
     )

    (declaration-or-statement
     (declaration)
     (statement)
     )

    ;; 8.5, p 264
    (conditional-statement
     (if-statement)
     (if-else-statement)
     )

    (if-statement
     ("if" "(" expression ")" statement ($$ `(if ,$3 ,$5)))
     )

    (if-else-statement
     ("if" "(" expression ")" statement "else" statement ($$ `(if ,$3 ,$5 ,7)))
     )

    ;; 8.6, p 266
    (iterative-statement
     (while-statement)
     (do-statement)
     (for-statement)
     )

    ;; 8.6.1, p 267
    (while-statement
     ("while" "(" expression ")" statement ($$ `(while ,$3 ,$5)))
     )

    ;; 8.6.2, p 268
    (do-statement
     ("do" statement "while" "(" expression ")" ";" ($$ `(do-while ,$2 ,$5)))
     )

    ;; 8.6.3, p 269
    (for-statement
     ("for" for-expressions statement ($$ `(for ,@$2 $3)))
     )

    (for-expressions
     ("(" initial-clause expression ";" expression ")" ($$ (list $2 $3 $5)))
     ("(" initial-clause expression ";" ")" ($$ (list $2 $3 '(expr))))
     ("(" initial-clause ";" expression ")" ($$ (list $2 '(expr) $4)))
     ("(" initial-clause ";" ")" ($$ (list $2 '(expr) '(expr))))
     )
    
    (initial-clause
     (expression ";")
     (";" ($$ '(expr)))
     (declaration)
     )

    ;; 8.7, p 274
    (switch-statement
     ("switch" "(" expression ")" statement)
     )

    (case-label
     ("case" constant-expression)
     )

    (default-label
      ("default")
      )

    ;; 8.8, p 277
    (break-statement ("break" ";" ($$ '(break))))

    (continue-statement ("continue" ";" ($$ '(continue))))

    ;; 8.9, p 279
    (return-statement
     ("return" expression ";" ($$ `(return $2)))
     ("return" ";" ($$ `(return (expr))))
     )

    ;; 8.10, p 280
    (goto-statement
     ("goto" named-label ";" ($$ `(goto $2)))
     )
    (named-label (identifier))

    ;; 8.11, p 281
    (null-statement
     (";" ($$ '(null-stmt))))

    ;; 9.1, p 286
    (translation-unit
     (top-level-declaration ($$ (make-tl 'trans-unit $1)))
     (translation-unit top-level-declaration ($$ (tl-append $1 $2)))
     )

    (top-level-declaration
     (declaration)
     (function-definition)
     (lone-comment)
     (cpp-statement)
     )

    (function-definition
     (function-def-specifier compound-statement)
     )

    (function-def-specifier
     ;; declaration-specifiers_opt declarator declaration-list_opt
     (declaration-specifiers
      declarator declaration-list
      ($$ `(fctn-defn1a ,(tl->list $1) ,$2 ,(tl->list $3))))
     (declaration-specifiers
      declarator
      ($$ `(fctn-defn1b ,(tl->list $1) ,$2)))
     (declarator
      ($$ `(fctn-defn1c ,$1)))
     )

    (declaration-list
     (declaration ($$ (make-tl $1)))
     (declaration-list declaration ($$ (tl-append $1 $2)))
     )

    ;; 9.1, p 287
    ;; These productions are repeated in the book.  See 4.5.4 above.
    ;; function-declarator =>
    ;; parameter-type-list =>
    ;; parameter-list =>
    ;; parameter-declaration =>
    ;; identifier-list =>

    (opt-code-comment () (code-comment))

    ;; non-terminal leaves
    (identifier
     ('$ident ($$ `(ident ,$1)))
     ('cpp-ident ($$ `(ident ,$1)))
     )
    (constant
     ('$fx ($$ `(fixed ,$1)))		; integer-constant
     ('$fl ($$ `(float ,$1)))		; floating-constant
     ('$ch ($$ `(char ,$1)))		; char-constant
     ('$string ($$ `(string ,$1)))	; string-constant
     )
    (code-comment ('$code-comm ($$ `(comment ,$1))))
    (lone-comment ('$lone-comm ($$ `(comment ,$1))))
    (cpp-statement ('cpp-stmt ($$ `(cpp-stmt ,$1))))
    )))

(define clang-mach
  (compact-machine
   ;;(identity
   (hashify-machine
    ;;(identity
    (make-lalr-machine clang-spec))))

(define len-v (assq-ref clang-mach 'len-v))
(define pat-v (assq-ref clang-mach 'pat-v))
(define rto-v (assq-ref clang-mach 'rto-v))
(define mtab (assq-ref clang-mach 'mtab))
(define sya-v (vector-map
               (lambda (ix nrg guts) (wrap-action nrg guts))
               (assq-ref clang-mach 'nrg-v) (assq-ref clang-mach 'act-v)))
(define act-v (vector-map (lambda (ix f) (eval f (current-module))) sya-v))

(include "pbody.scm")

(define raw-parser (make-lalr-parser clang-mach))

(define (run-parse) (raw-parser (gen-c-lexer)))

(define* (dev-parse-c #:key (cpp-defs '()) (inc-dirs '()))
  (let ((info (make-cpi cpp-defs inc-dirs)))
    (with-fluid* *info* info (lambda () (raw-parser (gen-c-lexer))))))

;(define dev-parse-c parse-c)

;; --- last line
