;;; lang/dxl/mach.scm
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
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

;; DOORS dxl parser

(define-module (nyacc lang dxl mach)
  #:export (dxl-spec dxl-mach dev-parse-dxl gen-dxl-lexer gen-dxl-files)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-43) #:select (vector-map))
  ;;#:use-module ((sxml xpath) #:select (sxpath))
  )

;; NSI = "no semi-colon insertion"

;; challenges:
;; { return "" }  : no semi

(define dxl-spec
  (lalr-spec
   (notice "Copyright (C) 2015 Matthew R. Wette -- all rights reserved")
   (prec< "then" "else")	       ; then/else SR-shift resolution
   (prec< "*" 'UNARY)		       ; unary
   ;;(expect 3)			       ; "if (ex) st" vs "if (ex) then st"
   (start program-proxy)
   (grammar
 
    (program-proxy (program ($$/ref 'a001 (tl->list $1))))

    (program
     (program-element
      ($$/ref 'a011 (if $1 (make-tl 'program $1) (make-tl 'program))))
     (program ";" program-element ($$/ref 'a012 (if $3 (tl-append $1 $3) $1)))
     )

    (program-element
     ($empty ($$ #f))
     (pragma ($$-ref 'a021))
     (statement ($$-ref 'a022))
     (function-definition ($$-ref 'a023))
     )

    (pragma
     ("pragma" identifier const-expr-list
      ($$ `(pragma ,$2 ,(tl->list $3))))
     )
    (const-expr-list
     ($empty ($$/ref 'a031 (make-tl 'const-expr-list)))
     (const-expr-list "," constant ($$/ref 'a032 (tl-append $1 $3)))
     )

    (function-definition
     (type-name identifier "(" param-list ")" "{" statement-list "}"
		($$/ref 'a041
			`(fctn-defn ,$1 ,$2 ,(tl->list $4) ,(tl->list $7))))
     (type-name identifier "(" ")" "{" statement-list "}"
		($$/ref 'a042
			`(fctn-defn ,$1 ,$2 (param-list) ,(tl->list $6))))
     )

    (param-list
     (type-name identifier
		($$/ref 'a051 (make-tl 'param-list `(param ,$1 ,$2))))
     (type-name "&" identifier
		($$/ref 'a052 (make-tl 'param-list `(ref-param ,$1 ,$2))))
     (param-list "," type-name identifier
		 ($$/ref 'a053 (tl-append $1 `(param ,$3 ,$4))))
     (param-list "," type-name "&" identifier
		 ($$/ref 'a054 (tl-append $1 `(ref-param ,$3 ,$4))))
     (param-list "," identifier
		 ($$/ref 'a055 (tl-append $1 `(param ,$3))))
     (param-list "," "&" identifier
		 ($$/ref 'a056 (tl-append $1 `(ref-param ,$3 ,$4))))
     )

    ;; The two forms of if-then are ambiguous because
    ;; "(" conditional-expression . ")" is also a primary so we can have
    ;;   if ( expr ) then ...
    ;; The reference manual says the above is a syntax error so we should
    ;; shift.
    (statement
     ("if" "(" conditional-expression ")" statement ($prec "then")
      ($$/ref 'a061 `(if ,$3 ,$5)))
     ("if" "(" conditional-expression ")" statement "else" statement
      ($$/ref 'a062 `(if ,$3 ,$5 ,$7)))
     ("if" expression "then" statement
      ($$/ref 'a063 `(if ,$2 ,$4)))
     ("if" expression "then" statement "else" statement
      ($$/ref 'a064 `(if)))
     ("for" identifier "in" expression "do" statement
      ($$/ref 'a065 `(for-in ,$2 ,$4 ,$6)))
     ("for" identifier "in" expression "->" expression "do" statement
      ($$/ref 'a065 `(for-all-olinks ,$2 ,$4 ,$6 ,$8)))
     ("for" identifier "in" expression "<-" expression "do" statement
      ($$/ref 'a065 `(for-all-ilinks ,$2 ,$4 ,$6 ,$8)))
     ("for" for-expressions statement
      ($$/ref 'a066 `(for ,(car $2) ,(cadr $2) ,(caddr $2) ,$3)))
     ("while" "(" expression ")" statement
      ($$/ref 'a067 `(while ,$3 ,$5)))
     ("{" "}"
      ($$/ref 'a068 '(comp-stmt)))
     ("{" statement-list "}"
      ($$/ref 'a069 `(comp-stmt ,(tl->list $2)))) ; compound
     (type-name variable-definition-list
		($$/ref 'a06A `(vble-defn ,$1 ,(tl->list $2))))
     (expression ($$/ref 'a06B `(expr-stmt ,$1)))
     ("return" expression ($$/ref 'a06C `(return ,$2)))
     ("return" ($$/ref 'a06D `(return)))
     )

    (type-name (type-name-1 ($$/ref 'a06E `(type-name ,$1))))
    (type-name-1
     ("bool") ("char") ("int") ("real") ("void") ("string")
     ('built-in-type)			; Object, Module, etc.
     )
    
    (variable-definition-list		; 07
     (variable-definition-list-item ($$/ref 'a071 (make-tl 'vble-defn-list $1)))
     (variable-definition-list "," variable-definition-list-item
			       ($$/ref 'a072 (tl-append $1 $3)))
     )
    (variable-definition-list-item
     (identifier ($$/ref 'a073 `(vble-list-item ,$1)))
     (identifier "=" conditional-expression
		 ($$/ref 'a074 `(vble-list-item ,$1 (initzer ,$3))))
     )
    
    (for-expressions
     ("(" opt-expr-w-term opt-expr-w-term expression ")"
      ($$ (list $2 $3 $4)))
     ("(" opt-expr-w-term opt-expr-w-term ")"
      ($$ (list $2 $3 '(expr))))
      )
    (opt-expr-w-term
     (expression ";")
     (";" ($$ '(expr)))
     )

    (statement-list			; 08
     (statement ($$/ref 'a081 (make-tl 'stmt-list $1)))
     (statement-list ";" statement ($$/ref 'a082 (tl-append $1 $3)))
     )

    #;(expression-list			; 09
     (conditional-expression ($$/ref 'a091 (make-tl 'expr-list $1)))
     (expression-list "," conditional-expression
		      ($$/ref 'a092 (tl-append $1 $3)))
    )

    (expression				; 10
     (comma-expression)
     )
    (comma-expression			; 13
     (assignment-expression)
     (comma-expression "," assignment-expression
		       ($$/ref 'a131 `(comma-expr ,$1 ,$3)))
     )
    (assignment-expression		; 14
     (conditional-expression)
     (postfix-expression assignment-op assignment-expression
			 ($$ `(assn-expr ,$1 (op ,$2) ,$3)))
     )
    #;(assignment-op			; 15
     ("=" ($$ 'assign)) ("+=" ($$ 'add-assign)) ("-=" ($$ 'sub-assign))
     ("*=" ($$ 'mul-assign)) ("/=" ($$ 'div-assign)) ("%=" ($$ 'mod-assign))
     ("<<=" ($$ 'lshift-assign)) (">>=" ($$ 'rshift-assign))
    ("&=" ($$ 'and-assign)) ("^=" ($$ 'xor-assign)) ("|=" ($$ 'or-assign)))
    (assignment-op
     ("=") ("+=") ("-=") ("*=") ("/=") ("%=") 
     ("<<=") (">>=") ("&=") ("^=") ("|="))

    (conditional-expression		; 16
     (logical-or-expression)
     (logical-or-expression "?" expression ":" conditional-expression
			    ($$/ref 'a161 `(cond-expr ,$1 ,$3 ,$5)))
     )

    (logical-or-expression		; 17
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression
			    ($$/ref 'a171 `(or ,$1 ,$3)))
     (logical-or-expression "or" logical-and-expression
			    ($$/ref 'a172 `(or ,$1 ,$3)))
     )
    (logical-and-expression		; 18
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression
			    ($$/ref 'a182 `(and ,$1 ,$3)))
     (logical-and-expression "and" bitwise-or-expression
			    ($$/ref 'a182 `(and ,$1 ,$3)))
     )
    (bitwise-or-expression		; 19
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$/ref 'a191 `(bit-or ,$1 ,$3)))
     )
    (bitwise-xor-expression		; 20
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			    ($$/ref 'a201 `(bit-xor ,$1 ,$3)))
     )
    (bitwise-and-expression		; 21
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			    ($$/ref 'a211 `(bit-and ,$1 ,$3)))
     )
    (equality-expression		; 22
     (relational-expression)
     (equality-expression "==" relational-expression
			    ($$/ref 'a221 `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression
			    ($$/ref 'a221 `(neq ,$1 ,$3)))
     )

    (relational-expression		; 23
     (shift-expression)
     (relational-expression "<" shift-expression ($$/ref 'a231 `(lt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$/ref 'a232 `(lt ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$/ref 'a233 `(le ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$/ref 'a234 `(ge ,$1 ,$3)))
     )

    (shift-expression			; 24
     (additive-expression)
     (shift-expression shift-op additive-expression
			    ($$/ref 'a241 (list $2 $1 $3)))
     )
    (shift-op ("<<" ($$/ref 'a242 'lshift)) (">>" ($$/ref 'a243 'rshift)))

    (additive-expression		; 25
     (multiplicative-expression)
     (additive-expression add-op multiplicative-expression
			    ($$/ref 'a251 (list $2 $1 $3)))
     )
    (add-op ("+" ($$/ref 'a252 'add)) ("-" ($$/ref 'a253 'sub)))

    (multiplicative-expression		; 26
     (cast-expression)
     (multiplicative-expression mult-op cast-expression
				($$/ref 'a261 (list $2 $1 $3)))
     )
    (mult-op ("*" ($$/ref 'a262 'mul)) ("/" ($$/ref 'a263 'div))
	     ("%" ($$/ref 'a264 'mod)))

    (cast-expression			; 27
     (unary-expression)
     ("(" type-name cast-expression ")"
      ($$/ref 'a271 `(cast-expr ,$2 ,$3)))
     ;;(expression type-name)
     )
    
    (unary-expression			; 28
     (concatenation)
     ("sizeof" unary-expression ($$ `(sizeof ,$2)))
     ("-" unary-expression ($prec 'UNARY) ($$ `(neg ,$2)))
     ("+" unary-expression ($prec 'UNARY) ($$ `(pos ,$2)))
     ("!" unary-expression ($$ `(not ,$2)))
     ("~" unary-expression ($$ `(what ,$2)))
     ("&" cast-expression ($$ `(ref-to ,$2)))
     ;;("*" cast-expression)
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2)))
     )

    (concatenation			; 30
     (postfix-expression)
     (postfix-expression concatenation ($$ `(concat ,$1 ,$2)))
    )
    
    (postfix-expression			; 29
     (primary-expression)
     (postfix-expression "." $string ($$ `(obj-sel ,$1 (string ,$3))))
     (postfix-expression "." identifier ($$ `(obj-sel ,$1 ,$3)))
     (postfix-expression "[" conditional-expression "]" ;; only 1 dim in dxl
			 ($$ `(array-ref ,$1 ,$3)))
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1)))
     (postfix-expression ":" primary-expression ($$ `(range ,$1 ,$3)))
     (postfix-expression ":" primary-expression "by" primary-expression
			 ($$ `(range ,$1 ,$3 ,$5)))
     )

    (primary-expression			; 31
     (identifier ($$ `(p-expr ,$1)))
     ;;(identifier array-ref)
     (constant ($$ `(p-expr ,$1)))
     ("(" expression ")" ($$/ref 'a312 $2))
     ("(" "current" type-name ")"
      ($$/ref 'a314 `(current (@ (type ,(sx-ref $3 1))))))
     ("current" ($$ '(current))) ;; ref' to current Object, Module, ...
     )

    (identifier				; 32
     ($ident ($$/ref 'a321 `(ident ,$1))))

    (constant				; 33
     ($fixed ($$ `(fixed ,$1)))	; integer-constant
     ($float ($$ `(float ,$1)))	; floating-constant
     ($chlit ($$ `(char ,$1)))		; char-constant
     ($string ($$ `(string ,$1)))	; string-constant
     )
    ;;(code-comment ($code-comm ($$ `(comment ,$1))))
    ;;(lone-comment ($lone-comm ($$ `(comment ,$1))))
    )))

(define dxl-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine dxl-spec))))

(define len-v (assq-ref dxl-mach 'len-v))
(define pat-v (assq-ref dxl-mach 'pat-v))
(define rto-v (assq-ref dxl-mach 'rto-v))
(define mtab (assq-ref dxl-mach 'mtab))
(define sya-v (vector-map (lambda (ix actn) (wrap-action actn))
                          (assq-ref dxl-mach 'act-v)))
(define act-v (vector-map (lambda (ix f) (eval f (current-module))) sya-v))

(include-from-path "nyacc/lang/dxl/body.scm")

(define raw-parser (make-lalr-parser dxl-mach))

(define* (dev-parse-dxl #:key debug)
  (catch
   'parse-error
   (lambda ()
     (raw-parser (gen-dxl-lexer) #:debug debug))
   (lambda (key fmt . rest)
     (apply simple-format (current-error-port) fmt rest)
     #f)))

;; === automaton file generator

(define (gen-dxl-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions dxl-mach (xtra-dir "dxlact.scm.new"))
  (write-lalr-tables dxl-mach (xtra-dir "dxltab.scm.new"))
  (let ((a (move-if-changed (xtra-dir "dxlact.scm.new")
			    (xtra-dir "dxlact.scm")))
	(b (move-if-changed (xtra-dir "dxltab.scm.new")
			    (xtra-dir "dxltab.scm"))))
    (when (or a b) 
      (system (string-append "touch " (lang-dir "parser.scm"))))))


;; --- last line ---
