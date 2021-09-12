;;; lang/tsh/mach.scm - code and expr grammars

;; Copyright (C) 2021 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;; 1) set-indexed must be predefined identifier
;;    set d [make-array (10, 10)]  # 10 x 10 array
;;    set d [make-vector f64 (10, 10)]  # 10 x 10 matrix

;;; Code:

(define-module (nyacc lang tsh mach)
  #:export (gen-tsh-files
	    tsh-spec tsh-mach 
	    ;;tsh-code-spec tsh-code-mach 
	    ;;tsh-expr-spec tsh-expr-mach 
	    )
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise))

;; need #<unspecified>

(define tsh-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2021 Matthew R. Wette"
			  license-lgpl3+))
   ;;(expect 0)
   ;;(reserve '$code-comm)
   (start top)
   (grammar

    (top
     (item-list))

    (item-list (item-list-1 ($$ (tl->list $1))))
    (item-list-1
     (item ($$ (make-tl 'item-list $1)))
     (item-list-1 item ($$ (tl-append $1 $2))))

    ;; item: top-level declaration or decl-stmt or exec-stmt
    (item
     (topl-decl term)
     (stmt term))

    (topl-decl
     ("proc" ident "{" arg-list "}" "{" stmt-list "}")
     ;;^ syntax sugar for: set foo [lambda { x y z } { ($x + $y + $z) }
     )

    (arg-list
     (arg-list-1 ($$ (tl->list $1))))
    (arg-list-1
     ($empty ($$ (make-tl 'arg-list)))
     (arg-list-1 ident ($$ (tl-append $1 `(arg ,$2))))
     (arg-list-1 "{" ident unit-expr "}"  ($$ (tl-append $1 `(arg ,$3 ,$4)))))

    (stmt-list
     (stmt-list-1 ($$ (tl->list $1))))
    (stmt-list-1
     (stmt ($$ (make-tl 'stmt-list $1)))
     (stmt-list-1 term stmt ($$ (tl-append $1 $3))))
    
    (stmt
     (decl-stmt)
     (exec-stmt)
     ($empty ($$ `(empty-stmt)))
     ($lone-comm ($$ `(comment ,$1))))
     
    (decl-stmt
     ("set" ident unit-expr ($$ `(set ,$2 ,$3)))
     ("set" ident/ix expr-list unit-expr ($$ `(set-ix ,$2 ,$3 ,$4)))
     )

    (exec-stmt
     ;;("{" stmt-list "}")
     ;;("while" unit-expr "{" stmt-list "}")
     (if-stmt)
     (ident expr-seq ($$ `(call ,$1 ,@(cdr $2))))
     ;;("lambda "{" arg-list "}" "{" stmt-list "}"
     ("format" expr-seq ($$ `(format . ,(cdr $2))))
     ("return" ($$ `(return)))
     ("return" unit-expr ($$ `(return ,$2)))
     ("incr" ident ($$ `(incr TODO)))
     ("incr" ident unit-expr ($$ `(incr TODO)))
     ("incr" ident/ix expr-list ($$ `(incr TODO)))
     ("incr" ident/ix expr-list unit-expr ($$ `(incr TODO)))
     )

    (if-stmt
     ("if" unit-expr "{" stmt-list "}"
      ($$ `(if ,$2 ,$4)))
     ("if" unit-expr "{" stmt-list "}" "else" "{" stmt-list "}"
      ($$ `(if ,$2 ,$4 (else ,$8))))
     ("if" unit-expr "{" stmt-list "}" elseif-list
      ($$ `(if ,$2 ,$4 ,@(sx-tail $6))))
     ("if" unit-expr "{" stmt-list "}" elseif-list "else" "{" stmt-list "}"
      ($$ `(if ,$2 ,$4 ,@(sx-tail $6) (else ,$9)))))
    (elseif-list
     (elseif-list-1 ($$ (tl->list $1))))
    (elseif-list-1
     ("elseif" unit-expr "{" stmt-list "}" 
      ($$ (make-tl 'elseif-list `(elseif ,$2 ,$4))))
     (elseif-list-1
      "elseif" unit-expr "{" stmt-list "}"
      ($$ (tl-append $1 'elseif-list `(elseif ,$2 ,$4)))))
    
    (unit-expr
     (primary-expression ($$ `(expr ,$1))))

    (expression
     (logical-or-expression))
    
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression ($$ `(or ,$1 ,$3))))
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression ($$ `(and ,$1 ,$3))))
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3))))
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3))))
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3))))
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3))))
    (relational-expression
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3))))
    (shift-expression
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3))))
    (additive-expression
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3))))
    (multiplicative-expression
     (unary-expression)
     (multiplicative-expression "*" unary-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" unary-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" unary-expression ($$ `(mod ,$1 ,$3))))
    (unary-expression
     (postfix-expression)
     ("-" unary-expression ($$ `(neg ,$2)))
     ("+" unary-expression ($$ `(pos ,$2)))
     ("!" unary-expression ($$ `(not ,$2)))
     ("~" unary-expression ($$ `(bitwise-not ,$2)))
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2))))
    (postfix-expression
     (primary-expression)
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1))))
    (primary-expression
     ("$" '$ident ($$ `(deref ,$2)))
     ("$" '$ident/ix "(" expr-list ")" ($$ `(deref-indexed ,$2 ,$4)))
     (fixed)
     (float)
     (string)
     (symbol)
     ;;($chlit ($$ `(char ,$1)))
     ("(" expr-list ")" ($$ $2))
     ;;("[" ident expr-seq "]" ($$ `(call $2 $3)))
     ("[" exec-stmt "]" ($$ `(eval ,$2)))
     )

    (expr-list
     (expression expr-list-tail ($$ (tl->list (tl-insert $2 $1))))
     (expression))
    (expr-list-tail
     ("," expression ($$ (make-tl 'expr-list $2)))
     (expr-list-tail "," expression ($$ (tl-append $1 $3))))

    (expr-seq
     (expr-seq-1 ($$ (tl->list $1))))
    (expr-seq-1
     ($empty ($$ (make-tl 'seq-list)))
     (expr-seq-1 primary-expression ($$ (tl-append $1 $2))))

    (ident ($ident ($$ `(ident ,$1))))
    (ident/ix ($ident/ix ($$ `(ident/ix ,$1))))
    (fixed ($fixed ($$ `(fixed ,$1))))
    (float ($float ($$ `(float ,$1))))
    (string ($string ($$ `(string ,$1))))
    (symbol ($symbol ($$ `(symbol ,$1))))
    (term (";") ("\n")))))

(define tsh-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine tsh-spec))
   #:keep 2
   #:keepers '($code-comm $lone-comm "\n")))

(define tsh-ia-spec
  (restart-spec tsh-mach 'item))

(define tsh-ia-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine tsh-ia-spec))
   #:keep 2
   #:keepers '($code-comm $lone-comm "\n")))

;;; =====================================

;; @defun {Scheme} gen-tsh-code-files [dir] => #t
;; Update or generate the files @quot{tsh-act.scm} and @quot{tsha-tab.scm}.
;; If there are no changes to existing files, no update occurs.
;; @end deffn
(define (gen-tsh-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))
  (write-lalr-actions tsh-mach (xtra-dir "tsh-act.scm.new")
		      #:prefix "tsh-")
  (write-lalr-tables tsh-mach (xtra-dir "tsh-tab.scm.new")
		     #:prefix "tsh-")
  (write-lalr-actions tsh-ia-mach (xtra-dir "tsh-ia-act.scm.new")
		      #:prefix "tsh-ia-")
  (write-lalr-tables tsh-ia-mach (xtra-dir "tsh-ia-tab.scm.new")
		     #:prefix "tsh-ia-")
  (let ((a (move-if-changed (xtra-dir "tsh-act.scm.new")
			    (xtra-dir "tsh-act.scm")))
	(b (move-if-changed (xtra-dir "tsh-tab.scm.new")
			    (xtra-dir "tsh-tab.scm")))
	(c (move-if-changed (xtra-dir "tsh-ia-act.scm.new")
			    (xtra-dir "tsh-ia-act.scm")))
	(d (move-if-changed (xtra-dir "tsh-ia-tab.scm.new")
			    (xtra-dir "tsh-ia-tab.scm"))))
    (or a b c d)))

;; --- last line ---
