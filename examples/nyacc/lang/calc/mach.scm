;; mach.scm - calculator
;;
;; Copyright (C) 2019 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;; Notes:

;; This is the same grammar as in the calc.scm file, but instead of
;; immediate execution, the parse tables are generated.

;;; Code:

(define-module (nyacc lang calc mach)
  #:export (gen-calc-files
            calc-file-spec calc-file-mach 
            calc-user-spec calc-user-mach)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse))

(define calc-file-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start prog)
   (grammar
    (prog
     (stmt-list ($$ (tl->list $1))))
    (stmt-list
     (stmt ($$ (make-tl 'stmt-list $1)))
     (stmt-list stmt ($$ (tl-append $1 $2))))
    (stmt
     ("\n" ($$ `(empty-stmt)))
     (expr "\n" ($$ `(expr-stmt ,$1)))
     (assn "\n" ($$ `(assn-stmt ,$1))))
    (expr
     (expr "+" expr ($$ `(add ,$1 ,$3)))
     (expr "-" expr ($$ `(sub ,$1 ,$3)))
     (expr "*" expr ($$ `(mul ,$1 ,$3)))
     (expr "/" expr ($$ `(div ,$1 ,$3)))
     ($fixed ($$ `(num ,$1)))
     ($float ($$ `(num ,$1)))
     ($ident ($$ `(ident ,$1)))
     ("(" expr ")" ($$ $2)))
    (assn
     ($ident "=" expr ($$ `(assn (ident ,$1) ,$3)))))))


;; Build an automaton for the full language (i.e., list of statements).
;; This is hashed, so tokens are represented by integers.
(define calc-file-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine calc-file-spec))))

;; Build an automaton for expressions to be used as Guile language.
;; Guile wants to see one statement at a time, so replace 'prog' start
;; with 'stmt' start.
(define calc-user-spec
  (restart-spec calc-file-spec 'stmt))

;; For purpose of demo, do not hashify the interactive one.
;; But the machine must have compacted tables!
(define calc-user-mach
  (compact-machine
    (make-lalr-machine calc-user-spec)))

;; Procedure to generate actions and tables.
(define (gen-calc-files)
  (write-lalr-actions 
   calc-file-mach "mach.d/calc-file-act.scm" #:prefix "calc-file-")
  (write-lalr-tables 
   calc-file-mach "mach.d/calc-file-tab.scm" #:prefix "calc-file-")
  (write-lalr-actions 
   calc-user-mach "mach.d/calc-user-act.scm" #:prefix "calc-user-")
  (write-lalr-tables 
   calc-user-mach "mach.d/calc-user-tab.scm" #:prefix "calc-user-")
  )

;; --- last line ---
