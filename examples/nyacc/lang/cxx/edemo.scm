;; edemo.scm - error recovery
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;; Note that if we add $default transition to state after $error is
;; seen then we should have a simple parser.
;; 1:	stmts => stmts . '$error ";"
;; 	stmts => stmts . "1" ";"
;; 	stmts => stmts . ";"
;; 	$start => stmts .
;; 		'$end => accept 0
;;		'$error => shift 2
;;		"1" => shift 3
;; 		";" => shift 4
;;
;; 2:	stmts => stmts '$error . ";"
;;		";" => shift 5
;;		'$default => shift 2       <= ADDED
;;
;; I need to prove that this always works.

(use-modules (nyacc lalr))
(use-modules (nyacc lex))
(use-modules (nyacc parse))

(define edemo-spec
  (lalr-spec
   (start stmts)
   (grammar
    (stmts
     ($empty)
     (stmts ";")
     (stmts "1" ";")
     (stmts $error ";" ($$ (display "error: bad statement\n")))))))

(define retry-spec
  (lalr-spec
   (start stmts)
   (grammar
    (stmts
     ($empty)
     (stmts ";")
     (stmts "+" addend ";"))
    (addend
     $search ;; if foo fails back-track and retry
     (foo)
     (bar))
    )))

;; what to do:
;;  (save) (restore) (remove)
;; (lexr 'save) => (set! stk (cons '() stk))
;; (lexr 'restore) =>  ???
;; (lexr 'remove) => (set! stk (cdr stk))
;; (lexr) => ???
(define (wrap-lexer lexer)
  (lambda* (#:optional (flag 'lex))
    (case flag
      ((lex) lexer)
      ((save) (error "not done"))
      ((restore) (error "not done"))
      ((remove) (error "not done"))
      (else  (error "bad flag")))))

(use-modules (nyacc bison))
;;(define edemo-mach (make-lalr-machine/bison edemo-spec))
(define edemo-mach (make-lalr-machine edemo-spec))
(add-recovery-logic! edemo-mach)

(with-output-to-file "lang.txt"
  (lambda ()
    (pp-lalr-grammar edemo-mach)
    (pp-lalr-machine edemo-mach)))
(write-lalr-tables edemo-mach "edemotab.scm")
(write-lalr-actions edemo-mach "edemoact.scm")

(define p
  (let ((rp (make-lalr-parser edemo-mach))
	(lx (make-lexer-generator (lalr-match-table edemo-mach))))
    (lambda ()
      (rp (lx) #:debug #f))))

(with-input-from-string "2;" p)
#|
|#

;; --- last line ---
