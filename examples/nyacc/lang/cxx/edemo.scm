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
;;		'$default => shift 2 <= added
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
     (stmts $error ";")))))

(use-modules (nyacc bison))
(define edemo-mach (make-lalr-machine/bison edemo-spec))

(with-output-to-file "lang.txt"
  (lambda ()
    (pp-lalr-grammar edemo-mach)
    (pp-lalr-machine edemo-mach)))
(write-lalr-tables edemo-mach "edemotab.scm")

(define p
  (let ((rp (make-lalr-parser edemo-mach))
	(lx (make-lexer-generator (lalr-match-table edemo-mach))))
    (lambda ()
      (rp (lx) #:debug #t))))

(with-input-from-string "2;" p)

;; --- last line ---
