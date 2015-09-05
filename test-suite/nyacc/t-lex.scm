;; lexT.scm
;;
;; Copyright (C) 2015 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;; needs work -- probably does not work anymore

(use-modules (nyacc lex))

(define mt
  (list
   '("<=" . lteq)
   '(">=" . gteq)
   '("==" . eqeq)
   '("<>" . ltgt)
   '(".+" . dot-plus)
   '(".-" . dot-minus)
   '(".*" . dot-times)
   '("./" . dot-divide)
   '(".^" . dot-carat)
   '(":=" . colon-eq)
   '(class . class)
   ))


(define g (make-comm-reader '(("/*" . "*/") ("//" . "\n"))
			    #:pushback-newline #t))
#;(define lx (make-lexer mt))

(define (f g s)
  (simple-format #t "~S =>\n" s)
  (with-input-from-string s
    (lambda ()
      (simple-format #t "[~S]\n" (g (read-char)))
      (simple-format #t "ch=~S\n" (read-char))
      )))

(f g "//xxx\nabc")

;; ---
