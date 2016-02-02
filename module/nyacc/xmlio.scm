;; work in progress
;; use bison (xml format) to generate a nyacc automaton
;;

;; (bison-
;;   (grammar (rules ...) (terminals ...) (nonterminals ...))
;;   (automaton (state ...)

;;                 (rule (@ (usefulness "useful") (number "37"))
;;                        (lhs "non_comment_statement")
;;                        (rhs (symbol "command")
;;                             (symbol "ident_nc_list")
;;                             (symbol "term")))
;;                  (rule (@ (usefulness "useful") (number "38"))
;;                        (lhs "lval_expr_list")
;;                        (rhs (symbol "lval_expr")))

(use-modules (sxml simple))
(use-modules (sxml match))
(use-modules (ice-9 pretty-print))

(let* ((s0 (call-with-input-file "gram.xml"
             (lambda (p) (xml->sxml p #:trim-whitespace? #t))))
       )
  (pretty-print s0)
  )
