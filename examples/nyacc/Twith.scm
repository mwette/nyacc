;; examples/nyacc/Twith.scm
;;
;; Copyright (C) 2015 Matthew R. Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (getcwd))
(add-to-load-path (string-append (getcwd) "/../../module"))

;; This is an example use of $with/$prune syntax.
;; It is currently not working.

(use-modules (nyacc lalr))
(use-modules (ice-9 pretty-print))

(define js-spec
  (lalr-spec
   (start Statement)
   (grammar
    (Statement
     ("{" StatementList "}")
     (($with Expression ($prune ObjectLiteral)))
     ;;(Expression)
     )
    (StatementList
     (Statement)
     (StatementList Statement))
    (Expression
     (AssignmentExpression)
     (ObjectLiteral))
    (AssignmentExpression ('$ident "=" Expression))
    (ObjectLiteral ("{" ExpressionList "}"))
    (ExpressionList
     (Expression)
     (ExpressionList Expression))
    )))

(unless js-spec (error "broken spec"))

(define js-mach (make-lalr-machine js-spec))

(with-output-to-file ",with.txt"
  (lambda ()
    (pp-lalr-grammar js-spec)
    (pp-lalr-machine js-mach)))

;;; --- last line    
