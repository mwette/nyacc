;;; nyacc/lang/matlab/pprint.scm
;;;
;;; Copyright (C) 2016 Matthew R. Wette
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

(define-module (nyacc lang matlab pprint)
  #:export (pretty-print-ml)
  #:use-module ((srfi srfi-1) #:select (pair-for-each fold-right))
  #:use-module (nyacc lang util)
  #:use-module (sxml match)
  #:use-module (ice-9 pretty-print)
  )

;; TODO;; this is C
(define op-sym 
  (let ((ot '(("=" . eq) ("+=" . pl-eq) ("-=" . mi-eq) ("*=" . ti-eq)
	      ("/=" . di-eq) ("%=" . mo-eq) ("<<=" . ls-eq) (">>=" . rs-eq)
	      ("&=" . ba-eq) ("^=" . bx-eq) ("|=" bo-eq))))
    (lambda (name)
      (assoc-ref ot name))))

;; TODO; this is C
(define op-prec
  ;; in order of decreasing precedence
  '((p-expr ident fixed float string)
    (comp-lit post-inc post-dec sel fctn-call array-ref)
    (de-ref ref-to neg pos not bitwise-not sizeof pre-inc pre-dec)
    (cast)
    (mul div ldiv mod)
    (add sub)
    (lshift rshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    (cond-expr)
    (assn-expr)
    (comma)))

;; TODO
(define op-assc ;; this is C
  '((left array-ref sel post-inc post-dec comp-lit mul div ldiv mod add sub
	  lshift rshift lt gt le ge bitwise-and bitwise-xor bitwise-or and or)
    (right pre-inc pre-dec sizeof bitwise-not not pos neg ref-to de-ref cast
	   cond assn-expr)
    (nonassoc)))

(define protect-expr? (make-protect-expr op-prec op-assc))

(define* (pretty-print-ml tree #:key (indent-level 3))

  (define fmtr (make-pp-formatter))
  (define (push-il) (fmtr 'push))
  (define (pop-il) (fmtr 'pop))
  
  (define sf (lambda args (apply fmtr args)))
  
  (define (unary/l op rep rval)
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))
  
  (define (unary/r op rep lval)
    (sf rep)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval)))
  
  (define (binary op rep lval rval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))

  (define (string->matlab st)
    (if (string-any #\' st)
	(list->string
	 (reverse
	  (string-fold
	   (lambda (ch seed)
	     (if (char=? #\' ch) (cons* #\' ch seed) (cons ch seed)))
	   st)))
	st))

  (define (ppx/p tree) (sf "(") (ppx tree) (sf ")"))
  
  (define (ppx tree)
    (sxml-match tree

      ((script-file . ,rest)
       #f)

      ((function-file . ,items)
       (for-each ppx items))

      ((fctn-defn (fctn-decl (ident ,name) ,iputs ,oputs ,coml) ,stmt-list)
       (sf "function [")
       (ppx oputs)
       (sf "] = ~A(" name)
       (ppx iputs) (sf ")\n")
       (for-each ppx (sx-tail coml 1))
       (push-il)
       (ppx stmt-list)
       (pop-il))

      ((comm ,text)
       (sf "%~A\n" text))

      ((ident-list . ,rest)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	rest))

      ((ident ,ident)
       (sf "~A" ident))

      ((stmt-list . ,stmts)
       (for-each
	(lambda (stmt)
	  (ppx stmt)
	  (sf "\n")
	  )
	stmts))

      ((assn ,lhs ,rhs)
       (ppx lhs) (sf " = ") (ppx rhs) (sf ";"))

      ((command (ident ,name) (arg-list . ,args))
       (sf "~A" name)
       (for-each (lambda (arg) (sf " ~A" (sx-ref arg 1))) args))

      ((matrix . ,rows)
       (sf "[")
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf "; "))
	  ;; newline?
	  )
	rows)
       (sf "]"))

      ((row . ,expr-list)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	expr-list))

      ((xpose ,expr) (ppx expr) (sf "'"))
      
      ((array-ref ,ident ,expr-list)
       (ppx ident) (sf "(") (ppx expr-list) (sf ")"))

      ((expr-list . ,items)
       (fold-right			; seed is "need comma"
	(lambda (item seed)
	  (if seed (sf ", "))
	  (cond
	   ((eqv? 'colon-expr (sx-tag item)) (sf ":") #f)
	   (else (ppx item) #t)))
	#f
	items))
      
      ((pos ,expr) (unary/l 'pos "+" expr))
      ((neg ,expr) (unary/l 'neg "-" expr))

      ((lt ,lval ,rval) (binary 'lt " < " lval rval))
      ((gt ,lval ,rval) (binary 'gt " > " lval rval))
      ((le ,lval ,rval) (binary 'le " <= " lval rval))
      ((ge ,lval ,rval) (binary 'ge " >= " lval rval))
      ((eq ,lval ,rval) (binary 'eq " == " lval rval))
      ((neq ,lval ,rval) (binary 'neq " != " lval rval))

      ((add ,lval ,rval) (binary 'add " + " lval rval))
      ((sub ,lval ,rval) (binary 'sub " - " lval rval))
      ((mul ,lval ,rval) (binary 'mul "*" lval rval))
      ((div ,lval ,rval) (binary 'div "/" lval rval))
      ((ldiv ,lval ,rval) (binary 'ldiv "\\" lval rval))

      ((sel ,id ,ex) (binary 'sel "." ex id))

      ((fixed ,value) (sf "~A" value))
      ((float ,value) (sf "~A" value))
      ((string ,value) (sf "'~A'" (string->matlab value)))
      
      #|
      ((p-expr ,expr) (ppx expr))
      ((char ,value) (sf "'~A'" (sx-ref tree 1)))

      ((scope ,expr) (sf "(") (ppx expr) (sf ")"))
      
      ((bitwise-not ,expr) (unary/l 'bitwise-not "~" expr))
      ((not ,expr) (unary/l 'not "!" expr))

      ((cast ,tn ,ex)
      (sf "(") (ppx tn) (sf ")")
      (if (protect-expr? 'rt 'cast ex)
      (ppx/p ex)
      (ppx ex)))

      ((mod ,lval ,rval) (binary 'mod "%" lval rval))
      
      ((post-inc ,expr) (unary/r 'post-inc "++" expr))
      ((post-dec ,expr) (unary/r 'post-dec "--" expr))
      
      ((array-of ,dir-declr ,arg)
      (ppx dir-declr) (sf "[") (ppx arg) (sf "]"))
      ((array-of ,dir-declr)
      (ppx dir-declr) (sf "[]"))
      ;; MORE TO GO
      
      ;; selection-statement
      ((if . ,rest)
      (let ((cond-part (sx-ref tree 1))
      (then-part (sx-ref tree 2)))
      (sf "if (") (ppx cond-part) (sf ") ")
      (ppx then-part)
      (let iter ((else-l (sx-tail tree 3)))
      (cond
      ((null? else-l) #t)
      ((eqv? 'else-if (caar else-l))
      (sf "else if (") (ppx (sx-ref (car else-l) 1)) (sf ") ")
      (ppx (sx-ref (car else-l) 2))
      (iter (cdr else-l)))
      (else
      (sf "else ")
      (ppx (car else-l)))))))

      ((switch ,expr (compd-stmt (block-item-list . ,items)))
      (sf "switch (") (ppx expr) (sf ") {\n")
      (for-each
      (lambda (item)
      (unless (memq (car item) '(case default)) (push-il))
      (ppx item)
      (unless (memq (car item) '(case default)) (pop-il)))
      items)
      (sf "}\n"))

      ;; labeled-statement
      ((case ,expr ,stmt)
      (sf "case ") (ppx expr) (sf ":\n")
      (push-il) (ppx stmt) (pop-il))

      ((default ,stmt)
      (sf "default:\n")
      (push-il) (ppx stmt) (pop-il))

      ;; This does not meet the convention of "} while" on same line. 
      ((do-while ,stmt ,expr)
      (sf "do ")
      (if (eqv? 'compd-stmt (sx-tag stmt)) 
      (ppx (cons 'compd-stmt-no-newline (cdr stmt)))
      (ppx stmt))
      (sf "while (") (ppx expr) (sf ");\n"))
      
      ;; for
      ((for (decl . ,rest) ,test ,iter ,stmt)
      (sf "for (") (ppx `(decl-no-newline . ,rest))
      (sf " ") (ppx test) (sf "; ") (ppx iter) (sf ") ")
      (ppx stmt))

      ((for (decl . ,rest) ,expr2 ,expr3 ,stmt)
      (sf "for (")
      (ppx `(decl . ,rest)) (sf " ") (ppx expr2) (sf "; ") (ppx expr3)
      (sf ") ") (ppx stmt))
      ((for ,expr1 ,expr2 ,expr3 ,stmt)
      (sf "for (")
      (ppx expr1) (sf "; ") (ppx expr2) (sf "; ") (ppx expr3)
      (sf ") ") (ppx stmt))

      ;; jump-statement
      ((goto ,where)
      (pop-il)			; unindent
      (sf "goto ~A;" (sx-ref where 1))
      ;; comment?
      (sf "\n")
      (push-il))			; re-indent
      |#

      (,otherwise
       (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))
      ))

  (ppx tree))


#;(use-modules (nyacc lang matlab parser))
#;(let ((sx (with-input-from-file "exam.d/ex02.m" parse-ml))
      )
  (pretty-print sx)
  ;;(simple-format #t "==>\n")
  ;;(pretty-print-ml sx)
  )

;; --- last line ---
