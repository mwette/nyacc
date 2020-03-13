;;; nyacc/lang/mlang/pprint.scm

;; Copyright (C) 2016,2018 Matthew R. Wette
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

(define-module (nyacc lang mlang pprint)
  #:export (pretty-print-ml)
  #:use-module ((srfi srfi-1) #:select (pair-for-each fold-right))
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
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

  (define (string->mlang st)
    (if (string-any #\' st)
	(reverse-list->string
	 (string-fold
	  (lambda (ch seed)
	    (if (char=? #\' ch) (cons* #\' ch seed) (cons ch seed)))
	  '() st))
	st))

  (define (ppx/p tree) (sf "(") (ppx tree) (sf ")"))
  
  (define (ppx tree)
    (sx-match tree

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
       (for-each (lambda (stmt) (ppx stmt) (sf "\n")) stmts))

      ((call-stmt ,name . ,args)
       (ppx name) (sf "(")
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) ", "))
	args)
       (sf ");"))

      ((aref-or-call ,name . ,args)
       (ppx name) (sf "(")
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) ", "))
	args)
       (sf ")"))

      ((assn ,lhs ,rhs)
       (ppx lhs) (sf " = ") (ppx rhs) (sf ";"))

      ((multi-assign ,lvals ,name ,args)
       (sf "[") (for-each ppx lvals) (sf "] = ")
       (ppx name) (sf "(")
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) (sf ", ")))
	args)
       (sf ");\n"))

      ((for . ,rest)
       #f)

      ((while . ,rest)
       #f)

      ((if . ,rest)
       #f)

      ((switch . ,rest)
       #f)

      ((return ,value)
       (sf "return ") (ppx value) (sf ";\n"))

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
      ((neq ,lval ,rval) (binary 'neq " ~= " lval rval))

      ((add ,lval ,rval) (binary 'add " + " lval rval))
      ((sub ,lval ,rval) (binary 'sub " - " lval rval))
      ((mul ,lval ,rval) (binary 'mul "*" lval rval))
      ((div ,lval ,rval) (binary 'div "/" lval rval))
      ((ldiv ,lval ,rval) (binary 'ldiv "\\" lval rval))

      ((sel ,id ,ex) (binary 'sel "." ex id))

      ((fixed ,value) (sf "~A" value))
      ((float ,value) (sf "~A" value))
      ((string ,value) (sf "'~A'" (string->mlang value)))
      
      (,_ (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))))

  (ppx tree))


#;(use-modules (nyacc lang mlang parser))
#;(let ((sx (with-input-from-file "exam.d/ex02.m" parse-ml))
      )
  (pretty-print sx)
  ;;(simple-format #t "==>\n")
  ;;(pretty-print-ml sx)
  )

;; --- last line ---
