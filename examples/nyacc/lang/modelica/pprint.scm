;;; nyacc/lang/modelica/pprint.scm

;; Copyright (C) 2018 Matthew R. Wette
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

(define-module (nyacc lang modelica pprint)
  #:export (pretty-print-mo)
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

(define* (pretty-print-mo tree #:key (indent-level 3))

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

  (define (string->modelica st)
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

      ((stored-definition . ,rest)
       #f)

      ((class . ,items)
       (for-each ppx items))

      (else (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))))

  (ppx tree))


;; --- last line ---
