;;; lang/c/util1.scm
;;;
;;; Copyright (C) 2015,2016 Matthew R. Wette
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

;; C parser utilities

(define-module (nyacc lang c99 util1)
  #:export (remove-inc-trees merge-inc-trees! elifify)
  #:use-module (nyacc lang util)
  #:use-module ((srfi srfi-1) #:select (append-reverse))
  #:use-module (srfi srfi-2) ;; and-let*
  #:use-module (sxml fold)
  #:use-module (sxml match)
)

;; @item remove-inc-trees tree
;; Remove the trees included with cpp-include statements.
;; @example
;; '(... (cpp-stmt (include "<foo.h>" (trans-unit ...))) ...)
;; => '(... (cpp-stmt (include "<foo.h>")) ...)
;; @end example
(define (remove-inc-trees tree)
  (if (not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
  (let iter ((rslt (make-tl 'trans-unit))
	     ;;(head '(trans-unit)) (tail (cdr tree))
	     (tree (cdr tree)))
    (cond
     ((null? tree) (tl->list rslt))
     ((and (eqv? 'cpp-stmt (car (car tree)))
	   (eqv? 'include (caadr (car tree))))
      (iter (tl-append rslt `(cpp-stmt (include ,(cadadr (car tree)))))
	    (cdr tree)))
     (else (iter (tl-append rslt (car tree)) (cdr tree))))))

;; @item merge-inc-trees tree
;; Remove the trees included with cpp-include statements.
;; @example
;; '(... (cpp-stmt (include "<foo.h>" (trans-unit (stmt ...))) ...)
;; => '(... (stmt...) ...)
;; @end example
#;(define (Xmerge-inc-trees tree)
  (if (not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
  (let iter ((rslt (make-tl 'trans-unit))
	     (tree (cdr tree)))
    (cond
     ((null? tree) (tl->list rslt))
     ((and (eqv? 'cpp-stmt (caar tree)) (eqv? 'include (cadar tree)))
      (iter (tl-extend rslt (cdr (merge-inc-trees (cdddar tree)))) (cdr tree)))
     (else (iter (tl-append rslt (car tree)) (cdr tree))))))


;; @item merge-inc-trees! tree => tree
;; This will (recursively) merge code from cpp-includes into the tree.
;; @example
;; (trans-unit
;;  (decl (a))
;;  (cpp-stmt (include "<hello.h>" (trans-unit (decl (b)))))
;;  (decl (c)))
;; =>
;; (trans-unit (decl (a)) (decl (b)) (decl (c)))
;; @end example
(define (merge-inc-trees! tree)

  ;; @item find-span (trans-unit a b c) => ((a . +->) . (c . '())
  (define (find-span tree)
    (cond
     ((not (pair? tree)) '())		; maybe parse failed
     ((not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
     ((null? (cdr tree)) (error "null c99-tree"))
     (else
      (let ((fp tree))			; first pair
	(let iter ((lp tree)		; last pair
		   (np (cdr tree)))	; next pair
	  (cond
	   ((null? np) (cons (cdr fp) lp))
	   ;; The following is an ugly hack to find cpp-include
	   ;; with trans-unit attached.
	   ((and-let* ((expr (car np))
		       ((eqv? 'cpp-stmt (car expr)))
		       ((eqv? 'include (caadr expr)))
		       (rest (cddadr expr))
		       ((pair? rest))
		       (span (find-span (car rest))))
		      (set-cdr! lp (car span))
		      (iter (cdr span) (cdr np))))
	   (else
	    (set-cdr! lp np)
	    (iter np (cdr np)))))))))

  ;; Use cons to generate a new reference:
  ;; (cons (car tree) (car (find-span tree)))
  ;; or not:
  (find-span tree)
  tree)


;; @deffn elifify tree => tree
;; This procedure will find patterns of
;; @example
;; (if cond-1 then-part-1
;;            (if cond-2 then-part-2
;;                       else-part-2
;; @end example
;; @noindent
;; and convert to
;; @example
;; (if cond-1 then-part-1
;;            (elif cond-2 then-part-2)
;;            else-part-2
;; @end example
(define (elifify tree)
  (define (fU tree)
    (sxml-match tree
      ((if ,x1 ,t1 (if ,x2 ,t2 (else-if ,x3 ,t3) . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) (else-if ,x3 ,t3) . ,rest))
      ((if ,x1 ,t1 (if ,x2 ,t2 . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) . ,rest))
      (,otherwise
       tree)))
  (foldt fU identity tree))
       
;; --- last line ---
