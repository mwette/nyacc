;;; lang/c/util.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
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

(define-module (nyacc lang c99 util)
  #:export (remove-inc-trees merge-inc-trees!)
  #:use-module (nyacc lang util)
  #:use-module ((srfi srfi-1) #:select (append-reverse))
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

  (define (find-span tree)
    ;; (trans-unit a b c) -> ((a . +->) . (c . '())
    (if (not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
    (if (null? (cdr tree)) (error "null c99-tree"))
    (let ((fp tree))			; first pair
      (let iter ((lp tree)		; last pair
		 (np (cdr tree)))	; next pair
	(cond
	 ((null? np) (cons (cdr fp) lp))
	 ((and (eqv? 'cpp-stmt (car (car np)))
	       (eqv? 'include (caadr (car np))))
	  (let* ((t-u (list-ref (cadar np) 2))
		 (span (find-span t-u)))
	    (set-cdr! lp (car span))
	    (iter (cdr span) (cdr np))))
	 (else
	  (set-cdr! lp np)
	  (iter np (cdr np)))))))

  ;; Use cons to generate a new reference:
  ;; (cons (car tree) (car (find-span tree)))
  ;; or not:
  (find-span tree)
  tree)

       
;; --- last line ---
