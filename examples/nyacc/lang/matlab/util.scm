;;; nyacc/lang/matlab/util.scm - matlab processing code
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

;; utilities for processing output trees

(define-module (nyacc lang matlab util)
  #:export (
	    apply-ml-sem ;; apply static semantics
	    typeify-tree
	    )
  #:use-module (nyacc lang util)
  #:use-module (ice-9 pretty-print)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module (sxml match)
  )

;; probably also need some sort of overall declaration form
;; need an example

;; need to remove aref-or-call
;; only way is to know if ident is variable or function
;; local variables
;; @item
;; global variables
;; @item
;; function arguments
;; @item
;; look for str2func
;; @item
;; look in dict for function
;; @item
;; if function argument is function then use will always be w/ ftn ref (@@).
;; @item
;; varargs
(define (apply-ml-sem tree . rest)
  (let* ((ml-dict (if (pair? rest) (car rest) '()))
	 )
    tree))


;; track vardict - type-usage, a list of:
;;   'float 'fixed 'float-a 'fixed-a 'struct 'aorf

(define (fout fmt . args)
  (apply simple-format #t fmt args))


;; dict = ((name . (type rank)) ...)
;; OR     ((name type . rank) ...)
(define (d-add-type dict name type)
  (let ((resp (assoc-ref dict name)))
    (cond
     ((not resp) (acons name type #f))
     ((eqv? (cadr resp) type) dict)
     (else (acons name (cons type (cddr resp)) dict)))))
 
(define (d-add-rank dict name rank)
  (let ((resp (assoc-ref dict name)))
    (cond
     ((not resp) (acons name #f rank))
     ((eqv? (cddr resp) rank) dict)
     (else (acons name (cons rank (cddr resp)) dict)))))

;; @deffn typeify-tree tree [dict] => tree
;; This needs work.
;; The idea is to end up with declarations for a matlab function-file.
;; The filename function should be public, all others private.
(define (typeify-tree tree . rest)
  
  (define (fD tree seed dict) ;; => (values tree seed dict)
    (sxml-match tree
      #;((function-file (@ (file ,name)) . ,rest)
       (values tree '() xxx))
      
      ((assn (array-ref (ident ,name) ,expr) ,rval)
       (values tree '() (d-add-rank dict name 'x)))
      (,otherwise
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => (values seed dict)
    ;;(fout "tree-tag=~S kseed=~S\n" (car tree) kseed)
    (case (car tree)
      ((float fixed)
       (values
	(cons (list (car tree) '(@ (rank "0")) (car kseed)) seed)
	dict))
      #;((array-ref)
       #t)
      (else
       (values
	(if (null? seed) (reverse kseed) ; w/o this top node is list
	    (cons (reverse kseed) seed))
	dict))))

  (define (fH tree seed dict) ;; => (values seed dict)
    (values (cons tree seed) dict))

  (let*
      ((ml-dict (if (pair? rest) (car rest) '()))
       (ty-dict '())
       (sx (foldts*-values fD fU fH tree '() ty-dict))
       )
    sx))

;; --- last line ---
