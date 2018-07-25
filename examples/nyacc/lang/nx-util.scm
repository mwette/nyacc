;;; nyacc/lang/nx-util.scm - utilities for Guile extension languages

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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

(define-module (nyacc lang nx-util)
  #:export (genxsym
	    nx-push-scope nx-pop-scope nx-top-level?
	    nx-add-toplevel nx-add-lexical nx-add-lexicals nx-add-symbol
	    nx-lookup-in-env nx-lookup
	    )
  )

(define (genxsym name) (gensym (string-append name "-")))

;; push/pop scope level
(define (nx-push-scope dict)
  (list (cons '@P dict)))
(define (nx-pop-scope dict)
  (or (assq-ref dict '@P) (error "coding error: too many pops")))
(define (nx-top-level? dict)
  (assoc-ref dict '@top))

;; Add toplevel to dict, return dict
(define (nx-add-toplevel name dict)
  (acons name `(toplevel ,(string->symbol name)) dict))

;; Add lexical to dict, return dict
(define (nx-add-lexical name dict)
  (acons name `(lexical ,(string->symbol name) ,(genxsym name)) dict))

;; (add-lexicals name1 name2 ... dict) 
(define (nx-add-lexicals . args)
  (let iter ((args args))
    (if (null? (cddr args)) (nx-add-lexical (car args) (cadr args))
	(nx-add-lexical (car args) (iter (cdr args))))))

;; Add lexical or toplevel based on level.
(define (nx-add-symbol name dict)
  (if (nx-top-level? dict)
      (nx-add-toplevel name dict)
      (nx-add-lexical name dict)))

(define (nx-lookup-in-env name env)
  (let ((sym (string->symbol name)))
    (if (module-variable env sym)
	`(@@ ,(module-name env) ,sym)
	#f)))

;; @deffn {Procedure} x_y->x-y a_string => a-string
;; Convert a C-like name to a Scheme-like name.
;; @end deffn
(define (x_y->x-y name)
  (string-map (lambda (ch) (if (char=? ch #\_) #\- ch)) name))

(define (nx-lookup name dict)
    (cond
   ((not dict) #f)
   ((null? dict) #f)
   ((assoc-ref dict name))		; => value
   ((assoc-ref dict '@P) =>		; parent level
    (lambda (dict) (nx-lookup name dict)))
   ((nx-lookup-in-env name (assoc-ref dict '@M)))
   ((nx-lookup-in-env (x_y->x-y name) (assoc-ref dict '@M)))
   (else #f)))

;; --- last line ---
