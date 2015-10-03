;;; examples/nyacc/util.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;;; or any later version published by the Free Software Foundation.  See the
;;; file COPYING included with the nyacc distribution.")

;; runtime utilities for the parsers -- needs work

(define-module (nyacc lang util)
  #:export (lang-crn-lic
	    make-tl tl->list
	    tl-append tl-insert tl-extend tl+attr
            fmterr)
  )

;; This is a generic copyright/licence that will be printed in the output
;; of the examples/nyacc/lang/*/ actions.scm and tables.scm files.
(define lang-crn-lic "Copyright (C) 2015 Matthew R. Wette

This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
or any later version published by the Free Software Foundation.  See the
file COPYING included with the this distribution.")


(define (fmterr fmt . args)
  (apply simple-format (current-error-port) fmt args))


;; @section Tagged Lists
;; Tagged lists are
;; They are implemented as a cons cell with the car and the cdr a list.
;; The cdr is used to accumulate appended items and the car is used to
;; keep the tag, attributes and inserted items.
;; @example
;; tl => '(H . T), H => (c a b 'tag); T =>
;; @end example

;; @table code

;; @item make-tl tag [item item ...]
;; Create a tagged-list structure.
(define (make-tl tag . rest)
  (let iter ((tail tag) (l rest))
    (if (null? l) (cons '() tail)
	(iter (cons (car l) tail) (cdr l)))))

;; @item tl->list tl
;; Convert a tagged list structure to a list.  This collects added attributes
;; and puts them right after the (leading) tag, resulting in something like
;; @example
;; (<tag> (@ <attr>) <rest>)
;; @end example
(define (tl->list tl)
  (let ((heda (car tl))
	(head (let iter ((head '()) (attr '()) (tl-head (car tl)))
		(if (null? tl-head)
		    (if (pair? attr)
			(cons (cons '@ attr) (reverse head))
			(reverse head))
		    (if (and (pair? (car tl-head)) (eq? '@ (caar tl-head)))
			(iter head (cons (cdar tl-head) attr) (cdr tl-head))
			(iter (cons (car tl-head) head) attr (cdr tl-head)))))))
    (let iter ((tail '()) (tl-tail (cdr tl)))
      (if (pair? tl-tail)
	  (iter (cons (car tl-tail) tail) (cdr tl-tail))
	  (cons tl-tail (append head tail))))))

;; @item tl-insert tl item
;; Insert item at front of tagged list (but after tag).
(define (tl-insert tl item)
  (cons (cons item (car tl)) (cdr tl)))

;; @item tl-append tl item ...
;; Append item at end of tagged list.
(define (tl-append tl . rest)
  (cons (car tl)
	(let iter ((tail (cdr tl)) (items rest))
	  (if (null? items) tail
	      (iter (cons (car items) tail) (cdr items))))))

;; @item tl-extend tl item-l
;; Extend with a list of items.  Like xxx
(define (tl-extend tl item-l)
  (apply tl-append tl item-l))

;; @item tl+attr tl key val)
;; Add an attribute to a tagged list.  Return the tl.
;; @example
;; (tl+attr tl 'type "int")
;; @end example
(define (tl+attr tl key val)
  (tl-insert tl (cons '@ (list key val))))

;; @item tl-merge tl tl1
;; Merge guts of phony-tl @code{tl1} into @code{tl}.
(define (tl-merge tl tl1)
  (error "not implemented (yet)")
  )

;; @table code


;;; --- last line
