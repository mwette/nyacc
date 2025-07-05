;;; nyacc/lang/c99/parse.scm

;; Copyright (C) 2014-2021,2025 Matthew Wette
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

;;; Description:

;; Derived from nyacc/parse.scm, this has added hack to convert typename
;; token to $ident if it works.

;;; Code:

(define-module (nyacc lang c99 parse)
  #:export (make-lalr-parser/c99))

(define $default 1)			; sync w/ lalr.scm
(define $error 2)			; sync w/ lalr.scm

(define vector-map (@@ (nyacc parse) vector-map))
(define wrap-action (@@ (nyacc parse) wrap-action))
(define make-xct (@@ (nyacc parse) make-xct))
(define dmsg/n (@@ (nyacc parse) dmsg/n))
(define dmsg/s (@@ (nyacc parse) dmsg/s))
(define parse-error (@@ (nyacc parse) parse-error))

;; Compare to nyacc/parser.scm(make-lalr-parser/num).
(define* (make-lalr-parser/c99 mach #:key (skip-if-unexp '()))
  (let* ((interactive #f) (env '())
         (len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v) env))
	 (ntab (acons 1 '$default (assq-ref mach 'ntab)))
         (mtab (assq-ref mach 'mtab))         ; NEW
         (typename (assq-ref mtab 'typename)) ; NEW
         (ident (assq-ref mtab '$ident))      ; NEW
	 (start (assq-ref (assq-ref mach 'mtab) '$start)))
    (lambda* (lexr #:key debug)
      (let loop ((state (list 0))	; state stack
		 (stack (list '$@))	; semantic value stack
		 (nval #f)		; non-terminal from prev reduction
		 (lval #f))		; lexical value (from lex'r)
	(cond
	 ((and (zero? (car state))
               interactive nval
	       (eqv? (car nval) start)) ; done
	  (cdr nval))
	 ((not (or nval lval))
	  (if (eqv? $default (caar (vector-ref pat-v (car state))))
	      (loop state stack (cons-source stack $default #f) lval)
	      (loop state stack nval (lexr)))) ; reload
	 (else
	  (let* ((laval (or nval lval))
		 (tval (car laval))
		 (sval (cdr laval))
		 (stxl (vector-ref pat-v (car state)))
		 (stx (or (assq-ref stxl tval)
                          (and (eq? tval typename) (assq-ref stxl ident)) ; NEW
			  (and (not (memq tval skip-if-unexp))
			       (assq-ref stxl $default))
			  #f)))		; error
	    (if debug (dmsg/n (car state) (if nval tval sval) stx ntab))
	    (cond
	     ((eq? #f stx)		; error
	      (if (memq tval skip-if-unexp)
		  (loop state stack #f #f)
		  (parse-error state laval)))
	     ((negative? stx)		; reduce
	      (let* ((gx (abs stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xct-v gx) stack))
		     (pobj (if (zero? gl) laval (list-tail stack (1- gl))))
		     (pval (source-properties pobj))
		     (tval (cons-source pobj (vector-ref rto-v gx) $$)))
		(if (supports-source-properties? $$)
		    (set-source-properties! $$ pval))
		(loop (list-tail state gl) (list-tail stack gl) tval lval)))
	     ((positive? stx)		; shift
	      (loop (cons stx state) (cons-source laval sval stack) 
		    #f (if nval lval #f)))
	     (else			; accept
	      (car stack))))))))))

;;; --- last line ---
