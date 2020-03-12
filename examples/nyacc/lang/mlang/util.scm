;;; nyacc/lang/mlang/util.scm - mlang processing code

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

;; utilities for processing output trees

(define-module (nyacc lang mlang util)
  #:export (
	    apply-ml-sem ;; apply static semantics
	    declify-ffile declify-script
	    name-expr->decl
	    )
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module (sxml match)
  )

;; probably also need some sort of overall declaration form
;; need an example

;; need to remove aref-or-call
;; only way is to know if ident is variable or function
;; only way ident is a variable is if ...
;; @item exists as lhs assn
;; @item exists in "global"
;; @item function arguments
;; For function
;; @item look for str2func
;; @item look in dict for function
;; @item
;; if function argument is function then use will always be w/ ftn ref (@@).
;; @item
;; varargs
(define (apply-ml-sem tree . rest)
  (let* ((ml-dict (if (pair? rest) (car rest) '()))
	 )
    tree))

;; decl
;; decl: fctn, struct, array, double, int
#;(define (name-expr->c-decl name expr) ;; => decl
  (sx-match expr
    ((aref-or-call (ident "zeros") ,ex-l)
     #f)))

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

(define (d-push dict)
  (list (cons '@P dict)))

(define (d-pop dict)
  (assq-ref dict '@P))

;; @edeffn lval->ident lval [disp] => string
;; Given an lval return the root identifier name as a string.
;; @var{disp} is the disposition (e.g., struct) of the lval
(define* (lval->ident lval #:optional (disp 'unknown))
  (sx-match lval
    ((ident ,name) (cons name disp))
    ((sel ,ident ,lval) (lval->ident lval 'struct))
    ((array-ref ,lval ,ex-l)
     (case disp
       ((struct) (lval->ident lval disp))
       (else (lval->ident lval 'array))))
    ((aref-or-call ,lval ,ex-l)
     (case disp
       ((struct) (lval->ident lval disp))
       (else (lval->ident lval 'array))))
    (,_ (throw 'util-error "unknown lval: ~S" lval))))

(define (binary-rank lval rval)
  (and lval rval (max lval rval)))

;; @deffn expr->rank expr => #f,0,1,..
;; Return rank of expression, if it can be determined.
(define* (expr->rank expr #:optional (dict '()))
  (case (sx-tag expr)
    ((ident) (and=> (assoc-ref dict (sx-ref expr 1)) cdr))
    ((sel) (expr->rank (sx-ref expr 2) dict))
    ((array-ref) (length (sx-tail (sx-ref expr 2) 1)))
    ((add sub mul div) (binary-rank (sx-ref expr 1) (sx-ref expr 2)))
    (else
     (throw 'util-error "unknown expr: ~S" expr))))
    
;; dictionary
;; (name . (fixed rank)) if type == float fixed
;; (name . (float rank)) if type == float fixed
;; (name . (var rank)) if type == float fixed
;; (name . (fctn pub?))

;; given a list of stmts, look in leading statements for
;; (comm "%: <name> : <type> ")
;; so osig = function doit(con, isig)
;;   %~ osig : double
;;   %~ con : struct
;;   %~ isig : double
;; should be (for now) struct double vector matrix

(define fold-in-decl
  (let ((rx1 (make-regexp "^~\\s*([0-9A-Za-z]+)\\s*:\\s*([0-9A-Za-z]+)")))
    (lambda (str seed)
      (let ((m (regexp-exec rx1 str)))
	(if m
	    (acons (match:substring m 1) (match:substring m 2) seed)
	    seed)))))

;; matrix: oset rstr cstr flag double *
;; vector: double *
	     
(define (parse-type-decls stmts)
  (let loop ((out '()) (stmts stmts))
    (cond
     ((null? stmts) out)
     ((not (eqv? 'comm (caar stmts))) out)
     (else
      (loop (fold-in-decl (cadar stmts) out) (cdr stmts))))))

;; @deffn declify-ffile tree [dict] => tree
;; This needs work.
;; The idea is to end up with declarations for a mlang function-file.
;; The filename function should be public, all others private.
(define (declify-ffile tree . rest)

  ;; TODO: get c decl from file comment
  ;; %#include "ex03b.h"
  ;; Then pass through c99 parser OR Maybe not

  (define (fD tree seed dict) ;; => (values tree seed dict)
    (sx-match tree
      ((function-file (@ (file ,name)) . ,rest)
       (values tree '()
	       (cons*
		(cons name (cons 'fctn #t))
		(cons "file" name)
		dict)))

      ((fctn-defn (fctn-decl . ,decl) (stmt-list . ,stmts))
       (let ((tdecls (parse-type-decls stmts))
	     )
	 (values tree '() dict)))

      ((fctn-decl (ident ,name) . ,rest)
       (let ((scope (if (equal? name (assoc-ref dict "file")) "pub" "prv"))
	     )
	 (values (sx-attr-add* tree 'scope scope) '() dict)))

      ((assn (aref-or-call ,expr ,ex-l) . ,rval)
       (values `(assn (array-ref ,expr ,ex-l)) '() dict))
      
      ((assn ,lval ,rval)
       ;;(fout "lval->ident=>~S\n" (lval->ident lval))
       ;;(values tree '() (d-add-rank dict name (length (sx-tail lval 1)))))
       (values tree '() dict))

      (,_ (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => (values seed dict)
    ;;(when (pair? tree) (simple-format #t "cartree=~S\n" (car tree)))
    (if
     (null? tree) (values (cons kseed seed) dict)
     
     (case (car tree)
       ((function-file) (values (reverse kseed) dict))

       ((float fixed)
	(values
	 (cons (sx-set-attr! (reverse kseed) 'rank "0") seed)
	 dict))
       
       #;((fctn-decl)
       (values
       (cons (reverse kseed) seed)
       (d-pop kdict)))
       
       (else
	(values (cons (reverse kseed) seed) dict)))))

  (define (fH tree seed dict) ;; => (values seed dict)
    (values (cons tree seed) dict))

  (let*
      ((ml-dict (if (pair? rest) (car rest) '()))
       (ty-dict '())
       (sx (foldts*-values fD fU fH tree '() ty-dict))
       )
    sx))


;; @deffn declify-script tree [dict] => tree
;; This needs work.
;; The idea is to end up with declarations for a mlang function-file.
;; The filename function should be public, all others private.
(define (declify-script tree . rest)
  
  (define (fD tree seed dict) ;; => (values tree seed dict)
    (sxml-match tree
      ((script-file (@ . ,attr) . ,rest)
       (values tree '()
	       (cons*
		(cons "file" (assq-ref attr "file"))
		dict)))

      ((assn (aref-or-call ,expr ,ex-l) . ,rval)
       (values `(assn (array-ref ,expr ,ex-l)) '() dict))
      
      #;((assn (ident ,name) (aref-or-call (ident "struct") ,ex-l))
       (let ((kvl (let loop ((kvl '()) (al (sx-tail ex-l 1)))
		    (if (null? al) (reverse kvl)
			(loop (cons (list (cadar al) (cadr al))
				    kvl) (cddr al)))))
	     )
	 (fout "struct ~S: ~S\n" name (map car kvl))
	 ;;(pretty-print kvl)
	 (values tree '() dict)))
       
      ((assn ,lval ,rval)
       ;;(fout "lval->ident=>~S\n" (lval->ident lval))
       ;;(fout "    ->rank =>~S\n" (expr->rank lval))
       (values tree '() dict))

      (,_ (values tree '() dict))))
  
  (define (fU tree seed dict kseed kdict) ;; => (values seed dict)
    ;;(fout "tree-tag=~S kseed=~S\n" (car tree) kseed)
    (case (car tree)
      ((script-file) (values (reverse kseed) dict))
      ((assn)
       (let* ((assn (reverse kseed))
	      (lval (sx-ref assn 1)) (rval (sx-ref assn 2))
	      (ltyp (sx-attr-ref lval 'type))
	      (rtype (sx-attr-ref rval 'type))
	      (rrank (and=> (sx-attr-ref rval 'rank) string->number))
	      )
	 ;;(fout "rval=~S type=~S rank=~S\n" rval rtype rrank)
	 (values (cons (reverse kseed) seed) dict)))
      ((add sub mul div ldiv mod)
       (let ()
	 (values
	  (cons (reverse kseed) seed)
	  dict)))
      ((fixed)
       (values
	(cons (sx-set-attr* (reverse kseed) 'type "fixed" 'rank "0") seed)
	dict))
      ((float)
       (values
	(cons (sx-set-attr* (reverse kseed) 'type "float" 'rank "0") seed)
	dict))
      (else
       (values (cons (reverse kseed) seed) dict))))

  (define (fH tree seed dict) ;; => (values seed dict)
    (values (cons tree seed) dict))

  (let*
      ((ml-dict (if (pair? rest) (car rest) '()))
       (ty-dict '())
       (sx (foldts*-values fD fU fH tree '() ty-dict))
       )
    sx))

;; --- last line ---
