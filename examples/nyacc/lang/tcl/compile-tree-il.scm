;;; nyacc/lang/tcl/compile-tree-il.scm - compile tcl sxml to tree-il

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

;;; Description:

;; limitations:
;; 1) variables cannot be introduced by lhs expression:
;;    i.e., a = 1 is OK, but a(1) = 1 is not

;;; Code:

(define-module (nyacc lang tcl compile-tree-il)
  #:export (compile-tree-il)
  #:use-module (nyacc lang tcl xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold append-reverse))
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  ;;#:use-module (system base compile)
  )
(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define xlib-mod '(nyacc lang tcl xlib))
(define xlib-module (resolve-module xlib-mod))
(define (xlib-ref name) `(@@ (nyacc lang tcl xlib) ,name))

;; scope must be manipulated at execution time
;; the @code{proc} command should push-scope
(define push-scope nx-push-scope)
(define pop-scope nx-pop-scope)
(define top-level? nx-top-level?)
(define add-toplevel nx-add-toplevel)
(define add-lexical nx-add-lexical)
(define add-lexicals nx-add-lexicals)
(define add-symbol nx-add-symbol)
(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

(define make-opcall (opcall-generator xlib-mod))
					   
;; @deffn {Procedure} sxml->xtil exp env opts
;; Compile SXML tree to external Tree-IL representation.
;; @end deffn
(define-public (sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (when #f
      (sferr "fD: ~S\n" tree)
      )
    (sx-match tree

      ((string ,sval)
       (values '() `(const ,sval) dict))

      ((deref ,name)
       (let ((ref (lookup name dict)))
	 (cond
	  (ref
	   (values '() ref dict))
	  (else
	   (let ((dict (add-symbol name dict)))
	     (values '() (lookup name dict) dict))))))

      ((deref ,name ,index)
       (error "indexing not done"))
      
      (else
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (when #f
      (sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      (sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      ;;(pperr tree)
      )
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (if
     (null? tree) (values (cons kseed seed) kdict)
     
     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
	(values (car kseed) kdict))

       ((command)
	(sferr "COMMAND NOT IMPLEMNENTED\n")
	(values (cons (reverse kseed) seed) kdict))

       ((expr)
	;;(sferr "expr: ~S\n" (reverse kseed))
	(values
	 (cons `(call ,(xlib-ref 'tcl:expr) . ,(rtail kseed)) seed)
	 kdict))

       (else
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  (foldts*-values fD fU fH `(*TOP* ,exp) '() env)
  )

;; @deffn {Procedure} compile-tree-il exp env opts => exp env cenv
;; On input @var{exp} is the SXML from our reader, @var{env} is ``an
;; environment'', and @var{opts} is a keyword list of options.  The procedure
;; return three values: the compiled expressin, the corresponding environment
;; for the target for the compiled language, and a continuation environment
;; for the next parsed syntax tree.
;; @end deffn
(define (compile-tree-il exp env opts)
  (sferr "sxml:\n") (pperr exp)
  ;; Need to make an interp.  All Tcl commands execute in an interp
  ;; so need (interp-lookup at turntime)
  (let ((cenv (if (module? env) (cons* `(@top . #t) `(@M . ,env) xdict) env)))
    (if exp 
	(call-with-values
	    (lambda ()
	      (sxml->xtil exp cenv opts)
	      ;;(values #f cenv)
	      )
	  (lambda (exp cenv)
	    (sferr "tree-il:\n") (pperr exp)
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[hello]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
