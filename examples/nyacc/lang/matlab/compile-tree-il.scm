;;; compile matlab sxml to tree-il

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

;; limitations:
;; 1) variables cannot be introduced by lhs expression:
;;    i.e., a = 1 is OK, but a(1) = 1 is not

(define-module (nyacc lang matlab compile-tree-il)
  #:export (compile-tree-il)
  #:use-module (nyacc lang matlab xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  ;;#:use-module (nyacc lang util)
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

(define xlib-mod '(nyacc lang matlab xlib))
(define xlib-module (resolve-module xlib-mod))
		       
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

;; @deffn {Procedure} ensure-toplevel name
;; Generate a TIL expression that will ensure the toplevel name is defined.
;; If a define needs to be issues the value is @code{(void)}.
;; @end deffn
(define (make-toplevel-defcheck name)
  (let ((var (genxsym "var")) (sym (string->symbol name)))
    `(let (var) (,var)
	  ((call (toplevel module-local-variable)
		 (call (toplevel current-module))
		 (const ,sym)))
	  (if (lexical var ,var) (void) (define ,sym (void))))))

;; Add toplevel def's from dict before evaluating expression.  This puts
;; @var{expr} at the end of a chain of @code{seq}'s that execution
;; conditional defines to a void.  See @code{make-toplevel-defcheck}.
(define (add-topdefs dict expr)
  (let iter ((refs dict))
    (cond
     ((null? refs) expr)
     ((string? (caar refs))
      `(seq ,(make-toplevel-defcheck (caar refs))
	    ,(iter (cdr refs))))
     (else (iter (cdr refs))))))

(define make-opcall (opcall-generator xlib-mod))
					   
;; @deffn {Procedure} js-sxml->tree-il/ext exp env opts
;; Compile javascript SXML tree to external tree-il representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define (xlang-sxml->tree-il/ext exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (sx-match tree

      ((ident ,name)
       (let ((ref (lookup name dict)))
	 (cond
	  (ref
	   (values '() ref dict))
	  (else
	   (let ((dict (add-symbol name dict)))
	     (values '() (lookup name dict) dict))))))

      ((fixed ,sval)
       (values '() `(const ,(string->number sval)) dict))

      ((float ,sval)
       (values '() `(const ,(string->number sval)) dict))

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
	(values (add-topdefs kdict (car kseed)) kdict))

       ;; For functions, need to check kdict for lexicals and add them.
       ;; set *TOP* for toplevel versoin of same.  (gen add-lexdefs
       ;; ((function)

       ;; Statements
       ((empty-statement)
	(values (cons '(void) kseed) kdict))

       ((call-stmt) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((assn)
	(let ((tail (rtail kseed)))
	  (values (cons `(set! ,(car tail) ,(cadr tail)) kseed) kdict)))

       ((multi-assn) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((for) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((while) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((if) ;; TODO
	(values (cons '(void) kseed) kdict))
       
       ((switch) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((case-list) ;; TODO
	(values (cons '(void) kseed) kdict))
       ((case) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((return) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((command) ;; TODO
	(values (cons '(void) kseed) kdict))


       ((lval-expr-list) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((colon-expr) ;; TODO
	(values (cons '(void) kseed) kdict))

       ((add) (make-opcall 'ml:+ seed kseed kdict))
       ((sub) (make-opcall 'ml:- seed kseed kdict))
       ((mul) (make-opcall 'ml:* seed kseed kdict))
       ((div) (make-opcall 'ml:/ seed kseed kdict))

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
;; for the next javascript tree.
;; @end deffn
(define (compile-tree-il exp env opts)
  (sferr "sxml:\n") (pperr exp)
  (let ((cenv (if (module? env) (cons* `(@top . #t) `(@M . ,env) xdict) env)))
    (if exp 
	(call-with-values
	    (lambda () (xlang-sxml->tree-il/ext exp cenv opts))
	  (lambda (exp cenv)
	    (sferr "tree-il:\n") (pperr exp)
	    (values (parse-tree-il exp) env cenv)
	    ;;(values (parse-tree-il '(const "[compile-tree-il skip]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
