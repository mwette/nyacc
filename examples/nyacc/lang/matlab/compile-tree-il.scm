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

(define xlib-module (resolve-module '(nyacc lang matlab xlib)))
		       
(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

;; @deffn {Procedure} js-sxml->tree-il/ext exp env opts
;; Compile javascript SXML tree to external tree-il representation.
;; This one is public because it's needed for debugging the compiler.
;; @end deffn
(define (js-sxml->tree-il/ext exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (sx-match tree

      (`(ident ,name)
       (let ((ref (lookup name dict)))
	 (cond
	  (ref
	   (values '() ref dict))
	  (else
	   (sferr "matlab: maybe undefined: ~A\n" name)
	   (values '() `(toplevel ,(string->symbol name)) dict)))))

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
     (null? tree) (values (cons kseed seed) dict)
     
     (case (car tree)
       ((*TOP*)
	(values (car kseed) kdict))

       (else
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  ;; We generate a dictionary with the env (module?) available at the top.
  #;(let ((dict (acons '@top #t (acons '@M env JSdict)))
	(sexp `(*TOP* ,exp)))
    (call-with-values
	(lambda () (foldts*-values fD fU fH sexp '() dict))
      (lambda (seed dict) seed)))
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
  (let ((cenv env))
    (if exp 
	(call-with-values
	    (lambda ()
		;;(ml-sxml->tree-il/ext exp cenv opts)
		(values #f cenv)
		)
	  (lambda (exp cenv)
	    (sferr "tree-il:\n") (pperr exp)
	    ;;(values (parse-tree-il exp) env cenv)
	    (values (parse-tree-il '(const "[compile-tree-il skip]")) env cenv)
     	    ))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
