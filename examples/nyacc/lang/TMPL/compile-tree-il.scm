;;; nyacc/lang/@x@/compile-tree-il.scm - compile @X@ sxml to tree-il

;; Copyright (C) 2023 Matthew Wette
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

;;; Notes:

;;; Todo:

;;; Code:

(define-module (nyacc lang @x@ compile-tree-il)
  #:export (compile-tree-il show-@x@-sxml show-@x@-xtil)
  #:use-module (nyacc lang @x@ xlib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang nx-lib)      ; nx-error
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold fold-right append-reverse))
  #:use-module (language tree-il)
  #:use-module (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr tree) (pretty-print tree (current-error-port)))

(define xlib-mod '(nyacc lang @x@ xlib))
(define xlib-module (resolve-module xlib-mod))
(define (xlib-ref name) `(@@ (nyacc lang @x@ xlib) ,name))

(define (lookup name dict)
  (or (nx-lookup name dict)
      (nx-lookup-in-env name xlib-module)))

(define (op-call op kseed)
  (rev/repl 'call (xlib-ref op) kseed))

(define (op-call/prim op kseed)
  (rev/repl 'primcall op kseed))

(define (make-function name arity body)
  (let* ((meta '((language . nx-@x@)))
	 (meta (if name (cons `(name . ,name) meta) meta)))
    `(lambda ,meta (lambda-case (,arity ,body)))))

;; @deffn {Procedure} sxml->xtil exp env opts
;; Compile SXML tree to external Tree-IL representation.
;; @end deffn
			  
(define-public (sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (define +SP (make-+SP tree))

    (sx-match tree

      ((string ,sval)
       (values '() (+SP `(const ,sval)) dict))

      ((float ,sval)
       (values '() (+SP `(const ,(string->number sval))) dict))

      ((fixed ,sval)
       (values '() (+SP `(const ,(string->number sval))) dict))

      ((switch . ,stmts)
       (values tree '()
               (nx-add-lexicals "swx~val" "break" (nx-push-scope dict))))

      ((for . ,stmts)
       (values tree '()
               (nx-add-lexicals "continue" "break" (nx-push-scope dict))))
      
      ((while . ,stmts)
       (values tree '()
               (nx-add-lexicals "continue" "break" (nx-push-scope dict))))
      
      ((function (ident ,name) (arg-list . ,args) ,body)
       (let* ((dict (nx-add-variable name dict))
	      (nref (lookup name dict))
	      (dict (nx-push-scope dict))
              ;; clean this up
	      (dict (fold (lambda (a d) (nx-add-lexical (cadadr a) d))
                          dict args))
	      (args (fold-right ;; replace arg-name with lexical-ref
		     (lambda (a l)
		       (cons (cons* (car a) (lookup (cadadr a) dict) (cddr a))
			     l)) '() args))
	      (dict (nx-add-lexical "return" dict))
	      (dict (acons '@F name dict))
              (proc `(proc ,nref (arg-list . ,args) ,body)))
	 (values (+SP proc) '() dict)))

      ((set (ident ,name) ,value)
       (let* ((dict (nx-ensure-variable name dict))
              (nref (lookup name dict)))
	 (values (+SP `(set ,nref ,value)) '() dict)))

      ((call (ident ,name) . ,args)
       (let ((ref (lookup name dict)))
	 (unless ref (nx-error "not defined: ~S" name))
	 (values (+SP `(call ,ref . ,args)) '() dict)))

      ;; don't process resolved references
      ((@@ ,module ,symbol)
       (values '() tree dict))

      (,_
       (values tree '() dict))))

  
  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (define +SP (make-+SP tree))

    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (nx-pop-scope kdict)
    (if
     (null? tree) (if (null? kseed)
		      (values seed kdict) 
		      (values (cons kseed seed) kdict))
     
     (case (car tree)

       ;; Before leaving add a call to make sure all toplevels are defined.
       ((*TOP*)
	(let ((tail (rtail kseed)))
	  (cond
	   ((null? tail) (values '(void) kdict)) ; just comments
	   (else (values (car kseed) kdict)))))

       ((function)
	(let* ((tail (rtail kseed))
	       (name-ref (list-ref tail 0))
	       (argl (list-ref tail 1))
	       (body (block (list-tail tail 2)))
	       (ptag (lookup "return" kdict))
	       (arity (make-arity argl))
	       ;; add locals : CLEAN THIS UP -- used in nx-octave also
	       (lvars (let loop ((ldict kdict))
			(if (eq? '@F (caar ldict)) '()
			    (cons (cdar ldict) (loop (cdr ldict))))))
               ;;(xxx (begin (sferr "loop\n") (pperr lvars) (quit)))
	       (body (let loop ((nl '()) (ll '()) (vl '()) (vs lvars))
                       ;; ^ this is wrap-bindings in javascript
		       (if (null? vs)
			   `(let ,nl ,ll ,vl ,body)
			   (loop
			    (cons (list-ref (car vs) 1) nl)
			    (cons (list-ref (car vs) 2) ll)
			    (cons '(void) vl)
			    (cdr vs)))))
	       ;;
	       (body (with-escape/arg ptag body))
	       (fctn (make-function (cadr name-ref) arity body))
	       (fctn (+SP fctn))
	       (stmt (if (eq? 'toplevel (car name-ref))
			 `(define ,(cadr name-ref) ,fctn)
			 `(set! ,name-ref ,fctn))) ;; never used methinks
	       )
	  ;;(sferr "proc ~S:\n" name-ref) (pperr tail) (pperr fctn)
	  (values (cons stmt seed) (nx-pop-scope kdict))))

       ((return)
	(let ((ret `(abort ,(lookup "return" kdict)
			   (,(if (> (length kseed) 1) (car kseed) '(void)))
			   (const ()))))
	  (values (cons (+SP ret) seed) kdict)))

       ;; conditional: elseif and else are translated by the default case
       ((if)
	(let* ((tail (rtail kseed))
	       (cond-expr `(primcall not (primcall zero? ,(list-ref tail 0))))
	       (then-expr (list-ref tail 1))
	       (rest-part (list-tail tail 2))
	       (rest-expr
		(let loop ((rest-part rest-part))
		  (match rest-part
		    ('() '(void))
		    (`((else ,body)) (block body))
		    (`((elseif ,cond-part ,body-part) . ,rest)
		     `(if (primcall not (primcall zero? ,cond-part))
			  ,body-part
			  ,(loop (cdr rest-part)))))))
	       (stmt (+SP `(if ,cond-expr ,then-expr ,rest-expr))))
	  (values (cons stmt seed) kdict)))
       ((elseif else)
	(values (cons (reverse kseed) seed) kdict))

       ((switch)
	(let* ((val (lookup "swx~val" kdict))
	       (sw (if (eq? (caar kseed) 'default)
		       (make-switch val (cdr kseed) (car kseed))
		       (make-switch val kseed '(void)))))
	  (values (cons (+SP sw) seed) (nx-pop-scope kdict))))
       
       ;; for allows continue and break
       ((for)
        (let* ((body (list-ref kseed 0))
               (next (list-ref kseed 1))
               (test `(primcall not (primcall zero? ,(list-ref kseed 2))))
               (init (list-ref kseed 3))
               (form (make-for init test next body kdict)))
	(values (cons (+SP form) seed) (nx-pop-scope kdict))))

       ((while)
	(let* ((test `(primcall not (primcall zero? ,(list-ref kseed 1))))
	       (body (list-ref kseed 0))
	       (form (make-while test body kdict)))
	  (values (cons (+SP form) seed) (nx-pop-scope kdict))))

       ((set)
	(let* ((value (car kseed))
	       (nref (cadr kseed))
	       (toplev? (eq? (car nref) 'toplevel))
	       (val (if toplev?
			`(define ,(cadr nref) ,value)
			`(set! ,nref ,value))))
	  (values (cons (+SP val) seed) kdict)))

       ((call)
	(values (cons (+SP `(call . ,(rtail kseed))) seed) kdict))

       ((empty-stmt)
	(values seed kdict))

       ;; ...
       ((add) (values (+SP (cons (op-call '@x@:+ kseed) seed)) kdict))
       ;; ...
       
       ((const)
        (values (+SP (cons (reverse kseed) seed)) kdict))

       (else
	(unless (member (car tree)
                        '(@@ toplevel lexical abort
                                      arg-list arg opt-arg rest-arg))
	  (sferr "MISSED: ~S\n" (car tree)))
	(cond
	 ((null? seed) (values (reverse kseed) kdict))
	 (else (values (cons (reverse kseed) seed) kdict)))))))


  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))


  (catch 'nx-error
    (lambda () (foldts*-values fD fU fH `(*TOP* ,exp) '() env))
    (lambda (key fmt . args)
      (apply simple-format (current-error-port)
	     (string-append "*** nx-@x@: " fmt "\n") args)
      (values '(void) env))))

(define show-sxml #f)
(define (show-@x@-sxml v) (set! show-sxml v))
(define show-xtil #f)
(define (show-@x@-xtil v) (set! show-xtil v))
(define* (debug-@x@ #:optional (arg #t))
  (set! show-sxml arg) (set! show-xtil arg))
(export debug-@x@)

(define (compile-tree-il exp env opts)
  (when show-sxml (sferr "sxml:\n") (pperr exp) (unless exp (quit)))
  (let ((cenv (if (module? env) (cons* `(@top . #t) `(@M . ,env) xdict) env)))
    (if exp 
	(call-with-values
	    (lambda () (sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    (when show-xtil (sferr "tree-il:\n") (pperr exp))
	    (values (parse-tree-il exp) env cenv)))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
