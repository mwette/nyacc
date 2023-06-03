;;; nyacc/lang/tsh/compile-tree-il.scm - compile tclish sxml to tree-il

;; Copyright (C) 2021,2023 Matthew Wette
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

;; 1) Derived from tcl/compile-tree-il.scm.

;;; Todo:

;; 1) clean up fD handling of set
;; 2) find way to gen (define x 1) instead of (define x #unsp#) (set! x 1)

;;; Code:

(define-module (nyacc lang tsh compile-tree-il)
  #:export (compile-tree-il show-tsh-sxml show-tsh-xtil)
  #:use-module (nyacc lang tsh xlib)
  #:use-module (nyacc lang nx-lib)
  #:use-module (nyacc lang nx-util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts*-values))
  #:use-module ((srfi srfi-1) #:select (fold fold-right append-reverse))
  #:use-module (srfi srfi-88)           ; string->keyword
  #:use-module (language tree-il)
  #:use-module (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define (xlib-ref name)
  `(@@ (nyacc lang tsh xlib) ,name))

(define (op-call op kseed)
  (rev/repl 'call (xlib-ref op) kseed))


;; @deffn {Procedure} sxml->xtil exp env opts
;; Compile SXML tree to external Tree-IL representation.
;; @end deffn
			  
(define-public (sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (define +SP (make-+SP tree))

    (sx-match tree

      ;; optimizations
      ((last (expr-list ,unit))
       (values unit '() dict))
      ;; ---

      ((keychar ,sval)
       (values '() (+SP `(const ,(string->keyword sval))) dict))

      ((keyword ,sval)
       (values '() (+SP `(const ,(string->keyword sval))) dict))

      ((string ,sval)
       (values '() (+SP `(const ,sval)) dict))

      ((float ,sval)
       (values '() (+SP `(const ,(string->number sval))) dict))

      ((fixed ,sval)
       (values '() (+SP `(const ,(string->number sval))) dict))

      ((ident ,sval)
       (values '() (+SP `(const ,(string->symbol sval))) dict))

      ((eval . ,stmts)
       (values tree '() (nx-add-lexical "return" (nx-push-scope dict))))
      
      ((switch . ,stmts)
       (values tree '()
               (nx-add-lexicals "swx~val" (nx-push-scope dict))))

      ((for . ,stmts)
       (values tree '()
               (nx-add-lexicals "continue" "break" (nx-push-scope dict))))
      
      ((while . ,stmts)
       (values tree '()
               (nx-add-lexicals "continue" "break" (nx-push-scope dict))))
      
      ((proc (ident ,name) ,args ,body)
       (let ((form `(set (ident ,name) (lambda (@ (name ,name)) ,args ,body))))
         (fD (+SP form) '() dict)))

      ((lambda (arg-list . ,args) ,body)
       (let* ((arg-name (lambda (x) (cadadr x)))
              (dict (nx-push-scope dict))
	      (dict (nx-add-lexical "return" dict))
              (dict (fold               ; add args to local scope
                     (lambda (a d) (nx-add-lexical (arg-name a) d))
                     dict args))
	      (args (fold-right         ; ident -> lexical 
		     (lambda (a l)
                       (let ((ref (nx-lookup (arg-name a) dict)))
		         (cons (cons* (car a) ref (cddr a)) l)))
                     '() args))
              (form `(lambda (arg-list . ,args) ,body)))
         ;;(sferr "fD/lambda: dict, args:\n") (pperr dict) (pperr args)
	 (values (+SP form) '() (acons '@F "*anon*" dict))))

      ((incr (ident ,var) ,val)
       (values (+SP `(incr ,var ,val)) '() dict))
      ((incr (ident ,var))
       (values (+SP `(incr ,var (const 1))) '() dict))
      ((incr/ix (ident ,var) ,ix ,val)
       (values (+SP `(incr/ix ,var ,ix ,val)) '() dict))
      ((incr/ix (ident ,var) ,ix)
       (values (+SP `(incr/ix ,var ,ix (const 1))) '() dict))
      
      ((call (ident ,name) . ,args)
       (let ((ref (nx-lookup name dict)))
	 (unless ref (nx-error "not defined: ~S" name))
	 (values (+SP `(call ,ref . ,args)) '() dict)))

      ((set-indexed (ident ,name) ,index ,value)
       ;; FIXME: If name is not local then this will look up.
       ;; Should be an error instead.
       (let ((nref (nx-lookup name dict)))
	 (unless nref (nx-error "not defined: ~S" name))
	 (values (+SP `(set-indexed ,nref ,index ,value)) '() dict)))

      ((set (ident ,name) ,value)
       (let* ((dict (nx-ensure-variable/frame name dict))
              (nref (nx-lookup name dict)))
         ;;(sferr "fD/set: name=~S dict\n" name) (pperr dict)
	 (values (+SP `(set ,nref ,value)) '() dict)))

      ((nonlocal . ,names)
       (values '() '() (nx-insert-nonlocals dict names)))

      ((use . ,strpath)
       (let* ((sympath (map string->symbol strpath))
              (path (map (lambda (sym) `(const ,sym)) sympath))
              (parg `(primcall list ,@path))
              (stmt `(call (@@ (nyacc lang nx-lib) nx-use-module) ,parg))
              (dict (hash-fold
                     (lambda (key val dict) (nx-add-toplevel key dict))
                     dict (module-obarray (resolve-interface sympath)))))
         (values '() (+SP stmt) dict)))

      ((script . ,stmts)
       (values tree '() (nx-add-lexical "sreturn" (nx-push-scope dict))))

      ((@@ ,module ,symbol)             ; don't process resolved references
       (values '() tree dict))

      ((@ . _)                          ; don't process attributes
       (values '() tree dict))

      (,_
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (define +SP (make-+SP tree))
    (define pass-through '(toplevel
                           lexical
                           abort
                           arg-list arg opt-arg rest-arg
                           @@))
    
    (when #f
      ;;(sferr "fU: ~S\n" (if (pair? tree) (car tree) tree))
      ;;(sferr "    kseed=~S\n    seed=~S\n" kseed seed)
      (sferr "fU: ~S, tree, kseed, seed\n" (if (pair? tree) (car tree) tree))
      (pperr tree) (pperr kseed) (pperr seed)
      (sferr "\n")
      ;;(pperr tree)
      )
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (if
     (null? tree) (if (null? kseed)
		      (values seed kdict) 
		      (values (cons kseed seed) kdict))
     
     (case (car tree)

       ;; before leaving add a call to make sure all toplevels are defined
       ((*TOP*)
        (values
         (let loop ((form (if (null? (cdr kseed)) '(void) (car kseed)))
                    (dict kdict))
           (when (null? dict) (error "coding at TOP"))
           (if (eq? '@top (caar dict))
               form
               (loop `(seq (define ,(caddar dict) ,nx-undefined-xtil) ,form)
                     (cdr dict))))
         kdict))

       ((script)
        (let* ((ptag (nx-lookup "sreturn" kdict))
               (form (with-escape/arg ptag (block (rtail kseed)))))
	  (values (cons form seed) (nx-pop-scope kdict))))

       ((stmt-list)
        (let* ((stmtl (rtail kseed))
               (blk (block stmtl))
               (blk (+SP blk)))
	  (values (cons blk seed) kdict)))

       ((comment)
	(values seed kdict))

       ((lambda)
        ;;(sferr "fU/lambda: dict\n") (pperr kdict)
	(let* ((tail (rtail kseed))
               (attr (and (pair? (car tail)) (eq? '@ (caar tail)) (car tail)))
	       (argl (list-ref tail (if attr 1 0)))
	       (body (block (list-tail tail (if attr 2 1))))
	       (ptag (nx-lookup "return" kdict))
	       (arity (make-arity argl))
               (body (wrap-locals body kdict))
	       (body (with-escape/arg ptag body))
               (name (and attr (assq-ref (cdr attr) 'name)))
	       (form (make-function name 'nx-tsh arity body)))
	  (values (cons form seed) (nx-pop-scope kdict))))

       ((return)
	(let ((ret `(abort ,(nx-lookup "return" kdict)
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
        ;; no break
	(let* ((val (nx-lookup "swx~val" kdict))
	       (sw (if (eq? (caar kseed) 'default)
		       (make-switch val (cdr kseed) (car kseed))
		       (make-switch val kseed '(void)))))
	  (values (cons (+SP sw) seed) (nx-pop-scope kdict))))
       
       ((case)
	(let ((val (+SP (reverse kseed))))
	  (values 
	   (if (and (pair? seed) (eq? (caar seed) 'default))
	       (cons* (car seed) val (cdr seed)) ;; default first
	       (cons val seed))
	   kdict)))
       
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

       ((continue)
        (values
         (cons `(abort ,(nx-lookup "continue" kdict) () (const ())) seed)
         kdict))

       ((break)
        (values
         (cons `(abort ,(nx-lookup "break" kdict) '() (const ())) seed)
         kdict))
       
       ((set)
	(let* ((value (car kseed))
	       (nref (cadr kseed))
	       (form `(set! ,nref ,value)))
          ;;(sferr "fU/set:\n") (pperr kseed)
	  (values (cons (+SP form) seed) kdict)))

       ((set-indexed)
	(let* ((value (car kseed))
	       (indx (cadr kseed))
	       (nref (caddr kseed))
	       (val `(call ,(xlib-ref 'tsh:indexed-set!) ,nref ,indx ,value)))
	  (values (cons (+SP val) seed) kdict)))

       ((call)
	(values (cons (+SP `(call . ,(rtail kseed))) seed) kdict))

       ((eval)
	(let ((body (with-escape/arg (nx-lookup "return" kdict) (car kseed))))
 	  (values (cons (+SP body) seed) (nx-pop-scope kdict))))

       ((empty-stmt)
	(values seed kdict))

       ((incr)
	(let* ((tail (rtail kseed))
	       (name (car tail))
	       (expr (cadr tail))
	       (vref (nx-lookup name kdict))
	       (stmt `(set! ,vref (primcall + ,vref ,expr))))
	  (values (cons (+SP stmt) seed) kdict)))

       ((source)
	(let ((stmt `(call ,(xlib-ref 'tsh:source) ,(car kseed))))
	  (values (cons (+SP stmt) seed) kdict)))

       ((format)
	(let* ((tail (rtail kseed))
	       (stmt `(call ,(xlib-ref 'tsh:format) . ,tail)))
	  (values (cons (+SP stmt) seed) kdict)))

       ((expr-list)
        (values (cons (+SP `(primcall list ,@(rtail kseed))) seed) kdict))

       ((last)
        (values
         (cons (+SP `(call ,(xlib-ref 'last) . ,(rtail kseed))) seed)
         kdict))

       ((expr)
	;;(sferr "expr:~S\n" kseed)
	(values (cons (+SP (car kseed)) seed) kdict))

       ;; pos neg ~ not
       ((pos) (values (+SP (cons (op-call 'tsh:pos kseed) seed)) kdict))
       ((neg) (values (+SP (cons (op-call 'tsh:neg kseed) seed)) kdict))
       ((lognot) (values (+SP (cons (op-call 'tsh:lognot kseed) seed)) kdict))
       ((not) (values (+SP (cons (op-call 'tsh:not kseed) seed)) kdict))

       ;; mul div mod add sub
       ((mul) (values (+SP (cons (op-call 'tsh:* kseed) seed)) kdict))
       ((div) (values (+SP (cons (op-call 'tsh:/ kseed) seed)) kdict))
       ((mod) (values (+SP (cons (op-call 'tsh:% kseed) seed)) kdict))
       ((add) (values (+SP (cons (op-call 'tsh:+ kseed) seed)) kdict))
       ((sub) (values (+SP (cons (op-call 'tsh:- kseed) seed)) kdict))
       
       ;; lshift rshift rrshift
       ((lshift) (values (+SP (cons (op-call 'tsh:lshift kseed) seed)) kdict))
       ((rshift) (values (+SP (cons (op-call 'tsh:rshift kseed) seed)) kdict))

       ;; lt gt le ge
       ((eq) (values (+SP (cons (op-call 'tsh:eq kseed) seed)) kdict))
       ((ne) (values (+SP (cons (op-call 'tsh:ne kseed) seed)) kdict))
       ((lt) (values (+SP (cons (op-call 'tsh:lt kseed) seed)) kdict))
       ((gt) (values (+SP (cons (op-call 'tsh:gt kseed) seed)) kdict))
       ((le) (values (+SP (cons (op-call 'tsh:le kseed) seed)) kdict))
       ((ge) (values (+SP (cons (op-call 'tsh:ge kseed) seed)) kdict))

       ((deref)
        (let* ((name (car kseed))
               (ref (nx-lookup name kdict)))
	  (unless ref (nx-error "undefined variable: ~S" name))
          (values (+SP (cons ref seed)) kdict)))

       ((deref-indexed)
        (let* ((tail (rtail kseed))
               (name (car tail))
               (ref (nx-lookup name kdict))
               (args (cdr tail))
               (proc (xlib-ref 'tsh:indexed-ref)))
	  (unless ref (nx-error "undefined variable: ~A" name))
	  (values (+SP (cons `(call ,proc ,ref ,@args) seed)) kdict)))

       ((const)
        (values (+SP (cons (reverse kseed) seed)) kdict))

       (else
	(unless (member (car tree) pass-through)
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
	     (string-append "*** tsh: " fmt "\n") args)
      (values '(void) env))))

(define show-sxml #f)
(define (show-tsh-sxml v) (set! show-sxml v))
(define show-xtil #f)
(define (show-tsh-xtil v) (set! show-xtil v))
(define* (debug-tsh #:optional (arg #t))
  (set! show-sxml arg) (set! show-xtil arg))
(export debug-tsh)

(define (compile-tree-il exp env opts)
  (when show-sxml (sferr "sxml:\n") (pperr exp) (unless exp (quit)))
  ;; Need to make an interp.  All TCLish commands execute in an interp
  ;; so need (interp-lookup at turntime)
  (let ((cenv (if (module? env) (cons* `(@top . #t) `(@M . ,env) xdict) env)))
    (if exp 
	(call-with-values
	    (lambda () (sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    (when show-xtil
              (sferr "tree-il:\n") (pperr exp)
              (force-output (current-error-port))
              )
	    (values (parse-tree-il exp) env cenv)))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
