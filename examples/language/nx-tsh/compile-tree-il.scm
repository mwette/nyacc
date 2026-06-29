;;; nyacc/lang/tsh/compile-tree-il.scm - compile tclish sxml to tree-il

;; Copyright (C) 2021,2023,2026 Matthew Wette
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

(define-module (language nx-tsh compile-tree-il)
  #:export (compile-tree-il show-tsh-sxml show-tsh-xtil)
  #:use-module (language nx-tsh xlib)
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
  `(@@ (language nx-tsh xlib) ,name))

(define (op-call op args)
  `(call ,(xlib-ref op) ,@args))


;; @deffn {Procedure} sxml->xtil exp env opts
;; Compile SXML tree to external Tree-IL representation.
;; @end deffn

(define-public (sxml->xtil exp env opts)

  (define (fD tree seed dict) ;; => tree seed dict
    (define +SP (make-+SP tree))

    ;;(sferr "fD tree=:\n") (pperr tree)
    (sx-match tree

      ;; optimizations
      ((last (expr-list ,unit))
       (values unit '() dict))
      ;;

      ((eval . ,_)
       (values tree '() (nx-add-var/scope "return" (nx-push-scope dict))))

      ((switch . ,_)
       (values tree '() (nx-add-var/scope "swx~val" (nx-push-scope dict))))

      ((for . ,_)
       (values tree '() (fold nx-add-var/scope (nx-push-scope dict)
                              '("continue" "break"))))

      ((while . ,_)
       (values tree '() (fold nx-add-var/scope (nx-push-scope dict)
                              '("continue" "break"))))

      ((proc (ident ,name) ,args ,body)
       (let ((form `(set (ident ,name) (lambda (@ (name ,name)) ,args ,body))))
         (fD (+SP form) '() dict)))

      ((lambda (@ . ,attrd) (arg-list . ,args) ,body)
       ;; we convert to arg-list understood by nx-util's make-arity
       (let* ((dict (nx-push-scope dict))
	      (dict (nx-add-var/scope "return" dict))
              (args (map cadr args))
              (dict (fold (lambda (a d) (nx-add-var/scope a d)) dict args))
              (args (fold-right
                     (lambda (a l) (cons `(arg ,(nx-lookup dict a)) l))
                     '() args))
              (form (+SP `(lambda (arg-list . ,args) ,body)))
              (name (or (and=> (assq-ref attrd 'name) car) "*anon*")))
	 (values form '() (nx-add-tag dict '@F name))))

      ((incr (ident ,var) ,val)
       (values (+SP `(incr ,var ,val)) '() dict))
      ((incr (ident ,var))
       (values (+SP `(incr ,var (const 1))) '() dict))
      ((incr/ix (ident ,var) ,ix ,val)
       (values (+SP `(incr/ix ,var ,ix ,val)) '() dict))
      ((incr/ix (ident ,var) ,ix)
       (values (+SP `(incr/ix ,var ,ix (const 1))) '() dict))

      ((call (ident ,name) . ,args)
       (let ((ref (nx-lookup dict name)))
	 (unless ref (nx-error "not defined: ~A" name))
	 (values (+SP `(call ,ref . ,args)) '() dict)))

      ((set-indexed (ident ,name) ,index ,value)
       ;; FIXME: If name is not local then this will look up.
       ;; Should be an error instead.
       (let ((nref (nx-lookup dict name)))
	 (unless nref (nx-error "not defined: ~A" name))
	 (values (+SP `(set-indexed ,nref ,index ,value)) '() dict)))

      ((set (ident ,name) ,value)
       ;;(sferr "dict:\n") (pperr dict)
       (let* ((dict (or (nx-ensure/taglev name '@F dict)
                        (nx-ensure/global name dict)))
              (nref (nx-lookup dict name)))
         ;;(sferr "fD/set: name=~S nref=~S\n" name nref) (pperr dict)
         ;;(if (string=? name "y") (quit))
	 (values (+SP `(set ,nref ,value)) '() dict)))

      ((nonlocal . ,names)
       ;;(values '() '() (nx-insert-nonlocals dict names)))
       (values '() '() (fold nx-add-var/global dict names)))

      ;; since tsh has no lexical scope besides procedures, this works:
      ((global . ,names)
       (values '() '() (fold nx-add-var/global dict names)))

      ((use . ,strpath)
       (let* ((sympath (map string->symbol strpath))
              (path (map (lambda (sym) `(const ,sym)) sympath))
              (parg `(primcall list ,@path))
              (stmt `(call (@@ (nyacc lang nx-lib) nx-use-module) ,parg))
              (dict (hash-fold
                     (lambda (key val dict) (nx-add-var/global key dict))
                     dict (module-obarray (resolve-interface sympath)))))
         (values '() (+SP (reverse stmt)) dict)))

      ((script . ,stmts)
       (values tree '() (nx-add-var/scope "sreturn" (nx-push-scope dict))))

      ((@@ ,module ,symbol)             ; don't process resolved references
       (values '() (reverse tree) dict))

      ((@ . ,_)                          ; don't process attributes
       (values '() (reverse tree) dict))

      (,_
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    ;; This routine rolls up processes leaves into the current branch.
    ;; We have to be careful about returning kdict vs dict.
    ;; Approach: always return kdict or (pop-scope kdict)
    (define +SP (make-+SP tree))
    (define (cons/src head tail)
      (set-source-properties! head (source-properties tree))
      (cons head tail))
    (define pass-through
      '(@@ toplevel lexical set! const abort
                    elseif else arg-list arg opt-arg rest-arg))
    
    (let ((form (reverse kseed)))
      ;;(sferr "fU: tree,form:\n") (pperr tree) (pperr form)
      (match form
        ('() (values seed kdict))

        ;; before leaving add a call to make sure all toplevels are defined
        (`(*TOP* . ,_)
        (values
         (let loop ((form (if (null? (cdr kseed)) '(void) (car kseed)))
                    (dict kdict))
           (when (null? dict) (error "coding at TOP"))
           (if (eq? '@top (caar dict))
               form
               (loop `(seq (define ,(caddar dict) ,nx-unspecified-xtil) ,form)
                     (cdr dict))))
         kdict))

        (`(script . ,_)
         (let* ((ptag (nx-lookup kdict "sreturn"))
                (form (with-escape/arg ptag (block (rtail kseed)))))
           ;;(sferr "sreturn: ptag=~s, form:" ptag) (pperr form)
	   (values (cons form seed) (nx-pop-scope kdict))))

        (`(stmt-list . ,_)
         (let* ((stmtl (rtail kseed))
                (blk (block stmtl))
                (blk (+SP blk)))
	   (values (cons blk seed) kdict)))

        (`(comment . ,_)
	 (values seed kdict))

        (`(lambda ,argl ,body)
	 (let* ((body (block body))
	        (ptag (nx-lookup kdict "return"))
	        (arity (make-arity argl))
                (body (wrap-locals body kdict))
	        (body (with-escape/arg ptag body))
                (name (or (and=> (assoc-ref kdict '@F) string->symbol) 'unknown))
	        (form (make-function name 'nx-tsh arity body)))
	   (values (cons form seed) (nx-pop-scope kdict))))

       (`(return . ,_)
	(let ((ret `(abort ,(nx-lookup kdict "return")
			   (,(if (> (length kseed) 1) (car kseed) '(void)))
			   (const ()))))
	  (values (cons (+SP ret) seed) kdict)))

       ;; conditional: elseif and else are translated by the default case
       (`(if . ,_)
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
	       (stmt `(if ,cond-expr ,then-expr ,rest-expr)))
	  (values (cons/src stmt seed) kdict)))

       (`(switch . ,_)
        ;; no break
	(let* ((val (nx-lookup kdict "swx~val"))
	       (sw (if (eq? (caar kseed) 'default)
		       (make-switch val (cdr kseed) (car kseed))
		       (make-switch val kseed '(void)))))
	  (values (cons (+SP sw) seed) (nx-pop-scope kdict))))

       (`(case . ,_)
	(let ((val (+SP (reverse kseed))))
	  (values
	   (if (and (pair? seed) (eq? (caar seed) 'default))
	       (cons* (car seed) val (cdr seed)) ;; default first
	       (cons val seed))
	   kdict)))

       ;; for allows continue and break
       (`(for . ,_)
        (let* ((body (list-ref kseed 0))
               (next (list-ref kseed 1))
               (test `(primcall not (primcall zero? ,(list-ref kseed 2))))
               (init (list-ref kseed 3))
               (form (make-for init test next body kdict)))
	  (values (cons/src form seed) (nx-pop-scope kdict))))

       (`(while . ,_)
	(let* ((test `(primcall not (primcall zero? ,(list-ref kseed 1))))
	       (body (list-ref kseed 0))
	       (form (make-while test body kdict)))
	  (values (cons/src form seed) (nx-pop-scope kdict))))

       (`(continue . ,_)
        (values
         (cons `(abort ,(nx-lookup kdict "continue") () (const ())) seed)
         kdict))

       (`(break . ,_)
        (values
         (cons `(abort ,(nx-lookup kdict "break") '() (const ())) seed)
         kdict))

       (`(set . ,_)
	(let* ((value (car kseed))
	       (nref (cadr kseed))
	       (form `(set! ,nref ,value)))
          ;;(sferr "fU/set:\n") (pperr kseed)
	  (values (cons/src form seed) kdict)))

       (`(set-indexed . ,_)
	(let* ((value (car kseed))
	       (indx (cadr kseed))
	       (nref (caddr kseed))
	       (val `(call ,(xlib-ref 'tsh:indexed-set!) ,nref ,indx ,value)))
	  (values (cons/src val seed) kdict)))

       (`(call ,proc . ,args)
        ;;(sferr "call ~s\n" proc)
        ;; TODO: check for ftn value: if not lambda assume want puts 
        #;(let ((proc (car (rtail kseed))))
          (pperr (rtail kseed))
          (and=> (match proc (`(const ,name) (nx-lookup dict name)) (_ #f))
            (lambda (val) (sferr "call ~s\n" val))))
	(values (cons (+SP `(call ,proc ,@args)) seed) kdict))

       (`(eval . ,_)
	(let ((body (with-escape/arg (nx-lookup kdict "return") (car kseed))))
 	  (values (cons (+SP body) seed) (nx-pop-scope kdict))))

       (`(empty-stmt . ,_)
	(values seed kdict))

       (`(incr ,name ,expr)
	(let* ((vref (nx-lookup kdict name))
	       (stmt `(set! ,vref (primcall + ,vref ,expr))))
	  (values (cons (+SP stmt) seed) kdict)))

       (`(source . ,_)
	(let ((stmt `(call ,(xlib-ref 'tsh:source) ,(car kseed))))
	  (values (cons (+SP stmt) seed) kdict)))

       (`(format . ,args)
        ;; This could be made more efficient for literal format strings
        ;; using the parse-format-string procedure from nx-printf module.
	(let* ((tail (rtail kseed))
	       (stmt `(call ,(xlib-ref 'tsh:format) . ,args)))
	  (values (cons (+SP stmt) seed) kdict)))

       (`(expr-list . ,expl)
        (values (cons (+SP `(primcall list ,@expl)) seed) kdict))

       (`(last . ,_)
        (values (cons (+SP `(begin . ,(rtail kseed))) seed) kdict))

       (`(expr . ,_)
	;;(sferr "expr:~S\n" kseed)
	(values (cons (+SP (car kseed)) seed) kdict))

       ;; pos neg ~ not
       (`(pos . ,args) (values (cons/src (op-call 'tsh:pos args) seed) kdict))
       (`(neg . ,args) (values (cons/src (op-call 'tsh:neg args) seed) kdict))
       (`(lognot . ,args)
        (values (cons/src (op-call 'tsh:lognot args) seed) kdict))
       (`(not . ,args) (values (cons/src (op-call 'tsh:not args) seed) kdict))

       ;; mul div mod add sub
       (`(mul . ,args) (values (cons/src (op-call 'tsh:* args) seed) kdict))
       (`(div . ,args) (values (cons/src (op-call 'tsh:/ args) seed) kdict))
       (`(mod . ,args) (values (cons/src (op-call 'tsh:% args) seed) kdict))
       (`(add . ,args) (values (cons/src (op-call 'tsh:+ args) seed) kdict))
       (`(sub . ,args) (values (cons/src (op-call 'tsh:- args) seed) kdict))

       ;; lshift rshift rrshift
       (`(lshift . ,args)
        (values (cons/src (op-call 'tsh:lshift args) seed) kdict))
       (`(rshift . ,args)
        (values (cons/src (op-call 'tsh:rshift args) seed) kdict))

       ;; lt gt le ge
       (`(eq . ,args) (values (cons/src (op-call 'tsh:eq args) seed) kdict))
       (`(ne . ,args) (values (cons/src (op-call 'tsh:ne args) seed) kdict))
       (`(lt . ,args) (values (cons/src (op-call 'tsh:lt args) seed) kdict))
       (`(gt . ,args) (values (cons/src (op-call 'tsh:gt args) seed) kdict))
       (`(le . ,args) (values (cons/src (op-call 'tsh:le args) seed) kdict))
       (`(ge . ,args) (values (cons/src (op-call 'tsh:ge args) seed) kdict))

       (`(deref ,name)
        (let* ((ref (or (nx-lookup kdict name)
                        `(@@ (guile-user) ,(string->symbol name)))))
          ;;(sferr "fU.deref: name=~s => ~s\n" name ref)
	  (unless ref (nx-error "undefined variable: ~A" name))
          (values (+SP (cons ref seed)) kdict)))

       (`(deref-indexed ,name ,expl)
        ;;(sferr "deref-indexed name=~s => ~s\n" name (nx-lookup kdict name))
        ;;(pperr kdict)
        ;;(quit)
        (let* ((ref (or (nx-lookup kdict name)
                        `(@@ (guile-user) ,(string->symbol name))))
               (proc (xlib-ref 'tsh:indexed-ref)))
	  (unless ref (nx-error "undefined variable: ~A" name))
	  (values (+SP (cons `(call ,proc ,ref ,expl) seed)) kdict)))

       (`(deref-indexed-expr ,expr)
        ;; The issue here is that the result should be a symbol but no
        ;; symbol table at run-time (i.e., need dynamic scoping)
        (sferr "WORK TO GO deref-indexed-expr"))
        
       #;(`(const . ,_)
        (values (+SP (cons form seed)) kdict))

       (`(keychar ,sval)
        (values (cons/src `(const ,(string->keyword sval)) seed) kdict))

       (`(keyword ,sval)
        (values (cons/src `(const ,(string->keyword sval)) seed) kdict))

       (`(string ,sval)
        (values (cons/src `(const ,sval) seed) kdict))

       (`(float ,sval)
        (values (cons/src `(const ,(string->number sval)) seed) kdict))

       (`(fixed ,sval)
        (values (cons/src `(const ,(string->number sval)) seed) kdict))

       (`(ident ,sval)
        (values (cons/src `(const ,(string->symbol sval)) seed) kdict))

       (_
	(unless (member (car form) pass-through)
	  (sferr "MISSED: ~S\n" (car tree)) (pperr form))
	(values (cons/src form seed) kdict)))))

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
  (let ((cenv (cons* `(@top . #t) `(@env . ,env) xdict)))
    ;;(pperr cenv)
    (if exp
	(call-with-values
	    (lambda () (sxml->xtil exp cenv opts))
	  (lambda (exp cenv)
	    (when show-xtil (sferr "tree-il:\n") (pperr exp))
	    (values (parse-tree-il exp) env cenv)))
	(values (parse-tree-il '(void)) env cenv))))

;; --- last line ---
