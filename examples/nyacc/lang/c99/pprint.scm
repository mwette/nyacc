;; nyacc/lang/c99/pprint.scm
;;

(define-module (nyacc lang c99 pprint)
  #:export (pretty-print-c)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  #:use-module (nyacc lang util)
  )

(define op-prec
  '((pre-inc pre-dec pos neg not)
    (mul div mod)
    (add sub)
    (lshift rshift rrshift)
    (lt gt le ge instanceof in)
    (equal not-equal not-equal-eq)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    ;; ...
    ))

(define op-assc
  '((left mul div mod add sub lshift rshift lt gt le ge)
    (right)
    (nonassoc)))

(define protect-expr? (make-protect-expr op-prec op-assc))

(define* (pretty-print-c tree #:key (indent-level 2))

  (define ppx
    (let* ((fmtr (make-pp-formatter))
	   (push-il (lambda () (fmtr 'push)))
	   (pop-il (lambda () (fmtr 'pop)))
	   (sf (lambda args (apply fmtr args)))
	   (sf-nl (lambda () (sf "\n")))
	   (ppx/p (lambda (tree) (sf "(") (ppx tree) (sf ")"))))

      (lambda (tree)
	(case (car tree)

	  (expression
	   

	  ((ary-ref)
	   (ppx (sx-ref tree 1)) (sf "[") (ppx (sx-ref tree 2)) (sf "]"))

	  ((lt gt le ge eq neq)
	   (let ((op (sx-ref tree 0))
		 (lval (sx-ref tree 1))
		 (rval (sx-ref tree 2)))
	     (if (protect-lval? op lval)
		 (ppx/p lval)
		 (ppx lval))
	     (case op
	       ((lt) (sf " < ")) ((gt) (sf " <= "))
	       ((le) (sf " > ")) ((ge) (sf " >= "))
	       ((eq) (sf " == ")) ((neq) (sf " != ")))
	     (if (protect-rval? op rval)
		 (ppx/p rval)
		 (ppx rval))
	     ))

	  ((add sub mul div)
	   (let ((op (sx-ref tree 0))
		 (lval (sx-ref tree 1))
		 (rval (sx-ref tree 2)))
	     (if (protect-expr? 'lt op lval)
		 (ppx/p lval)
		 (ppx lval))
	     (case op ;;(car tree)
	       ((add) (sf " + ")) ((sub) (sf " - "))
	       ((mul) (sf "*")) ((div) (sf "/")))
	     (if (protect-expr? 'rt op rval)
		 (ppx/p rval)
		 (ppx rval))
	     ))

	  ((de-ref ref-to)
	   (let ((op (sx-ref tree 0))
		 (ex (sx-ref tree 1)))
	     (sf (case op ((de-ref) "*") ((ref-to) "&")))
	     (if (protect-expr? 'lt op ex)
		 (ppx/p ex)
		 (ppx ex))))

	  ((char) (sf "'~A'" (sx-ref tree 1))
	  ((fixed) (sf "~A" (sx-ref tree 1))
	  ((float) (sf "~A" (sx-ref tree 1))
	  ((string) (sf "~S" (sx-ref tree 1)))
	  ((identifier) (sf "~A" (sx-ref tree 1)))

	  (else
	   (simple-format #t "\nnot handled: ~S\n" (car tree))
	   #f)))))

  (ppx tree))

;; --- last line ---
