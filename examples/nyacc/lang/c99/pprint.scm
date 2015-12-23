;;; nyacc/lang/c99/pprint.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nyacc lang c99 pprint)
  #:export (pretty-print-c99)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  #:use-module (nyacc lang util)
  )

(define op-prec
  '((p-expr ident fixed float string)
    (d-sel i-sel post-inc post-dec)
    (pre-inc pre-dec sizeof pos neg not bitwise-not ref-to de-ref)
    (mul div mod)
    (add sub)
    (lshift rshift rrshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    (assn-expr)
    ))

(define op-assc
  '((left mul div mod add sub lshift rshift lt gt le ge)
    (right)
    (nonassoc)))

(define protect-expr? (make-protect-expr op-prec op-assc))

(define* (pretty-print-c99 tree #:key (indent-level 2))

  (define cpp-ppx
    (let* ((fmtr (make-pp-formatter)) ;; formatter needs to use \\
	   (sf (lambda args (apply fmtr args))))
      (lambda (tree)
	(case (car tree)
	  ;; statements
	  ((if) (sf "#if ") (cpp-ppx (sx-ref tree 1)) (sf "\n"))
	  ((else) (sf "#else\n"))
	  ((endif) (sf "#endif\n"))
	  ((include) (sf "#include ~A\n" (sx-ref tree 1)))
	  ((define)
	   (sf "#define ~A")
	   (and=> (assq-ref tree 'args)
		  (lambda (args)
		    (sf "(")
		    (pair-for-each
		     (lambda (pair)
		       (sf "~S" (car pair))
		       (if (pair? (cdr pair)) (sf ", ")))
		     args)
		    (sf ")")))
	   (sf " ~A\n" (assq-ref tree 'repl)))
		     
	  ;; expressions ..
	  ((defined)
	   (sf "defined(~A)" (sx-ref tree 1)))
	  ))
      ))
	   
  (define ppx
    (let* ((fmtr (make-pp-formatter))
	   (push-il (lambda () (fmtr 'push)))
	   (pop-il (lambda () (fmtr 'pop)))
	   (sf (lambda args (apply fmtr args)))
	   (sf-nl (lambda () (sf "\n")))
	   (ppx/p (lambda (tree) (sf "(") (ppx tree) (sf ")"))))

      (lambda (tree)
	(case (car tree)

	  ((trans-unit)
	   (for-each ppx (sx-tail tree 1)))

	  ((extern-C-begin) (sf "extern \"C\" {\n"))
	  ((extern-C-end) (sf "}\n"))

	  ((comment)
	   (sf "/* ~A */\n" (sx-ref tree 1)))

	  ((cpp-stmt)
	   (cpp-ppx (sx-ref tree 1)))

	  ((decl)
	   (ppx (sx-ref tree 1))	; decl-spec-list
	   (sf " ")
	   (sf "TODO:declr")
	   (sf ";\n"))

	  ((decl-spec-list)
	   (let iter ((dsl (sx-ref tree 1)))
	     (when (pair? dsl)
	       (case (car dsl)
		 ((stor-spec)
		  (sf "stor-spec"))
		 ((type-qual)
		  (sf "type-qual"))
		 ((type-spec)
		  (sf "type-spec"))
		 (else
		  (sf "[~S] " (car dsl))))
	       (if (pair? (cdr dsl)) (sf " "))
	       (iter (cdr dsl)))))

	  ((ary-ref)
	   (ppx (sx-ref tree 1)) (sf "[") (ppx (sx-ref tree 2)) (sf "]"))

	  ((lt gt le ge eq neq)
	   (let ((op (sx-ref tree 0))
		 (lval (sx-ref tree 1))
		 (rval (sx-ref tree 2)))
	     (if (protect-expr? 'lt op lval)
		 (ppx/p lval)
		 (ppx lval))
	     (case op
	       ((lt) (sf " < ")) ((gt) (sf " <= "))
	       ((le) (sf " > ")) ((ge) (sf " >= "))
	       ((eq) (sf " == ")) ((neq) (sf " != ")))
	     (if (protect-expr? 'rt op rval)
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

	  ((d-sel i-sel)
	   (let ((op (sx-ref tree 0))
		 (id (sx-ref tree 1))
		 (ex (sx-ref tree 2)))
	     (if (protect-expr? 'lt op ex)
		 (ppx/p ex)
		 (ppx ex))
	     (sf (case op ((d-sel) ".") ((i-sel) "->")))
	     (ppx id)))

	  ((p-expr)
	   (ppx (sx-ref tree 1)))

	  ((char) (sf "'~A'" (sx-ref tree 1)))
	  ((fixed) (sf "~A" (sx-ref tree 1)))
	  ((float) (sf "~A" (sx-ref tree 1)))
	  ((string) (sf "~S" (sx-ref tree 1)))
	  ((ident) (sf "~A" (sx-ref tree 1)))

	  (else
	   (simple-format #t "\nnot handled: ~S\n" (car tree))
	   #f)))))

  (ppx tree))

;; --- last line ---
