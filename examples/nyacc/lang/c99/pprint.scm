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
  #:use-module (sxml match)
  #:use-module (ice-9 pretty-print)
  )

(define op-prec
  '((p-expr ident fixed float string)
    (comp-lit post-inc post-dec i-sel d-sel fctn-call array-ref)
    (de-ref ref-to neg pos not bitwise-not sizeof pre-inc pre-dec)
    (cast)
    (mul div mod)
    (add sub)
    (lshift rshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    (cond-expr)
    (assn-expr)
    (comma)
    ))

(define op-assc
  '((left array-ref d-sel i-sel post-inc post-dec comp-lit mul div mod add sub
	  lshift rshift lt gt le ge bitwise-and bitwise-xor bitwise-or and or)
    (right pre-inc pre-dec sizeof bitwise-not not pos neg ref-to de-ref cast
	   cond assn-expr)
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
	   (sf "#define ~A" (sx-ref (sx-ref tree 1) 1))
	   (and=> (assq-ref tree 'args)
		  (lambda (args)
		    (sf "(")
		    (pair-for-each
		     (lambda (pair)
		       (sf "~A" (cadar pair))
		       (if (pair? (cdr pair)) (sf ",")))
		     args)
		    (sf ")")))
	   (sf " ~A\n" (sx-ref (assq 'repl (cdr tree)) 1)))
		     
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

	  ((p-expr) (ppx (sx-ref tree 1)))
	  ((ident) (sf "~A" (sx-ref tree 1)))
	  ((char) (sf "'~A'" (sx-ref tree 1)))
	  ((fixed) (sf "~A" (sx-ref tree 1)))
	  ((float) (sf "~A" (sx-ref tree 1)))
	  ((string) (sf "~S" (sx-ref tree 1)))

	  ((comment) (sf "/*~A*/\n" (sx-ref tree 1)))

	  ((array-ref)
	   (ppx (sx-ref tree 2)) (sf "[") (ppx (sx-ref tree 1)) (sf "]"))

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

	  ((cast)
	   (let ((tn (sx-ref tree 1)) (ex (sx-ref tree 2)))
	     (sf "(") (ppx tn) (sf ")")
	     (if (protect-expr? 'rt 'cast ex)
		 (ppx/p ex)
		 (ppx ex))))

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
		 (ppx rval))))

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
		 (ppx rval))))

	  ((decl)
	   (let ((specs (sx-ref tree 1))
		 (initl (assq 'init-declr-list (cdr tree)))
		 (comm (assq 'comment (cdr tree))))
	     (ppx specs)
	     (if initl (ppx initl))
	     (sf "; ")			; leave space for comment
	     (if comm (ppx comm) (sf "\n"))))

	  ((decl-spec-list)
	   (let iter ((dsl (sx-tail tree 1)))
	     (when (pair? dsl)
	       (case (sx-tag (car dsl))
		 ((stor-spec) (sf "~A" (car (sx-ref (car dsl) 1))))
		 ((type-qual) (sf "qual=~A" (sx-ref (car dsl) 1)))
		 ((type-spec) (ppx (car dsl)))
		 (else
		  (sf "[?:~S] " (car dsl))))
	       (if (pair? (cdr dsl)) (sf " "))
	       (iter (cdr dsl)))))

	  ((init-declr-list comp-declr-list)
	   (pair-for-each
	    (lambda (pair)
	      (sf " ")
	      (ppx (car pair))
	      (if (pair? (cdr pair)) (sf ",")))
	    (sx-tail tree 1)))

	  ((init-declr comp-declr)
	   (let* ((declr (sx-ref tree 1))
		  (initr (sx-fref tree 2))
		  (iexpr (and initr (sx-ref initr 1)))
		  (comm #f)		; fix this
		  )
	     (ppx declr)
	     (when initr
	       (sf " = ")
	       (case (sx-tag iexpr)
		 ((initzer-list)
		  (sf "{")
		  (sf "initzer-list")
		  (sf " }"))
		 (else
		  (ppx iexpr)))
	       (sf "; ")
	       (if comm (ppx comm) (sf "\n")))))

	  ((type-spec)
	   (let ((arg (sx-ref tree 1))) ;; did I mess this up?
	     (case (sx-tag arg)
	       ((fixed-type) (sf "~A" (sx-ref arg 1)))
	       ((float-type) (sf "~A" (sx-ref arg 1)))
	       ((struct-ref) (ppx arg))
	       ((struct-def) (ppx arg))
	       ((union-ref) (ppx arg))
	       ((union-def) (ppx arg))
	       ((enum-def) (sf "TODO/3: ~S" (sx-ref arg 1)))
	       ((typename) (sf "~A" (sx-ref arg 1)))
	       (else (error "missing " arg)))))

	  ((struct-ref) (sf "struct ~A" (sx-ref (sx-ref tree 1) 1)))
	  ((union-ref) (sf "union ~A" (sx-ref (sx-ref tree 1) 1)))
	  
	  ((struct-def union-def)
	   (let ((name (assq-ref tree 'ident))
		 (flds (assq-ref tree 'field-list)))
	     (if name
		 (sf "struct ~S {\n" name)
		 (sf "struct {\n"))
	     (push-il)
	     (pair-for-each
	      (lambda (pair)
		(case (caar pair)
		  ((comment) (ppx (car pair)))
		  ((comp-decl) (ppx (car pair)))
		  (else (error "pprint: fixup struct-def"))))
	      flds)
	     (pop-il)
	     (sf "} ")))

	  ((comp-decl)
	   (let ((specs (sx-ref tree 1))
		 (initl (assq 'comp-declr-list (cdr tree)))
		 (comm (assq 'comment (cdr tree))))
	     (ppx specs)
	     (if initl (ppx initl))
	     (if comm (ppx comm) (sf "\n"))))

	  ;; (enum-def enum-ref)
	  ;; enum-def-list enum-defn
	  ;; fctn-spec
	  ;; ptr-declr
	  ;; array-of (THIS IS COMPLEX)
	  ;; ftn-declr
	  ;; pointer
	  ;; param-list
	  ;; param-decln
	  
	  ((type-name)
	   (let ((spec (sx-ref tree 1))
		 (abdr (and (<  2 (length tree)) (sx-ref tree 2))))
	     (if (not (eqv? (sx-tag spec) 'decl-spec-list))
		 (error "assuming decl-spec-list"))
	     (ppx spec)
	     (if abdr (ppx abdr))))

	  ;; abs-declr
	  ((abs-declr)
	   (let iter ((decls (sx-tail tree 1)))
	     (when (pair? decls)
	       (case (sx-tag (car decls))
		 ((pointer)
		  (sf "*"))
		 (else
		  (error "need to finish abs-declr")))
	       (iter (cdr decls)))))

	  ((trans-unit)
	   (pair-for-each
	    (lambda (pair)
	      (ppx (car pair))
	      ;; Need some heuristics on when to insert blank lines.
	      ;;(if (pair? (cdr pair)) (sf "\n"))
	      )
	    (sx-tail tree 1)))

	  ((cpp-stmt)
	   (cpp-ppx (sx-ref tree 1)))

	  ((extern-C-begin) (sf "extern \"C\" {\n"))
	  ((extern-C-end) (sf "}\n"))

	  (else
	   (simple-format #t "\nnot handled: ~S\n" (car tree))
	   #f)))))

  (ppx tree))

;; --- last line ---
