;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(add-to-load-path (getcwd))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11)) ;; let*-values
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr sx) (pretty-print sx (current-error-port) #:per-line-prefix "  "))

(define cpp-defs
  (cond
   ((string-contains %host-type "darwin") '("__GNUC__=6" "__signed=signed"))
   (else (get-gcc-cpp-defs))))
(define inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/usr/include" "c99-exam"
     "/usr/include/glib-2.0" "/usr/lib/x86_64-linux-gnu/glib-2.0/include"
     ;;"/usr/include/dbus-1.0" "/usr/lib/x86_64-linux-gnu/dbus-1.0/include"
     ;;
     "/usr/include/cairo" "/usr/include/glib-2.0"
     "/usr/lib/x86_64-linux-gnu/glib-2.0/include"
     "/usr/include/pixman-1" "/usr/include/freetype2" "/usr/include/libpng12"
     )
   (get-gcc-inc-dirs)))
(define inc-help c99-def-help)

(define mode 'code)
(define debug #f)
(define xdef? (lambda (name mode) (memq mode '(code decl))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      ;;(pp cpp-defs) (pp inc-help) (pp inc-dirs)
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:show-incs #t
		 ;;#:xdef? xdef?
		 ))))

(define (parse-string str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:show-incs #f
 		 #:mode mode #:debug debug
		 #:xdef? xdef?))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

;; The standard says:
;;  "For two qualified types to be compatible, both shall have the identically
;;   qualified version of a compatible type; the order of type qualifiers within
;;   a list of specifiers or qualifiers does not affect the specified type."

;;(and=> (parse-file "c99-exam/ex14.c") ppin)
;;(and=> (parse-c99x "(a*b)+c") ppin)

(use-modules (arch-info))
(use-modules (system foreign))
(use-modules (ice-9 match))

;; @deffn {Procedure} eval-sizeof-type tree [udict ddict]
;; => (values sizeof-val align-of)
;; @end deffn

(define* (eval-sizeof-type tree #:optional (udict '()) (ddict '()))
  (unless (eq? 'sizeof-type (sx-tag tree)) (error "bad tree"))

  ;; Update size with var of size @var{s}, alignment @var{a}, on size @var{rs}.
  (define (update-size s a rs)
    (+ s (* a (quotient (+ rs (1- a)) a))))

  ;; update running size, base alignment
  (define (incr/udecl-list udecls size base-align)
    (let loop ((size size) (base-align base-align) (udecls udecls))
      (if (null? udecls)
	  (values size base-align)
	  (call-with-values
	      (lambda () (sizeof-mtail (cdr (udecl->mdecl (cdar udecls)))))
	    (lambda (elt-size elt-align)
	      (loop (update-size elt-size elt-align size)
		    (max elt-align base-align) (cdr udecls)))))))

  (define (sizeof-mtail mtail)
    (match mtail
      (`((pointer-to) . ,rest)
       (values (sizeof-basetype '*) (alignof-basetype '*)))
      (`((fixed-type ,name))
       (values (sizeof-basetype name) (alignof-basetype name)))
      (`((float-type ,name))
       (values (sizeof-basetype name) (alignof-basetype name)))
      (`((array-of ,size) . ,rest)
       (let ((mult (eval-c99-cx size)))
	 (call-with-values
	     (lambda () (sizeof-mtail rest))
	   (lambda (size align)
	     (values (* mult size) align)))))

      (`((struct-def (field-list . ,fields)))
       (let loop ((size 0) (align 0) (flds fields))
	 (cond
	  ((null? flds)
	   (values (update-size 0 align size) align))
	  (else
	   (call-with-values
	       (lambda ()
		 (let ((udecls (unitize-decl (car flds))))
		   (incr/udecl-list udecls size align)))
 	     (lambda (size align)
	       (loop size align (cdr flds))))))))

      (_ (sferr "c99/eval-sizeof-type: missed\n") (pperr mtail)
	 (quit)
	 (throw 'nyacc-error "coding error"))))

  (let* ((type-name (sx-ref tree 1))
	 (specl (sx-ref type-name 1))
	 (declr (or (sx-ref type-name 2) '(param-declr)))
	 ;;(x (begin (sf "1:\n") (ppin specl)))
	 ;;(x (begin (sf "2:\n") (ppin declr)))
	 (declr (reify-declr declr))	; not needed, as ->mdecl will do this
	 ;;(x (begin (sf "3:\n") (ppin declr)))
	 (udecl `(udecl ,specl ,declr))
	 ;;(x (begin (sf "4:\n") (ppin udecl)))
	 (xdecl (expand-typerefs udecl udict ddict))
	 ;;(x (begin (sf "5:\n") (ppin xdecl)))
	 (mdecl (udecl->mdecl xdecl))
	 ;;(x (begin (sf "6:\n") (ppin mdecl)))
	 )
    (sizeof-mtail (cdr mdecl))))


;; === from cxeval.scm:
(define (expand-typename typename udict)
  (let* ((decl `(udecl (decl-spec-list
			(type-spec (typename ,typename)))
		       (declr (ident "_"))))
	 (xdecl (expand-typerefs decl udict))
	 (xname (and xdecl (sx-ref* xdecl 1 1 1 1))))
    xname))
;; ===

;; int, int*, int*[], int **, int(), int(float), int*(float),
;; int*(float)[3]

(define (fold p s l)
  (let loop ((s s) (l l))
    (if (null? l) s (loop (p (car l) s) (cdr l)))))


(when #f
  (let* ((code "int foo = sizeof(int(*)());")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo")))
    (pp udecl)
    (sf "orig:  ~A\n" code)
    (sf "   =>") (pp99 udecl)))

(when #f
  (let* ((code (string-append
		"typedef int *bar_t;\n"
		;;"bar_t foo(bar_t (*)(bar_t));\n" ;; <= param-list broken
		"int foo(bar_t);\n"
		))
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (xdecl (expand-typerefs udecl udict))
	 )
    (pp udecl)
    (pp xdecl)
    ;;(pp99 udecl)
    ;;(pp99 xdecl)
    #t))

(when #t
  (let* ((code (string-append
		;;"int x = sizeof(int*(*)(int));\n"
		;;"typedef struct { int x; double z; void *p; } foo_t;\n" ;;24,8
		;;"typedef struct { int x,y; } foo_t;\n" ;; 8,4
		;;"typedef struct { int x; double y; } foo_t;\n"  ;; 16,8
		;;"typedef struct { int x,y; double z; } foo_t;\n" ;; 16,8
		"typedef struct { double x; int y; } foo_t;\n"  ;; 16,8
		;;"typedef int foo_t;\n" ;; 4,4
		;;"typedef long foo_t;\n" ;; 8,8
		;;"typedef int foo_t[5];\n" ;; 20,4
		"int x = sizeof(foo_t);\n"
		))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (sot-x (sx-ref* udecl 2 2 1))
	 ;;(declr (sx-ref (sx-ref sot-x 1) 2))
	 ;;(ctail (sx-tail (assoc-ref udict "y")))
	 )
    (newline)
    ;;(ppin sot-x)
    (call-with-values
	(lambda () (eval-sizeof-type sot-x udict))
      (lambda (size align)
	(sf "  sizeof(above)=~S alignof(above)=~S\n" size align)))
    (newline)
    #t))

;; --- last line ---
