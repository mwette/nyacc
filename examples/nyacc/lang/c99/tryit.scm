;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(add-to-load-path (getcwd))

(use-modules (srfi srfi-1))
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
(define ppsx (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))

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
  ;;(simple-format #t "~S => \n" str)
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

;;(and=> (parse-file "c99-exam/ex14.c") ppsx)
;;(and=> (parse-c99x "(a*b)+c") ppsx)

(use-modules (arch-info))
(use-modules (system foreign))

;; @deffn {Procedure} eval-sizeof-type tree [udict ddict]
;; => (values sizeof-val align-of)
;; @end deffn

(define* (eval-sizeof-type tree #:optional (udict '()) (ddict '()))
  (unless (eq? 'sizeof-type (sx-tag tree)) (error "bad tree"))
  (let* ((type-name (sx-ref tree 1))
	 (specl (sx-ref type-name 1))
	 (declr (reify-declr (sx-ref type-name 2)))
	 (udecl `(udecl ,specl ,declr))
	 ;;(xdecl (expand-typerefs udecl udict ddict))
	 (mdecl (udecl->mdecl udecl))
	 )
    (ppsx udecl)
    (pp99 udecl)
    (pp mdecl)
    #f))

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

(when #t
  (pp
  (fold
   (lambda (pair status)
     (let* ((tree (parse-string (car pair)))
	    (udict (c99-trans-unit->udict tree))
	    (udecl (assoc-ref udict "foo"))
	    (namer (lambda () "@"))
	    (mdecl (udecl->mdecl udecl #:namer namer)))
       (unless (equal? mdecl (cdr pair))
	 (sf "code: ~S\n" (car pair))
	 (sf "expected:\n") (ppsx (cdr pair))
	 (sf "expanded:\n") (ppsx mdecl)
	 (newline))
       (and status (equal? mdecl (cdr pair)))
       ))
   #t '(("int foo;" . ("foo" (fixed-type "int")))
	("int foo();" . ("foo" (function-returning (param-list))
			 (fixed-type "int")))
	("int *();" . ("@" (function-returning (param-list))
		       (pointer-to) (fixed-type "int")))
	))))

(when #f
  (let* ((code (string-append
		;;"typedef int bar_t[2];\n"
		;;"typedef int bar_t;\n"
		"bar_t foo(bar_t (*)(bar_t));\n" ;; <= param-list broken
		"int foo(bar_t);\n"
		))
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (xdecl (expand-typerefs udecl udict))
	 )
    (pp udecl)
    (pp xdecl)
    (pp99 udecl)
    (pp99 xdecl)
    #t))

(when #f
  (let* ((code (string-append
		"typedef struct { int x; double z; void *p; } foo_t;\n"
		"int x = sizeof(int*(*)(int));\n"
		"int *(*y)(int);\n"
		))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (sot-x (sx-ref* udecl 2 2 1))
	 ;;(declr (sx-ref (sx-ref sot-x 1) 2)
	 ;;(ctail (sx-tail (assoc-ref udict "y")))
	 )
    (newline)
    (ppsx sot-x)
    (eval-sizeof-type sot-x udict)
    #t))

;; --- last line ---
