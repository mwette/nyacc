;; examples/nyacc/lang/c99/tryit.scm

;; Copyright (C) 2020-2021 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;(add-to-load-path (getcwd))

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 munge-base))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang c99 ffi-help))
(use-modules (nyacc lang arch-info))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (sxml fold))
(use-modules (sxml xpath))
(use-modules ((srfi srfi-1) #:select (fold-right)))
(use-modules (srfi srfi-11))		; let-values
(use-modules (rnrs arithmetic bitwise))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))


(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define cep current-error-port)
(define (sferr fmt . args) (apply simple-format (cep) fmt args))
(define (pperr sx) (pretty-print sx (cep) #:per-line-prefix "  "))
(define (ppe99 sx) (pretty-print-c99 sx (cep) #:per-line-prefix "  "))

(define *cpp-defs* (get-gcc-cpp-defs))
(define *inc-dirs* (get-gcc-inc-dirs))
(define *inc-help* c99-def-help)

(define *mode* (make-parameter 'code))
(define *debug* (make-parameter #f))
(define *xdef?* (lambda (name mode) (memq mode '(code decl))))

(define* (parse-file file #:key cpp-defs inc-dirs mode debug)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define* (parse-string str
		       #:optional (tyns '())
		       #:key cpp-defs inc-dirs mode debug)
  (with-input-from-string str
    (lambda ()
      (parse-c99 tyns
		 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
 		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(define (fold p s l)
  (let loop ((s s) (l l))
    (if (null? l) s (loop (p (car l) s) (cdr l)))))

(define-syntax-rule (pass-if mesg expr)
  (begin
    (display mesg)
    (newline)
    (pp expr)))

(define (remove-comments tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (sx-match tree
      ((comment ,text) seed)
      (,_ (if (pair? seed) (cons (reverse kseed) seed) (reverse kseed)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

(when #f
  (let* ((code "int foo = sizeof(int(*)());")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (expr (sx-ref* udecl 2 2 1))
	 )
    (sf "declaration::\n")
    (pp udecl)
    (sf "extract initializer expression:\n")
    (pp expr)
    (sf "evaluate:\n")
    (sf "x = ~S\n" (eval-c99-cx expr))))
(when #f
  (let* ((code "int intx;\n")
	 (tree (or (parse-string code) (error "parse failed")))
	 )
    (sf "~A\n" code)
    (ppin tree)
    (pp99 tree)
    ))
(when #f
  (let* ((code "int foo(int x) asm(\"foo\");")
	 ;;(code "int foo(int x);")
	 (tree (parse-string code)))
    (pp code) (pp tree) (pp99 tree) (newline)
    ))
(when #f
  (let* ((code "*(x->y->z)")
	 (tree (parse-c99x code)))
    (pp code) (pp tree) (pp99 tree) (newline)
    ))
(when #f
  (let* ((code
	  (string-append
	   "void foo() { __asm__ goto (\"mov r0,r1\" : "
	   ": [mcu] \"I\" (123), [ssr] \"X\" (456) "
	   " : \"foo\", \"bar\" : error ); }"))
	 (tree (parse-string code #:mode 'decl))
 	 )
    (pp tree)
    #t))

(when #f
  (let* ((code
	  (string-append
	   "#define sei() __asm__ __volatile__ (\"sei\" ::: \"memory\")\n"
	   "int foo() { sei(); }\n"
	   ))
	 (tree (parse-string code #:mode 'code))
 	 )
    #t))
(when #f
  (let* ((code "int foo() { spice->meas[1].pin = &mega->portD.pin[0]; }\n")
	 (tree (parse-string code #:mode 'code)))
    (pp tree)
    #t))

(when #f
  (let* ((code
	  (string-append
	   "#define ISR(vector, ...) void vector (__VA_ARGS__) \n"
	   "ISR(__vector__12__) { int x; }\n"))
	 (tree (parse-string code #:mode 'code)))
    (pp tree)
    (pp99 tree)
    ))

(when #f
  (let* ((code "typedef enum { A, B=3, C } foo;")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (edl (sx-ref* udecl 1 2 1 1))
	 (xxx (canize-enum-def-list edl))
	 )
    (pp udecl)
    (pp edl)
    (pp xxx)
    ))

(when #f
  (let* ((code
	  (string-append
	   #|
	   "int foo() {\n"
	   "  typedef int foo_t;\n"
	   "  {\n"
	   "    typedef int foo_t[3];\n"
	   "    1;\n"
	   "  }\n"
	   "}\n"
	   |#
	   "typedef int foo_t;\n"
	   "int foo() {\n"
	   "  typedef int foo_t[3];\n"
	   "  1;\n"
	   "}\n"
	   ))
	 (tree (parse-string code #:mode 'code)))
    (pp tree)))

(when #f                               ; bug #60474
  (let* ((code
	  (string-append
	   "const int x = 1;\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (specl (sx-ref udecl 1))
	 (declr (sx-ref udecl 2))
	 )
    (pp udecl)
    (call-with-values (lambda () (cleanup-udecl specl declr))
      (lambda (specl declr)
	(pp `(udecl ,specl ,declr))))
    ))

(when #f
  (let* ((code
	  (string-append
	   "#define bar(X) #X\n"
	   "#define foo(X) bar(X)\n"
	   "char *s = foo('abc');\n"))
	 (tree (parse-string code #:mode 'decl)))
    (pp tree)))

(when #f
  (let* ((code
	  (string-append
           "#if 1\n"
           "#define g_abort() abort ()\n"
           "#else\n"
           "void g_abort (void);\n"
           "#endif\n"
           "int x;\n"))
	   (tree (parse-string code #:mode 'decl)))
	  (pp tree)
	  ))

(when #f
  (let* ((code
	  (string-append
	   "typedef struct { int x; double y; } foo_t;\n"
	   ;;"void bar(foo_t);\n"
	   "int x = sizeof(foo_t);\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 )
    (pp tree)
    ))

(when #f
  (let* ((code
	  (string-append
	   "const int x = 1;\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (specl (sx-ref udecl 1))
	 (declr (sx-ref udecl 2))
	 )
    (pp udecl)
    (pp (cleanup-udecl specl declr))
    ))

(when #f
  (let* ((code
	  (string-append
	   "typedef struct {\n"
	   " int x;\n"
	   " union { int a; int b; };\n"
	   " int y;\n"
	   " union { int c[3]; double d; };\n"
	   ;;" int z;\n"
	   "} foo_t;\n"
	   "foo_t s1;\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "s1"))
	 (udecl (expand-typerefs udecl udict))
	 (mdecl (udecl->mdecl udecl))
	 (mtail (cdr mdecl))
	 )
    ;;(pp tree)
    ;;(pp udecl)
    ;;(pp mdecl)
    ;;(pp (mtail->bs-desc mtail))
    (pp (mtail->ffi-desc mtail))
    ))

(when #f				; bug #60474
  (let* ((code
	  (string-append
	   "const int x = 1;\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (specl (sx-ref udecl 1))
	 (declr (sx-ref udecl 2))
	 )
    (pp udecl)
    (call-with-values (lambda () (cleanup-udecl specl declr))
      (lambda (specl declr)
	(pp `(udecl ,specl ,declr))))
    ))

(when #f
  (let* ((code "typedef int foo_t; foo_t foo[] = { 1, 2, 3, 4 };" )
	 (tree (parse-string code #:mode 'decl))
	 (udict (c99-trans-unit->udict tree))
	 ;;(udecl (assoc-ref udict "x"))
	 ;;(specl (sx-ref udecl 1))
	 ;;(declr (sx-ref udecl 2))
	 )
    (pp tree)
    (pp udict)
    ))


;; with-arch from (nyacc lang arch-info)
(when #f
  (let* ((code
	  (string-append
	   "typedef struct {\n"
	   "  char c;\n"
	   "  void *p;\n"
	   "} foo_t;\n"
	   ))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo_t"))
	 (type-name '(type-name (decl-spec-list (type-spec (typename "foo_t")))))
	 )
    (display code) (newline)
    (with-arch "native"
      (sf "native:\n")
      (sf "  size=~S\n" (eval-sizeof-type `(sizeof-type ,type-name) udict))
      (sf "  align=~S\n" (eval-alignof-type `(alignof-type ,type-name) udict)))
    (with-arch "avr"
      (sf "avr:\n")
      (sf "  size=~S\n" (eval-sizeof-type `(sizeof-type ,type-name) udict))
      (sf "  align=~S\n" (eval-alignof-type `(alignof-type ,type-name) udict)))
    ))

(when #f
  ;; bad printout: typedef (const) void *gconstpointer;
  ;; fixed
  (let* ((tree  '(udecl (decl-spec-list
			 (stor-spec (typedef))
			 (type-qual (const))
			 (type-spec (void)))
			(init-declr
			 (ptr-declr (pointer) (ident "gconstpointer"))))))
    (pretty-print-c99 tree)))

;; symbol could have init-zer so need
;; (initzer->data

;; shortened types?
;; (decl (decl-spec-list (type-spec ...)) (declr-list (declr))
;; => (type (type-spec ...) (param-declr ...)

;; @deffn {Procedure} num-lit-suffix lit-str => list
;; Return the trailing characters in a numeric literal.
;; @end deffn
(define (num-lit-suffix lit-str)
  (let loop ((chl '()) (ix (1- (string-length lit-str))))
    (cond
     ((zero? ix) chl)
     ((char-numeric? (string-ref lit-str ix)) chl)
     (else (loop (cons (string-ref lit-str ix) chl) (1- ix))))))

;; @deffn literal-type-spec
;; For a literal return a reasonable type spec. @*
;; @end deffn
(define (literal-type-spec lit)
  (match lit
    (`(fixed-type ,sval) `(fixed-type "int"))
    ))

(define* (eval-cx expr #:optional udict ddict #:key fail-proc)

  (define (fail fmt . args)
    (and fail-proc (apply fail-proc fmt args)))

  (define (ddict-lookup name)
    (let ((repl (assoc-ref ddict name)))
      (cond
       ((not repl) #f)
       ((pair? repl) #f)
       ((string=? name repl) #f)
       (else repl))))
  
  (define (eval-ident sx)
    (let* ((name (sx-ref sx 1)) (repl (ddict-lookup name)))
      (and (string? repl) (string->number repl))))
  
  (define (uop op ex)
    (and op ex (op ex)))
  
  (define (bop op lt rt)
    (and op lt rt (op lt rt)))

  (letrec
      ((ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3

       (eval-expr ;; expr type-spec declr => expr type-spec declr
	(lambda* (expr #:optional type)
	  (case (sx-tag expr)
	    ((fixed)
	     (values
	      (string->number (cnumstr->scm (sx-ref expr 1)))
	      (if type type (literal-type-spec expr))))
	    ((float) (string->number (cnumstr->scm (sx-ref expr 1))))
	    ((char) (char->integer (string-ref (sx-ref expr 1) 0)))
	    ((string) (string-join (sx-tail expr 1) ""))
	    ((pre-inc post-inc) (uop 1+ (ev1 expr)))
	    ((pre-dec post-dec) (uop 1- (ev1 expr)))
	    ((pos) (and expr (ev1 expr)))
	    ((neg) (uop - (ev1 expr)))
	    ((not) (and expr (if (equal? 0 (ev1 expr)) 1 0)))

	    ((div) (bop / (ev1 expr) (ev2 expr)))
	    ((mod) (bop modulo (ev1 expr) (ev2 expr)))
	    ((add) (bop + (ev1 expr) (ev2 expr)))
	    ((sub) (bop - (ev1 expr) (ev2 expr)))
	    ((lshift) (bop bitwise-arithmetic-shift-left (ev1 expr) (ev2 expr)))
	    ((rshift) (bop bitwise-arithmetic-shift-right (ev1 expr) (ev2 expr)))
	    ((lt) (if (bop < (ev1 expr) (ev2 expr)) 1 0))
	    ((le) (if (bop <= (ev1 expr) (ev2 expr)) 1 0))
	    ((gt) (if (bop > (ev1 expr) (ev2 expr)) 1 0))
	    ((ge) (if (bop >= (ev1 expr) (ev2 expr)) 1 0))
	    ((eq) (if (bop = (ev1 expr) (ev2 expr)) 1 0))
	    ((ne) (if (bop = (ev1 expr) (ev2 expr)) 0 1))
	    ((bitwise-not) (uop lognot (ev1 expr)))
  	    ((bitwise-or) (bop logior (ev1 expr) (ev2 expr)))
	    ((bitwise-xor) (bop logxor (ev1 expr) (ev2 expr)))
	    ((bitwise-and) (bop logand (ev1 expr) (ev2 expr)))
	    ;;
	    ((or)
	     (let ((e1 (ev1 expr)) (e2 (ev2 expr)))
	       (if (and e1 e2) (if (and (zero? e1) (zero? e2)) 0 1) #f)))
	    ((and)
	     (let ((e1 (ev1 expr)) (e2 (ev2 expr)))
	       (if (and e1 e2) (if (or (zero? e1) (zero? e2)) 0 1) #f)))
	    ((cond-expr)
	     (let ((e1 (ev1 expr)) (e2 (ev2 expr)) (e3 (ev3 expr)))
	       (if (and e1 e2 e3) (if (zero? e1) e3 e2) #f)))
	    ;;
	    ((sizeof-type)
	     (catch 'c99-error
	       (lambda () (eval-sizeof-type expr udict))
	       (lambda (key fmt . args) (apply fail fmt args))))
	    ((sizeof-expr)
	     (catch 'c99-error
	       (lambda () (eval-sizeof-expr expr udict))
	       (lambda (key fmt . args) (apply fail fmt args))))
	    ((alignof)
	     (catch 'c99-error
	       (lambda () (eval-alignof-type expr udict))
	       (lambda (key fmt . args) (apply fail fmt args))))
	    ((offsetof)
	     (catch 'c99-error
	       (lambda () (eval-offsetof expr udict))
	       (lambda (key fmt . args) (apply fail fmt args))))
	    ((ident) (or (eval-ident expr)
			 (fail "cannot resolve identifier ~S" (sx-ref expr 1))))
	    ((p-expr) (ev1 expr))
	    ((cast) (ev2 expr))

	    ((ref-to)
	     (let*-values (((x1 t1) (ev1 expr)))
	       (pp x1)
	       (pp t1)
	       (values x1 t1)))

	    ((fctn-call) #f)		; assume not constant
	    ;;
	    ;; TODO 
	    ((comp-lit) (fail "cxeval: comp-lit not implemented"))
	    ((comma-expr) (fail "cxeval: comma-expr not implemented"))
	    ((i-sel) (fail "cxeval: i-sel not implemented"))
	    ((d-sel) (fail "cxeval: d-sel not implemented"))
	    ((array-ref) (fail "cxeval: array-ref not implemented"))
	    ;; 
	    (else
	     (sferr "eval-c99-cx:") (pperr expr)
	     (throw 'c99-error "eval-c99-cx: coding error"))))))

    (eval-expr expr)))

(define (mkdsg str)
  (let loop ((res '()) (elts (string-split str #\.)))
    (cond
     ((null? res)
      (loop `(p-expr (ident ,(car elts))) (cdr elts)))
     ((pair? elts)
      (loop `(d-sel (ident ,(car elts)) ,res) (cdr elts)))
     (else res))))

(when #f
  (let* ((code
	  (string-append
	   "enum {  FOO = sizeof(int), BAR = sizeof(double) }; \n"
	   "typedef struct {\n"
	   "  int x1, x2;\n"
	   "  struct { int x1, x2; } xx;\n"
	   "  int y;\n"
	   "  struct { int y1; void *y2; } yy[FOO];\n"
	   ;;"  struct { int y1; void *y2; } yy;\n"
	   ;;"  struct { int z1; void *z2; };\n"
	   "} foo_t;\n"
	   "int sz = sizeof(foo_t);\n"
	   "int al = _Alignof(foo_t);\n"
	   ;;"int os = __builtin_offsetof(foo_t, xx.x3);\n"
	   "int os = __builtin_offsetof(foo_t, yy[2].y2);\n"))
	 (main
	  (string-append
	   "#include <stdio.h>\n"
	   "int main() {\n"
	   " printf(\"sz=%d\\n\", sz);\n"
	   " printf(\"al=%d\\n\", al);\n"
	   " printf(\"os=%d\\n\", os);\n"
	   "}\n"))
	 (tree (parse-string code #:mode 'code))
	 (udict (c99-trans-unit->udict tree))
	 (udict (udict-add-enums udict))
	 (sz (udict-ref udict "sz"))
	 (sz-of-ty (sx-ref* sz 2 2 1))
	 (al (udict-ref udict "al"))
	 (al-of-ty (sx-ref* al 2 2 1))
	 (os (udict-ref udict "os"))
	 (os-of-ty (sx-ref* os 2 2 1)))
    ;;(display code) (display main)
    (pp os-of-ty) (quit)
    (sf "/*\n")
    (with-arch "native"
      (sf "native:\n")
      (sf "  size = ~S\n" (eval-sizeof-type sz-of-ty udict))
      (sf "  align = ~S\n" (eval-alignof-type al-of-ty udict))
      (sf "  offset = ~S\n" (eval-offsetof os-of-ty udict)))
    (with-arch "avr"
      (sf "avr:\n")
      (sf "  size = ~S\n" (eval-sizeof-type sz-of-ty udict))
      (sf "  align = ~S\n" (eval-alignof-type al-of-ty udict))
      (sf "  offset = ~S\n" (eval-offsetof os-of-ty udict)))
    (sf "*/\n")))

(when #f
  (let* ((code
	  (string-append
	   ;;"enum { NN = sizeof(int), MM = sizeof(long) };\n"
	   "typedef enum { N1 = 1, N2 = 2 } num_t;\n"
	   "typedef struct { int m; double b[N2]; } bar_t;\n"
	   ;;"typedef struct { int x; double z[3][4]; bar_t bar; } foo1_t;\n"
	   ;;"typedef struct { int r; double c[2]; } foo2_t;\n"
	   ;;"typedef struct { int s; double d[5]; } foo3_t;\n"
	   ;;"typedef struct { foo1_t f1; foo2_t *f2; foo3_t f3[N]; } foo_t;\n"
	   ))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 (udict (udict-add-enums udict))
	 ;;(udecl (udict-ref udict "foo_t"))
	 (udecl (udict-ref udict "bar_t"))
	 (xdecl (expand-typerefs udecl udict))
	 (mdecl (udecl->mdecl xdecl))
	 (mtail (cdr mdecl))
	 )
    ;;(sf "~A\n" code)
    ;;(pp udict)
    (pp (sizeof-mtail mtail udict))
    (pp (find-offsets mtail udict))
    0))

(when #f
  (let* ((code
	  (string-append
	   "typedef struct { int m; double b[N2]; } bar_t;\n"
	   "typedef struct { int x; double z[3][4]; bar_t bar; } foo1_t;\n"
	   "typedef struct { int r; double c[2]; } foo2_t;\n"
	   "typedef struct { int s; double d[5]; } foo3_t;\n"
	   "typedef struct { foo1_t f1; foo2_t *f2; foo3_t f3[N]; } foo_t;\n"
	   "foo_t f;\n"
	   ))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (udict-ref udict "f"))
	 (t-exp `(typeof-expr (d-sel (ident "f1") (ident "f"))))
	 )
    (pp (eval-typeof-expr t-exp udict))
    0))

(when #f
  (let* ((code
	  (string-append
	   "typedef struct { double d; char c; } foo_t[3];\n"
	   "int x = sizeof(foo_t);\n"
	   ))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 ;;(udecl (udict-ref udict "x"))
	 (foo-t '(type-name
		  (decl-spec-list (type-spec (typename "foo_t")))))
	 (sz-exp `(sizeof-type ,foo-t))
	 )
    (sf "Expect (3 . 16), get:\n")
    (pp (find-offsets foo-t udict))
    (pp (find-sizes foo-t udict))
    (sf "\ntotal size = ~A\n" (eval-sizeof-type sz-exp udict))
    0))

;; --- last line ---
