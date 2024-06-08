;; examples/nyacc/lang/c99/tryit.scm

;; Copyright (C) 2020-2024 Matthew Wette
;;
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

;;(add-to-load-path (getcwd))

(use-modules (ice-9 textual-ports))
(use-modules (ice-9 popen))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))
(use-modules ((srfi srfi-1) #:select (last fold-right fold)))
(use-modules (srfi srfi-11))            ; let-values
(use-modules (rnrs arithmetic bitwise))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (sxml fold))
(use-modules ((sxml xpath) #:hide (filter)))

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

(set! *random-state* (random-state-from-platform))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define (ff fmt . args) (apply simple-format #f fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define cep current-error-port)
(define (sferr fmt . args) (apply simple-format (cep) fmt args))
(define (pperr sx) (pretty-print sx (cep) #:per-line-prefix "  "))
(define (ppe99 sx) (pretty-print-c99 sx (cep) #:per-line-prefix "  "))

(define (pp99s exp)
  (call-with-output-string (lambda (port) (pretty-print-c99 exp port))))


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

;; source-properties
(when #f
  (let* ((tree (parse-file "test1.c"))
         (node (and tree (sx-ref* tree 1))))
    (pp node)
    (sf "(source-properties node) => ~S\n" (source-properties node))))

(when #f                               ; bug #60474
  (let* ((code "const int x = 1;\n")
         (tree (parse-string code #:mode 'code))
         (udict (c99-trans-unit->udict tree))
         (udecl (assoc-ref udict "x"))
         (specl (sx-ref udecl 1))
         (declr (sx-ref udecl 2)))
    (pp udecl)
    (call-with-values (lambda () (cleanup-udecl specl declr))
      (lambda (specl declr) (pp `(udecl ,specl ,declr))))))

;; bug #60474
(when #f
  (let* ((code "const int x = 1;\n")
         (tree (parse-string code #:mode 'code))
         (udict (c99-trans-unit->udict tree))
         (udecl (assoc-ref udict "x"))
         (specl (sx-ref udecl 1))
         (declr (sx-ref udecl 2)))
    (pp udecl)
    (call-with-values (lambda () (cleanup-udecl specl declr))
      (lambda (specl declr)
        (pp `(udecl ,specl ,declr))))))

(when #f
  (let* ((code "typedef int foo_t; foo_t foo[] = { 1, 2, 3, 4 };" )
         (tree (parse-string code #:mode 'decl))
         (udict (c99-trans-unit->udict tree)))
    (pp tree)
    (pp udict)))

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

(when #f
  (let* ((code
          (string-append
           "typedef enum { N1 = 1, N2 = 2 } num_t;\n"
           "typedef struct { int m; double b[N2]; } bar_t;\n"))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         (udict (udict-add-enums udict))
         (udecl (udict-ref udict "bar_t"))
         (xdecl (expand-typerefs udecl udict))
         (mdecl (udecl->mdecl xdecl))
         (mtail (cdr mdecl)))
    (pp (sizeof-mtail mtail udict))
    (pp (find-offsets mtail udict))))

(when #f
  (let* ((code
          (string-append
           "typedef struct { double d; char c; } foo_t[3];\n"
           "int x = sizeof(foo_t);\n"))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         (foo-t '(type-name
                  (decl-spec-list (type-spec (typename "foo_t")))))
         (sz-exp `(sizeof-type ,foo-t)))
    (sf "Expect (3 . 16), get:\n")
    (pp (find-offsets foo-t udict))
    (pp (find-sizes foo-t udict))
    (sf "\ntotal size = ~A\n" (eval-sizeof-type sz-exp udict))))

(when #f
  (let* ((code "int foo = sizeof(\"$ABCDEF\")*2;")
         (tree (or (parse-string code) (error "parse failed")))
         (udict (c99-trans-unit->udict tree))
         (udecl (assoc-ref udict "foo"))
         (expr (sx-ref* udecl 2 2 1)))
    (sf "declaration::\n")
    (pp udecl)
    (sf "evaluate:\n")
    (sf "x = ~S\n" (eval-c99-cx expr))))

(when #f
  (let* ((code
          (string-append
           "typedef enum let { A = 1, B = 2 } let_t;\n"
           "typedef struct { double x; let_t y; } foo_t;\n"))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         (foo-t '(type-name (decl-spec-list (type-spec (typename "foo_t"))))))
    (pp (find-types foo-t udict '("let_t")))))

;; bug 57949
(when #f
  (let* ((code "struct foo { void bar(void); };\n")
         (tree (parse-string code))
         (tree (remove-comments tree))
         (udict (c99-trans-unit->udict tree))
         (udecl (assoc-ref udict '(struct . "foo")))
         (fdecl (fh-cnvt-udecl udecl udict))
         (sdecl (with-input-from-string fdecl read)))
    (display fdecl)))

;; bug #63604
(when #f
  (let* ((code "int foo(const float color[static 4]);")
         (tree (parse-string code)))
    (pp tree)))

;; =============================================================================

(define (rand-bitfields n)
  (define types (list "short" "int" "long"))
  (define sizes (map (lambda (st ft) (cons st (* 8 (ffi:sizeof ft))))
                     types (list ffi:short ffi:int ffi:long)))
  (define (rbfdecl st ix)
    (let* ((bs (assoc-ref sizes st))
           (nb (random (1+ bs)))
           (vn (ff "v~S" ix)))
      (cond
       ((zero? (random 3)) (ff "  ~A ~A;\n" st vn))
       ((= nb bs) (ff "  ~A ~A;\n" st vn))
       ((zero? nb) (ff "  ~A :0;\n" st))
       (else (ff "  ~A ~A: ~A;\n" st vn nb)))))
  (let loop ((lines '()) (nn 1))
    (if (> nn n)
        (string-join (reverse lines) "")
        (loop (cons (rbfdecl (list-ref types (random 3)) nn) lines) (1+ nn)))))

(define (check-case n)
  (let* ((code (string-append "struct {\n" (rand-bitfields n) "} tt;\n"
                              "long x = sizeof(tt);\n"))
         (prog (string-append "#include <stdio.h>\n" code
                              "int main() { printf(\"%ld\", x); }\n"))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         (expr (sx-ref* tree 2 2 1 2 1))
         (gcc-result
          (begin
            (with-output-to-file "ttx.c" (lambda () (display prog)))
            (system "gcc ttx.c")
            (string->number (get-string-all (open-input-pipe "./a.out")))))
         (c99-result (eval-c99-cx expr udict)))
    (equal? gcc-result c99-result)))

(when #f
  (let loop ((n 10))
    (cond
     ((zero? n) #t)
     ((check-case 9) (loop (1- n)))
     (else #f))))

(define (rand-fields n)
  (define types (list "short" "int" "long" "double" "float"))
  (define sizes (list ffi:short ffi:int ffi:long ffi:double ffi:float))
  (define ntype (length types))
  (define (rbfdecl sx ix)
    (let* ((st (list-ref types sx)) (bs (list-ref sizes sx))
           (nb (random (1+ bs))) (vn (ff "v~S" ix)))
      (cond
       ((or (> sx 2) (= ix n) (zero? (random 3))) (ff "  ~A ~A;\n" st vn))
       ((= nb bs) (ff "  ~A ~A;\n" st vn))
       ((zero? nb) (ff "  ~A :0;\n" st))
       (else (ff "  ~A ~A: ~A;\n" st vn nb)))))
  (let loop ((lines '()) (nn 1))
    (if (> nn n)
        (string-join (reverse lines) "")
        (loop (cons (rbfdecl (random ntype) nn) lines) (1+ nn)))))

(when #f
  (let* ((code
          (string-append
           ;;"struct foo {\n" (rand-fields 5) "};\n"
           ;;"long tsz = __builtin_offsetof(struct foo, v5);\n"
           "struct foo {\n long v5[4][5]; };\n"
           "long offs = __builtin_offsetof(struct foo, v5[1+1][3]);\n"
           "long size = sizeof(struct foo);\n"
           ))
         (prog
          (string-append
           "#include <stdio.h>\n"
           code
           "int main() { printf(\"%ld\", tsz); }\n"))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         (offs-ex (sx-ref* tree 2 2 1 2 1))
         (size-ex (sx-ref* tree 3 2 1 2 1))
         ;;(c99-result (eval-c99-cx exp-foo udict))
         ;;(size (eval-c99-cx
         #|
         (gcc-result
          (begin
            (with-output-to-file "tt8c.c" (lambda () (display prog)))
            (system "gcc tt8c.c")
            (string->number (get-string-all (open-input-pipe "./a.out")))))
         |#
         )
    (pp offs-ex)
    (pp size-ex)
     (sf "gettin offs=~S size=~S\n"
         (eval-c99-cx offs-ex udict)
         (eval-c99-cx size-ex udict))
    (sf "expect offs=104 size=160\n")
    #|
    (sf "c99: ~S  gcc: ~S\n" c99-result gcc-result)
    (unless (equal? c99-result gcc-result) (pp code) (exit 1))
    |#
    #t))


(define (parse-path text)
  (with-input-from-string text
    (lambda ()
      (let loop ((terms '()) (ch (read-char)))
        (cond
         ((eof-object? ch) terms)
         ((char=? #\*) #f)
         )))))

#|
(use-modules (nyacc lang c99 cxeval))
(define (ss fmt . args) (apply simple-format #f fmt args))
(define (x-gen-offset t-exp)
  (define (mkoff l) (ss "OFFSET(0,~A)" (string-join l ".")))
  (let loop ((os '()) (cn '()) (tx t-exp))
    (match tx
      (`(ident ,n) (map mkoff (cons (cons n cn) os)))
      (`(p-expr ,ex) (loop os cn ex))
      (`(d-sel (ident ,n) ,ex) (loop os (cons n cn) ex))
      (`(i-sel (ident ,n) ,ex) (loop (cons (cons n cn) os) '() ex)))))

(define (gen-offset expr)
  (let loop ((os '()) (cn '()) (tx expr))
    (match tx
      (`(ident ,n) (cons tx os)))
      (`(d-sel (ident ,n) ,ex) (loop (cons n cn) ex))
      ;;(`(de-ref ,expr) (cons
      ;;
      (`(p-expr ,ex) (loop os cn ex))
      (`(i-sel ,id ,ex) (loop os ch `(d-sel ,id (de-ref ,ex))))
      ))
|#

(when #f
  (let* ((code
          (string-append
           "typedef struct { int p; int q; } foo_t;\n"
           "typedef struct { int r; foo_t s; foo_t *t; } bar_t;\n"
           "typedef struct { int u; bar_t v; bar_t *w; } baz_t;\n"
           "baz_t f;\n"
           ))
         (tree (parse-string code))
         (udict (c99-trans-unit->udict tree))
         ;;
         (comp "baz_t")
         (path "w->s")
         (code `(udecl (decl-spec-list (type-spec (typename ,comp)))
                       (comp-declr (ident "_"))))
         (udict (cons `("_" . ,code) udict))
         (path (string-append "_." path))
         (t-exp (parse-c99-cx path))
         (type (eval-typeof-expr t-exp udict)) ;; (type-name ,specl ,declr)
         ;;
         #|
         |#
         ;;(path "a.b->c.d")
         ;;(t-exp (parse-c99-cx path))
         )
    ;;(pp tree)
    ;;(pp path)
    ;;(pp t-exp)
    ;;(pp (gen-offset t-exp))
    (pp type)
    0))

(when #f
  ;; bug 65849
  (let* ((code (string-append
                "#warning \"warn me!\"\n"
                "#warning \"oops\"\n"))
         (tree (parse-string code)))
    #t))

;; --- last line ---
