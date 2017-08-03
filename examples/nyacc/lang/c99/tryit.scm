;; examples/nyacc/lang/c99/tryit.scm

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang c99 util2))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    '("__GNUC__=6")
    (remove (lambda (s)
	      (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-gcc-cpp-defs)))
   (else
    '())))
(define inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/usr/include")
   (get-gcc-inc-dirs)))
(define inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin"
       "__builtin_va_list=void*"
       "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       ;;"__has_include(X)=__has_include__(X)"
       "__has_include=__has_include__"
       ;;"__dead2=" ;; /usr/include/unistd.h, l.645, __attribute__(X)
       ;;"__has_include(X)=" ;; kill me, again: unistd.h, l.655
       )
      ;;("sys/cdefs.h" "__DARWIN_ALIAS(X)=")
      ))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)=")
      ))))

(define mode 'code)
(define mode 'file)
(define mode 'decl)
(define debug #f)

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:xdef? #t))))

(define (parse-string str)
  ;;(simple-format #t "~S => \n" str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:mode mode #:debug debug 
		 #:xdef? #t))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define ppsx (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 pretty-print-c99)

;; The standard says:
;;   For two qualified types to be compatible, both shall have the identically
;;   qualified version of a compatible type; the order of type qualifiers within
;;   a list of specifiers or qualifiers does not affect the specified type.

;;(and=> (parse-file "exam.d/ex14.c") ppsx)
;;(and=> (parse-c99x "(a*b)+c") ppsx)

(define adecl #f)
(let* ((code "struct foo { int x, y; } *a, b;\n") (indx 1)
       (code "typedef int *foo_t[4]; foo_t bar[3], *baz;\n") (indx 2)
       (code (string-append
	      "struct foo { int a; double b; };\n"
	      "struct foo x;\n"))
       (code "enum { A = 1<<3, B } bar;\n")
       (code "char *s = \"foo\0bar\";\n")
       ;;(code "int x = ((*(expr))->id);\n") ;;  "(*expr)->id")
       ;;(code "((*(expr))->id)")
       (indx 2)
       (code (string-append
	      "#if __has_include(<stdio.h>)\n"
	      "int x;\n"
	      "#else\n"
	      "char x;\n"
	      "#endif\n"
	      ))
       (tree (parse-string code))
       ;;(tree (parse-c99x code))
       ;;(tree (parse-file "null.c"))
       
       ;;(udict (c99-trans-unit->udict tree))
       ;;(udecl (udict-ref udict "bar"))
       ;;(mspec (udecl->mspec udecl))
       ;;(decl (and=> ((sxpath `((decl ,indx))) tree) car))
       ;;(xdecl (expand-typerefs decl udict))
       )
  #;(with-input-from-string "(  <f o o.h>  )"
    (lambda () (sferr "~S\n" (scan-arg-literal)) ))
  #;(with-input-from-string "_foo__"
    (lambda () (sferr "~S\n" (read-c-ident #\_))))
  ;;(display code)
  ;;(ppsx tree)
  ;;(pp99 tree)
  ;;(ppsx udecl)
  ;;(sf "=>\n")
  ;;(ppsx xdecl)
  ;;(pp99 xdecl)
  ;;(set! adecl decl)
  #t)

;; --- last line ---
