;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    (append
     '("__GNUC__=6" "__signed=signed")
     #;(remove (lambda (s)
	       (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	     (get-gcc-cpp-defs))
     ))
   (else
    '())))
(define inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/opt/local/include/eina-1"
     "/opt/local/include/eina-1/eina"
     "/tmp"
     "/usr/include")
   (get-gcc-inc-dirs)))
(define inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin"
       "__builtin_va_list=void*"
       "__attribute__(X)="
       "__extension__="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       )
      ))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__extension__="
       )
      ))))

(define mode 'file)
(define mode 'decl)
(define mode 'code)
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
(define pp pretty-print)
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
       (code (string-append
	      "typedef int *foo_t;\n"
	      "typedef double hmm_t[3];\n"
	      "int baz(foo_t (*baz)(hmm_t y));\n"
	      ))
       (code (string-append
	      "typedef struct foo foo_t;\n"
	      "foo_t *y;\n"
	      "foo_t z;\n"
	      ))
       (code "int x = 0x00000000FFFFFFFFLLU;")
       (code (string-append
	      "typedef struct {\n"
	      "  unsigned int is_bin : 1, is_write : 2;\n"
	      "  long lineno;"
	      "} foo_t;\n"
	      ))
       (code "double x = 123.4f;")
       (code "#if L'c'\nint x = 1;\n#endif\n")
       (code "#include <Eina.h>\nint x = 1;\n")
       ;;(code "wchar_t x = U'c';\n")
       (indx 2)
       (tree (parse-string code))
       ;;(tree (parse-file "xxx.c"))
       ;;(udict (c99-trans-unit->udict tree))
       ;;(ddict (udict-enums->ddict udict))
       ;;(udecl (udict-ref udict "x"))
       ;;(mdecl (udecl->mspec udecl))
       ;;(decl (and=> ((sxpath `((decl ,indx))) tree) car))
       ;;(xdecl (expand-typerefs udecl udict '((struct . "foo"))))
       #;(cpp-defs '(("G_VA_COPY" . "va_copy")
		   ("va_copy" ("a" "b") . "__builtin(a,b)")))
       )
  ;;(sferr "~S\n" (with-input-from-string "12'" (lambda () (read-hex))))
  ;;(display code)
  (ppsx tree)
  ;;(pp99 tree)
  ;;(ppsx xdecl)
  ;;(display "==\n")
  ;;(pp99 xdecl)
  ;;(ppsx mspec)
  ;;(ppsx (expand-typerefs udecl udict '("foo_t")))
  ;;(ppsx ddict)
  ;;(set! adecl decl)
  ;;(ppsx (get-gcc-cpp-defs))
  #t)

;; ex12.c: illustrates removal of comment prefix, offset-8 => offset-2
;;(let ((tree (parse-file "exam.d/ex12.c"))) (pp99 tree))
      
;;(ugly-print (quote `(abc ,@def)))

(define n1570a ;; sec 6.7.8
  (string-append
   "typedef signed int t;\n"
   "typedef int plain;\n"
   "struct tag {\n"
   "  unsigned t:4;\n"
   "  const t:5;\n"
   "  plain r:5;\n"
   "};\n"))
(define n1570b
  (string-append
   n1570a
   "int foo() {\n"
   "  t f(t (t));\n"
   "  long t;\n"
   "  return 1;\n"
   "};\n"))

;; --- last line ---
