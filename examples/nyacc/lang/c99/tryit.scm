;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang util))
(use-modules (nyacc lang sx-match))
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
     "/usr/include"
     "c99-exam")
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

(define mode 'code)
(define mode 'decl)
(define mode 'file)
(define debug #f)
(define xdef? (lambda (name mode) (memq mode '(code))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:xdef? xdef?))))

(define (parse-string str)
  ;;(simple-format #t "~S => \n" str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:mode mode #:debug debug 
		 #:xdef? xdef?))))

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
       ;;(code "#if L'c'\nint x = 1;\n#endif\n")
       ;;(code "#include <Eina.h>\nint x = 1;\n")
       ;;(code "wchar_t x = U'c';\n")
       (code "char x = '\\b';\nchar *y = \"\\b\";\n")
       (code (string-append
	      "typedef int *bla_t[2];\n"
	      "bla_t foo(bla_t (*)(bla_t));\n"))
       (code (string-append
	      "#define NX 3\n"
	      "const int zz = 3;"
	      "int x[NX+1];\n"))
       (indx 2)
       (code "((int)'q')")
       
       ;;(tree (parse-c99x "\"\""))
       ;;(tree (parse-c99x code))
       ;;(tree (parse-file "c99-exam/ex14.c"))
       ;;(expr (sx-ref* tree 2 2 1 1 2))
       
       ;;(udict (c99-trans-unit->udict tree))
       ;;(ddict (udict-enums->ddict udict))
       ;;(ddict (c99-trans-unit->ddict tree))
       
       ;;(decl (and=> ((sxpath `((decl ,indx))) tree) car))
       ;;(xdecl (expand-typerefs decl udict))
       ;;(udecl (udict-ref udict "x"))
       ;;(mdecl (udecl->mspec udecl))
       ;;(decl (and=> ((sxpath `((decl ,indx))) tree) car))
       ;;(exp (parse-c99x "1+2"))
       ;;(exp (parse-c99x "sizeof(\"abc\")"))
       ;;(val (eval-c99-cx exp))
       ;;(tree (parse-c99x "((int)'q')"))
       (tree (parse-c99x "\"abc\" \"def\""))
       )
  ;;(ppsx exp)
  ;;(ppsx val)
  ;;(display code)
  (ppsx tree)
  ;;(ppsx (eval-c99-cx tree))
  ;;(sf "dd:\n") (ppsx ddict)
  ;;(ppsx (eval-c99-cx expr udict ddict))
  ;;(sf "~S\n" (
  ;;(ppsx decl)
  ;;(pp99 xdecl)
  ;;(ppsx (get-gcc-inc-dirs))
  #t)

;;(use-modules (nyacc lang c99 ffi-help))

;;(define fmod (fh-c-fun-decl->procedure "doulble fmod(double x, double y)"))

#|
(sf "~S\n"
    (with-input-from-string "'\\177'"
	(lambda () (read-c-chlit (read-char)))))

(sf "~S\n"
    (with-input-from-string "\"\\177\""
	(lambda () (read-c-string (read-char)))))
|#

;;(pp (sx-match '(foo (bar "baz") "hello")
;;       ((foo (bar . ,text) . ,rest) #t) (* #f)))

;; ex12.c: illustrates removal of comment prefix, offset-8 => offset-2

;;(let ((tree (parse-file "c99-exam/ex01.c"))) (pp99 tree))
;;(ugly-print (quote `(abc ,@def)))
;; --- last line ---
