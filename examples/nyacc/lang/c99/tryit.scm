;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppsx (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 pretty-print-c99)

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
(define inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin"
       "__builtin_va_list=void*"
       ;;"__attribute__(X)="
       "__extension__="
       "__inline=" "__inline__="
       "__restrict="
       "__THROW="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       )
      ))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" 
       ;;"__attribute__(X)="
       "__inline=" "__inline__="
       "__restrict="
       "__THROW="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       "__extension__="
       )
      ))))

(define mode 'file)
(define mode 'code)
(define mode 'decl)
(define debug #f)
(define xdef? (lambda (name mode) (memq mode '(code decl))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      ;;(pp cpp-defs) (pp inc-help) (pp inc-dirs)
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug #f
		 #:show-incs #f
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
		 #:mode mode #:debug #f
		 #:xdef? xdef?))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

;; The standard says:
;;  "For two qualified types to be compatible, both shall have the identically
;;   qualified version of a compatible type; the order of type qualifiers within
;;   a list of specifiers or qualifiers does not affect the specified type."

;;(and=> (parse-file "c99-exam/ex14.c") ppsx)
;;(and=> (parse-c99x "(a*b)+c") ppsx)

(define adecl #f)
;;(ppsx cpp-defs)
;;(ppsx inc-dirs)
;;(ppsx inc-help)

(let* ((code (string-append
	      "struct foo { int a; double b; };\n"
	      "struct foo x; /* hmmm */ \n"
	      ))
       ;;(tree (parse-string code))
       (tree (parse-file "zz.c"))
       (udict (c99-trans-unit->udict/deep tree))
       (udecl (udict-ref udict "gluSphere"))
       ;;(udecl (stripdown-udecl udecl))
       ;;(udecl (expand-typerefs udecl udict))
       ;;(mdecl (udecl->mspec/comm udecl))
       ;;(udecl (unitize-decl decl))
       )
  ;;(pp (get-gcc-cpp-defs))
  ;;(pp tree)
  ;;(pp udict)
  (pp udecl)
  ;;(pp mdecl)
  ;;(pp (parse-string "int ***x;"))
  ;;(ppsx udecl)
  ;;(pp99 xdecl)
  ;;(ppsx (eval-c99-cx tree))
  ;;(ppsx (get-gcc-inc-dirs))
  ;;(pp cpp-defs)
  #t)

;; --- last line ---
