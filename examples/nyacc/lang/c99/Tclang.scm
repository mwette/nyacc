;; Tlangc.scm - clang dev
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 pgen))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang util))		; move-if-changed
(use-modules (nyacc lalr))
(use-modules (nyacc util))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))
;;(use-modules (srfi srfi-43))

(define defs '(("arch" . "x86_64")))
(define incs '("."))

;; test parser
(let ((sx (with-input-from-file "ex1.c"
	    (lambda ()
	      (dev-parse-c #:cpp-defs defs #:inc-dirs incs #:debug #f)))))
  (pretty-print sx)
  (pretty-print-c99 sx)
  #t)

(with-output-to-file "lang.txt.new"
  (lambda ()
    (let* ((notice (assq-ref (assq-ref clang-spec 'attr) 'notice))
           (lines (if notice (string-split notice #\newline) '())))
      (for-each (lambda (l) (simple-format #t "  ~A\n" l)) lines)
      (newline))
    (pp-lalr-grammar clang-spec)
    (pp-lalr-machine clang-mach)))
(move-if-changed "lang.txt.new" "lang.txt")

(with-output-to-file "gram.y.new" (lambda () (lalr->bison clang-spec)))
(move-if-changed "gram.y.new" "gram.y")

(write-lalr-tables clang-mach "tables.scm.new")
(write-lalr-actions clang-mach "actions.scm.new")

(when (or (move-if-changed "actions.scm.new" "actions.scm")
	  (move-if-changed "tables.scm.new" "tables.scm"))
  (system "touch parser.scm")
  (compile-file "parser.scm"))

;; expression parser
(let* ((cexpr-spec (restart-spec clang-mach 'expression))
       (cexpr-mach (make-lalr-machine cexpr-spec)))
  (write-lalr-tables cexpr-mach "exprtab.scm.new")
  (write-lalr-actions cexpr-mach "expract.scm.new"))

(when (or (move-if-changed "expract.scm.new" "exprtab.scm")
	  (move-if-changed "exprtab.scm.new" "exprtab.scm"))
  (system "touch xparser.scm")
  (compile-file "xparser.scm"))


;; --- last line ---
