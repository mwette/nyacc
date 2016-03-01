;; Tmach.scm - clang automaton and dev parser
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang c99 mach))
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
  (simple-format #t "===>")
  (pretty-print-c99 sx)
  #t)

(define (module-path filename)
  (string-append "../../../../module/nyacc/lang/c99/" filename))

#;(with-output-to-file "lang.txt.new"
  (lambda ()
    (let* ((notice (assq-ref (assq-ref clang-spec 'attr) 'notice))
           (lines (if notice (string-split notice #\newline) '())))
      (for-each (lambda (l) (simple-format #t "  ~A\n" l)) lines)
      (newline))
    (pp-lalr-grammar clang-spec)
    (pp-lalr-machine clang-mach)
    (move-if-changed "lang.txt.new" "lang.txt")))

#;(with-output-to-file "gram.y.new"
  (lambda ()
    (lalr->bison clang-spec)
    (move-if-changed "gram.y.new" (module-path "gram.y"))))

(begin
  (write-lalr-actions clang-mach "actions.scm.new")
  (write-lalr-tables clang-mach "tables.scm.new")
  (when (or (move-if-changed "actions.scm.new" (module-path "actions.scm"))
	    (move-if-changed "tables.scm.new" (module-path "tables.scm")))
    (system (string-append "touch " (module-path "parser.scm")))
    (compile-file (module-path "parser.scm"))))

;; expression parser
#;(let* ((cexpr-spec (restart-spec clang-mach 'expression))
       (cexpr-mach (make-lalr-machine cexpr-spec)))
  (write-lalr-actions cexpr-mach "expract.scm.new")
  (write-lalr-tables cexpr-mach "exprtab.scm.new")
  (when (or (move-if-changed "expract.scm.new" (module-path "expract.scm"))
	    (move-if-changed "exprtab.scm.new" (module-path "exprtab.scm")))
    (system (string-append "touch " (module-path "xparser.scm")))
    (compile-file (module-path "xparser.scm"))))

;; --- last line ---
