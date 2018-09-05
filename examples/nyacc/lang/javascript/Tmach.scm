(define redo #f)
;; Tmach.scm - javascript
;;
;; Copyright (C) 2015,2016,2018 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (nyacc lang javascript mach))

(when redo
  (gen-javascript-files)
  ;;(system "touch parser.scm")
  )

(use-modules (nyacc lang javascript parser))
(use-modules (nyacc lang javascript pprint))
(use-modules (nyacc lang javascript compile-tree-il))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when redo
  (with-output-to-file ",file.txt"
    (lambda ()
      (pp-lalr-notice javascript-spec)
      (pp-lalr-grammar javascript-spec)
      (pp-lalr-machine javascript-mach))))

(when redo
  (with-output-to-file ",stmt.txt"
    (lambda ()
	;;(pp-lalr-notice javascript-ia-spec)
	(pp-lalr-grammar javascript-ia-spec)
	(pp-lalr-machine javascript-ia-mach))))

(when #f
  (with-output-to-file "gram.y.new"
    (lambda () (lalr->bison javascript-spec))))

(when #f
  (let* ((xargs (cdr (program-arguments)))
	 (file (if (pair? xargs) (car xargs) ",ex1.js"))
	 (res (with-input-from-file file dev-parse-js)))
    (let ((til (compile-tree-il res (current-module) '())))
      (let ((val (compile til
			  #:env (current-module)
			  #:from 'tree-il
			  #:to 'value)))
	(simple-format #t "~S\n" val)
	#t))))

;; --- last line ---
