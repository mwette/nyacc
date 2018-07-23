(define redo #t)
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
  (gen-js-files) (system "touch parser.scm")
  (gen-ia-files) (system "touch iaparser.scm"))

(use-modules (nyacc lang javascript parser))
(use-modules (nyacc lang javascript pprint))
(use-modules (nyacc lang javascript compile-tree-il))
(use-modules (nyacc lalr))
(use-modules (nyacc parse))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(when redo
  (with-output-to-file ",lang.txt"
    (lambda ()
      (pp-lalr-notice js-spec)
      (pp-lalr-grammar js-spec)
      (pp-lalr-machine js-mach))))

(when redo
  (with-output-to-file ",ialang.txt"
    (lambda ()
      ;; This needs to be same as in mach.scm!
      (let* ((ia-spec (restart-spec js-spec 'ProgramElement))
	     (ia-mach (make-lalr-machine ia-spec))
	     (ia-mach (compact-machine ia-mach #:keep 0))
	     (ia-mach (hashify-machine ia-mach)))
	;;(pp-lalr-notice ia-spec)
	(pp-lalr-grammar ia-spec)
	(pp-lalr-machine ia-mach)))))

(when #f
  (with-output-to-file "gram.y.new"
    (lambda () (lalr->bison js-spec))))

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
