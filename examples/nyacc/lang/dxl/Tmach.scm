;; Tmach.scm - DOORS DXL
;;
;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (string-append (getcwd) "/../../../../examples/"))
(add-to-load-path (string-append (getcwd) "/../../../../module/"))

(use-modules (nyacc lang dxl mach))
(use-modules (nyacc lalr))
(use-modules (nyacc export))
(use-modules (ice-9 pretty-print))

(unless dxl-spec (exit))

(when #t
  (gen-dxl-files)
  (system "touch parser.scm"))

(when #t
  (with-output-to-file ",lang.txt"
    (lambda ()
      (pp-lalr-notice dxl-spec)
      (pp-lalr-grammar dxl-spec)
      (pp-lalr-machine dxl-mach))))
(when #f
  (with-output-to-file ",gram.y"
    (lambda () (lalr->bison dxl-spec))))

(use-modules (sxml fold))
(use-modules (nyacc lang sx-match))


;; NOW WORKING
(define (elifify tree)
  (define (fU tree)
    (sx-match tree
      ((if ,x1 ,t1 (if ,x2 ,t2 (else-if ,x3 ,t3) . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) (else-if ,x3 ,t3) . ,rest))
      ((if ,x1 ,t1 (if ,x2 ,t2 . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) . ,rest))
      (*
       tree)))
  (foldt fU identity tree))

(when #f
  (let* ((sx (with-input-from-file "exam.d/ex1.dxl"
	       (lambda () (dev-parse-dxl #:debug #f))))
	 (sx (elifify sx))
	 )
    (pretty-print sx)
    #t))

#;(let ((lx (gen-dxl-lexer)))
  (with-input-from-file "exam.d/ex1.dxl"
    (lambda ()
      (let iter ((tp (lx)))
	(unless (eqv? (cdr tp) 'EOF)
	  (simple-format #t "~S\n" (cdr tp))
(iter (lx)))))))

;; --- last line ---
