#!/opt/local/bin/guile -e main -s
!#

(use-modules (srfi srfi-37))

(define (arg:inc-dir opt name arg incs defs files)
  (values (cons arg incs) defs files))

(define (arg:define opt name arg incs defs files)
  (values incs (cons arg defs) files))

(define (arg:free arg incs defs files)
  (values incs defs (cons arg files)))

(define (bad-arg opt name arg incs defs files)
  (simple-format #t "bad-arg: ~S\n" name)
  (quit))

(define opts
  (list
   (option '(#\I "inc-dir") #t #t arg:inc-dir)
   (option '(#\D "define") #t #t arg:define)))

(define (process-args args)
  (call-with-values
      (lambda () (args-fold args opts bad-arg arg:free '() '() '()))
    (lambda (incs defs files)
      (list (cons 'incs (reverse incs))
	    (cons 'defs (reverse defs))
	    (cons 'files (reverse files))))))

(let* (;;(args (cdr (program-arguments)))
       ;;(argd (process-args args))
       ;;(file (car (assq-ref argd 'files)))
       ;;(defs (assq-ref argd 'defs))
       ;;(incs (assq-ref argd 'incs))
       (file "cairo.h")
       (defs (gen-gcc-defs #f))
       (incs '("/usr/include"))
       #;(sx (call-with-input-file file
	     (lambda (p) (xml->sxml p #:trim-whitespace? #t))))
       (sx (process-spec sx #:defs defs #:incs incs))
       )

  ;;(debug-disable 'backtrace)
  ;;(display "=>\n")
  ;;(pretty-print sx)
  (simple-format #t "defs=~S\n" defs)
  ;;(sxml->xml sx)
  
  0)

(define (main argl)
 (display "hello, world!\n")
 )

;; --- last line ---

