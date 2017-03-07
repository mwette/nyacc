#!/opt/local/bin/guile -e main -s
!#

(add-to-load-path (string-append (getcwd) "/../../../../examples/"))
(add-to-load-path (string-append (getcwd) "/../../../../module/"))

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 util1))


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

(define my-defs (gen-gcc-defs '()))
(define my-help '())
(define (my-xdef? name mode)
  (cond
   ((< (string-length name) 6) #f)
   ((string= name "CAIRO" 0 5) #t)
   ((string= name "cairo" 0 5) #t)
   (else #f)))
   

(define (my-parse defs incs file)
  (with-input-from-file file
    (lambda () (parse-c99 #:cpp-defs (append my-defs defs) #:inc-dirs incs
                          #:inc-help my-help #:xdef? my-xdef?
			  #:mode 'file #:debug #f))))

(let* (;;(args (cdr (program-arguments)))
       ;;(argd (process-args args))
       ;;(file (car (assq-ref argd 'files)))
       ;;(defs (assq-ref argd 'defs))
       ;;(incs (assq-ref argd 'incs))
       (file "cairo.h")
       (defs '()) ;;'("CAIRO_BEGIN_DECLS="))
       (incs '("/opt/local/include" "/opt/local/include/cairo" "/usr/include"))
       #;(sx (call-with-input-file file
	     (lambda (p) (xml->sxml p #:trim-whitespace? #t))))
       ;;(sx (process-spec sx #:defs defs #:incs incs))
       (xx (my-parse defs incs "/opt/local/include/cairo/cairo.h"))
       )

  0)

(define (main argl)
 (display "hello, world!\n")
 )

;; --- last line ---

