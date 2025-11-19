;;; scripts/compile-ffi.scm --- NYACC's command-line FFI compiler

;; Copyright (C) 2017-2021,2024-2025 Matthew Wette
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Author: Matt Wette <matt.wette@gmail.com>

;;; Commentary:
;; Usage: compile-ffi [ARGS]
;; Type `compile-ffi --help' for help.

;;; Code:

(define-module (scripts compile-ffi)
  #:use-module (nyacc foreign arch-info)
  #:use-module (system base language)
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module ((srfi srfi-1) #:select (fold fold-right lset-union every))
  #:use-module (srfi srfi-37)
  #:version (3 00 2))

(define *ffi-help-version* "3.00.2")

(define (compile-scm file)
  (compile-file file
                #:from 'scheme #:to 'bytecode
                #:optimization-level 1 #:opts '()))

(define (fix-path path)
  (let* ((cwd (getcwd)))
    (if (string-contains path cwd)
        (substring/shared path (1+ (string-length cwd)))
        path)))

(define %summary
  "Compile a ffi-file to .scm and maybe .go.")

(define (sfmt fmt . args)
  (apply simple-format (current-error-port) fmt args)
  (force-output (current-error-port)))

(define (fail fmt . args)
  (simple-format  (current-error-port) "compile-ffi: error: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (warn fmt . args)
  (simple-format  (current-error-port) "compile-ffi: warning: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (note fmt . args)
  (simple-format  (current-error-port) "compile-ffi: notice: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (show-version)
  (simple-format #t "compile-ffi ~A\n" *ffi-help-version*))

(define (acons/seed key val seed)
  (acons key (cons val (or (assq-ref seed key) '())) seed))

(define (show-usage)
  (simple-format #t "Usage: guild compile-ffi [OPTIONS] FILE
Generate a Guile Scheme file from the source FFI file FILE.

  -h, --help            print this help message
  --version             print version number

  -b, --backend=BACKEND back end target: only cdata for now
  -L, --load-path=DIR   add DIR to the front of the module load path
  -I, --inc-dir=DIR     add DIR to list of dir's to search for C headers
  -o, --output=OFILE    write output to OFILE
  -y, --output-dir=ODIR write files to directory ODIR
  -d, --debug=x,y       set debug flags: echo-decl, parse
  -s, --show-incs       show includes during parsing
  -m, --machine=MACHINE target machine, if non-native (e.g., i686)
  -D, --list-deps       list dependencies and quit
  -X, --no-exec         don't generate .go file(s)
  -R, --dont-recurse    don't do recursive compile on dep's
  -a, --any-suffix      allow any suffix (if not, must be .ffi)
  -w, --no-foreign-library  guile has no (system foreign-library) module

See the FFI-Helper User's Manual for help generating a .ffi file.
Report bugs to https://savannah.nongnu.org/projects/nyacc.\n"))

(define options
  ;; specification of command-line options
  ;; (option (char str) req-arg? opt-arg? proc)
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'help #t opts) files)))
   (option '("version") #f #f
           (lambda (opt name arg opts files)
             (show-version) (exit 0)))
   (option '(#\d "debug") #t #f
           (lambda (opt name arg opts files)
             (values (acons 'debug arg opts) files)))
   (option '(#\o "output") #t #f
           (lambda (opt name arg opts files)
             (if (assoc-ref opts 'output)
                 (fail "`-o' option cannot be specified more than once"))
             (values (acons 'output arg opts) files)))
   (option '(#\y "output-dir") #t #f
           (lambda (opt name arg opts files)
             (if (assoc-ref opts 'output-dir)
                 (fail "`-y' option cannot be specified more than once"))
             (values (acons 'output-dir arg opts) files)))
   (option '(#\b "backend") #t #f
           (lambda (opt name arg opts files)
             (values (acons 'backend (string->symbol arg) opts) files)))
   (option '(#\m "machine") #t #f
           (lambda (opt name arg opts files)
             (values (acons 'machine arg opts) files)))
   (option '(#\s "show-incs") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'show-incs #t opts) files)))
   (option '(#\L "load-path") #f #f
           (lambda (opt name arg opts files)
             (values (acons/seed 'load-path arg opts) files)))
   (option '(#\I "inc-dir") #t #f
           (lambda (opt name arg opts files)
             (values (acons/seed 'inc-dirs arg opts) files)))
   (option '(#\D "list-deps") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'list-deps #t (acons 'no-recurse #t opts)) files)))
   (option '(#\R "dont-recurse") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'no-recurse #t opts) files)))
   (option '(#\X "no-exec") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'no-exec #t opts) files)))
   (option '(#\a "any-suffix") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'any-suffix #t opts) files)))
   (option '(#\w "no-foreign-library") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'no-foreign-library #t opts) files)))))

;; from scripts/compile.scm
(define (parse-args args)
  (args-fold args
             options
             (lambda (opt name arg files opts)
               (fail "unrecognized option: ~S" name)
               (exit 1))
             (lambda (file opts files)
               (or (assq-ref opts 'any-suffix)
                   (string-suffix? ".ffi" file)
                   (fail "expecting .ffi suffix"))
               (values opts (cons file files)))
             `((backend . ,(or (getenv "FFI_HELP_BACKEND") 'cdata))
               (machine . "native")) '()))

;; --- check dependencies -------------------------------------------

(define-syntax find-ffi-uses
  (lambda (x)
    (syntax-case x ()
      ((_ key val rest ...)
       (eq? (syntax->datum #'key) #:use-ffi-module)
       #'(cons (quote val) (find-ffi-uses rest ...)))
      ((_ attr rest ...) #'(find-ffi-uses rest ...))
      ((_) #''()))))

(define-syntax-rule (define-ffi-module spec attr ...)
  (cons (quote spec) (find-ffi-uses attr ...)))
(export define-ffi-module)

(define (find-in-path file)
  (let loop ((pathl (cons "." %load-path)))
    (if (null? pathl) #f
        (let ((path (string-append (car pathl) "/" file)))
          (if (access? path R_OK) path
              (loop (cdr pathl)))))))

(define (more-recent? file1 file2)
  ;; file1 is more recent than file2 (from ice-9/boot-9.scm)
  (let ((stat1 (stat file1)) (stat2 (stat file2)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
        (and (= (stat:mtime stat1) (stat:mtime stat2))
             (>= (stat:mtimensec stat1)
                 (stat:mtimensec stat2))))))

(define (tsort filed filel)
  (define (covered? deps done) (every (lambda (e) (member e done)) deps))
  (let loop ((done '()) (hd '()) (tl filel))
    (if (null? tl)
        (if (null? hd) done (loop done '() hd))
        (cond
         ((not (assq-ref filed (car tl)))
          (loop (cons (car tl) done) hd (cdr tl)))
         ((covered? (assq-ref filed (car tl)) done)
          (loop (cons (car tl) done) hd (cdr tl)))
         (else
          (loop done (cons (car tl) hd) (cdr tl)))))))

;; Given ffi-module filename, return alist of dependent specs
(define (module-ffi-deps file)
  (unless (access? file R_OK)
    (fail "not found: ~S" file))
  (call-with-input-file file
    (lambda (iport)
      (let ((env (make-fresh-user-module)))
        (eval '(use-modules (scripts compile-ffi)) env)
        (let loop ((exp (read iport)))
          (cond
           ((eof-object? exp) (fail "no `define-ffi-module' in ~S" file))
           ((and (pair? exp) (eqv? 'define-ffi-module (car exp))) (eval exp env))
           (else (loop (read iport)))))))))

;; from requested compiled file, return depends-on dict of .ffi-files
(define (all-ffi-deps file)
  ;; depd is file->mods, specd is mod->file
  (let loop ((depd '()) (specd '()) (file file) (todo '()))
    (cond
     ((and file (not (assoc-ref depd file)))
      (let ((mdep (module-ffi-deps file))) ;; (mod-spec . dep-specs)
        (loop (acons file (cdr mdep) depd) (acons (car mdep) file specd)
              #f (append todo (cdr mdep)))))
     ((pair? todo)
      (let* ((base (string-join (map symbol->string (car todo)) "/"))
             (ffi-file (string-append base ".ffi"))
             (path (find-in-path ffi-file)))
        (unless path (fail "can't find ~s" ffi-file))
        (loop depd specd path (cdr todo))))
     (else
      (reverse
       (map (lambda (d)
              (cons (car d) (map (lambda (m) (assoc-ref specd m)) (cdr d))))
            depd))))))

(define (gen-supd depd all)
  (map (lambda (file)
         (cons file (fold
                     (lambda (ent s)
                       (if (member file (cdr ent)) (cons (car ent) s) s))
                     '() depd)))
       all))

(define (scm-for-ffi ffi-file opts)
  (cond
   ((assq-ref opts 'output))
   ((assq-ref opts 'output-dir) =>
    (lambda (dir) (string-append dir "/" (basename ffi-file ".ffi") ".scm")))
   (else (string-append (string-drop-right ffi-file 4) ".scm"))))

;; from list of all files and depd, return supplier-for dict
(define (ensure-ffi-deps file opts)

  (define (check-ffi-vs-scm ffis seed)
    (fold
     (lambda (ffi seed)
       (let ((scm (scm-for-ffi ffi opts)))
         (cond
          ((not (access? scm R_OK)) (cons ffi seed))
          ((more-recent? ffi scm) (cons ffi seed))
          (else seed))))
     seed ffis))
  
  (define (add-sups supd todo)
    (fold
     (lambda (supd-ent seed)
       (if (member (car supd-ent) todo)
           (lset-union string=? seed (cdr supd-ent))
           seed))
     todo supd))

  ;; 1. if scm does not exist or is older than ffi add it
  ;; 2. for each added, add all mods it is supplier for
  (let* ((depd (all-ffi-deps file))
         (all (apply lset-union string=? depd))
         (supd (gen-supd depd all))
         (tord (reverse (tsort depd all)))
         (todo (check-ffi-vs-scm all '()))
         (todo (add-sups supd todo)))
    (for-each
     (lambda (ffi)
       (when (and (member ffi todo) (not (string=? ffi file)))
         (compile-ffi ffi (acons 'output #f opts))))
     tord)))


;; -----------------------------------------------------------------------------

(define (compile-ffi ffi-file opts)
  (let* ((scm-file (scm-for-ffi ffi-file opts))
         (mod (case (assq-ref opts 'backend)
                ((cdata cd) '(nyacc lang c99 ffi-help-cd))
                (else (fail "bad backend: ~s" (assq-ref opts 'backend)))))
         (compile-ffi-file (module-ref (resolve-module mod) 'compile-ffi-file)))
    (when (assq-ref opts 'list-deps)
      (for-each
       (lambda (f) (sfmt "~A\n" (fix-path f)))
       (apply lset-union string=? (all-ffi-deps ffi-file)))
      (exit 0))
    (catch 'ffi-help-error
      (lambda ()
        (catch 'c99-error
          (lambda ()
            (sfmt "compiling `~A' ...\n" (fix-path ffi-file))
            (compile-ffi-file ffi-file (acons 'output scm-file opts))
            (sfmt "... wrote `~A'\n" (fix-path scm-file)))
          (lambda (key fmt . args)
            (apply throw 'ffi-help-error fmt args))))
      (lambda (key fmt . args)
        (if (access? scm-file W_OK) (delete-file scm-file))
        (apply fail fmt args)))
    (unless (assq-ref opts 'no-exec)
      (sfmt "compiling `~A' ...\n" (fix-path scm-file))
      (let ((go-file (compile-scm scm-file)))
        (load-compiled go-file)
        (sfmt "... wrote `~A'\n" (basename go-file)))
      (sleep 1))))

(define (main . args)
  (call-with-values
      (lambda () (parse-args args))
    (lambda (opts files)
      (when (or (assq-ref opts 'help) (null? files)) (show-usage) (exit 0))
      (unless (string=? (assq-ref opts 'machine) "native")
        (unless (memq (assq-ref opts 'backend) '(cdata))
          (fail "only cdata supports non-native machines architectures"))
        (unless (getenv "CC")
          (fail "for non-native machine architectures define env var CC")))
      (for-each
       (lambda (file)
         (with-arch (assq-ref opts 'machine)
           (unless (assq-ref opts 'no-recurse)
             (ensure-ffi-deps file opts))
           (compile-ffi file opts)))
       (reverse files))))
  (exit 0))

;; --- last line ---
