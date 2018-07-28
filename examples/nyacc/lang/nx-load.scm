;;; nyacc/lang/nx-load.scm - loading Guile extension languages

;; Copyright (C) 2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;

(define-module (nyacc lang nx-load)
  #:export (nx-load
	    nx-compile-file
	    nx-compile-and-load
	    )
  #:use-module (system base language)
  #:use-module (system base message)
  #:use-module (ice-9 receive)
  )

;; === new stuff =======================

;; or save to 
(define (skip-whitespace port)
  (let loop ((ch (read-char port)))
    (simple-format (current-error-port) " skip ~S\n" ch)
    (cond
     ((eof-object? ch) ch)
     ((char-whitespace? ch) (loop (read-char port)))
     (else (unread-char ch port)))))

(define (next-word port)
  (let loop ((chl '()) (ch (read-char port)))
    (cond
     ((eof-object? ch) (reverse-list->string chl))
     ((char-whitespace? ch) (unread-char ch port) (reverse-list->string chl))
     (else (loop (cons ch chl) (read-char port))))))

;; unread all except one
(define (unread-chars-1 chl port)
  (let loop ((chl chl))
    (cond
     ((null? (cdr chl)) (car chl))
     (else (unread-char (car chl) port)
	   (loop (cdr chl))))))

(define (unread-chars chl port)
  (let loop ((chl chl))
    (cond
     ((null? chl))
     (else (unread-char (car chl) port)
	   (loop (cdr chl))))))

(define (make-keyword-reader keyword)
  (let ((n (string-length keyword)))
    (lambda (port)
      (simple-format (current-error-port) "slurp-lang? ~S n=~S\n" port n)
      (let loop ((ix 0) (chl '()) (ch (read-char port)))
	(simple-format (current-error-port) " loop ~S ~S\n" ch ix)
	(cond
	 ((eof-object? ch) (unread-chars-1 chl port) #f)
	 ((= ix n) (unread-char ch port) #t)
	 ((char=? ch (string-ref keyword ix))
	  (loop (1+ ix) (cons ch chl) (read-char port)))
	 (else (unread-chars chl port) #f))))))

(define slurp-lang? (make-keyword-reader "#lang"))

(define (lang-from-port port)
  (cond
   ((slurp-lang? port)
    (skip-whitespace port)
    (let ((lang (next-word port)))
      (simple-format #t "lang=~S\n" lang)
      (let skip-line ((ch (read-char port)))
	(cond
	 ((eof-object? ch))
	 ((char=? #\newline ch))
	 (else (skip-line (read-char)))))
      (string->symbol lang)))
   (else #f)))

(define %file-extension-map
  '(("scm" . scheme)
    ("js" . nx-javascript)
    ("m" . nx-matlab)
    ("el" . elisp)
    ("js" . ecmascript)))

(define* (lang-from-file file)
  (let* ((ix (string-rindex file #\.))
	(ext (and ix (substring file (1+ ix)))))
    (and ext (assoc-ref %file-extension-map ext))))

;; === compile.scm ================

;; call-once

(define call-with-output-file/atomic
  (@@ (system base compile) call-with-output-file/atomic))

(define ensure-language
  (@@ (system base compile) ensure-language))

(define ensure-directory
  (@@ (system base compile) ensure-directory))

(define compiled-file-name
  (@@ (system base compile) compiled-file-name))

(define* (compile-file file #:key
			(output-file #f)
			(from #f)
			(to 'bytecode)
			(env #f)
			(opts '())
			(canonicalization 'relative))
  (simple-format (current-error-port) "comile-file\n")
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (let* ((comp (or output-file (compiled-file-name file)
                     (error "failed to create path for auto-compiled file"
                            file)))
           (in (open-input-file file))
           (enc (file-encoding in)))
      ;; Choose the input encoding deterministically.
      (set-port-encoding! in (or enc "UTF-8"))

      (ensure-directory (dirname comp))
      (call-with-output-file/atomic comp
	(lambda (port)
	  (let* ((from (or from
			   (lang-from-port in)
			   (lang-from-file file)
			   (current-language)))
		 (env (or env (default-environment from))))
	    ((language-printer (ensure-language to))
	     (read-and-compile in #:env env #:from from #:to to #:opts
			       (cons* #:to-file? #t opts))
	     port)))
	  file)
	comp)))

(define* (compile-and-load file #:key (from (current-language)) (to 'value)
                           (env (current-module)) (opts '())
                           (canonicalization 'relative))
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (read-and-compile (open-input-file file)
                      #:from from #:to to #:opts opts
                      #:env env)))

(define compile-passes
  (@@ (system base compile) compile-passes))

(define compile-fold
  (@@ (system base compile) compile-fold))

(define find-language-joint
  (@@ (system base compile) find-language-joint))

(define default-language-joiner
  (@@ (system base compile) default-language-joiner))

(define read-and-parse
  (@@ (system base compile) read-and-parse))

(define* (read-and-compile port #:key
                           (from (current-language))
                           (to 'bytecode)
                           (env (default-environment from))
                           (opts '()))
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let ((joint (find-language-joint from to)))
      (parameterize ((current-language from))
        (let lp ((exps '()) (env #f) (cenv env))
          (let ((x (read-and-parse (current-language) port cenv)))
            (cond
             ((eof-object? x)
              (close-port port)
              (compile ((or (language-joiner joint)
                            (default-language-joiner joint))
                        (reverse exps)
                        env)
                       #:from joint #:to to
                       ;; env can be false if no expressions were read.
                       #:env (or env (default-environment joint))
                       #:opts opts))
             (else
              ;; compile-fold instead of compile so we get the env too
              (receive (jexp jenv jcenv)
                  (compile-fold (compile-passes (current-language) joint opts)
                                x cenv opts)
                (lp (cons jexp exps) jenv jcenv))))))))))

(define* (compile x #:key
                  (from (current-language))
                  (to 'value)
                  (env (default-environment from))
                  (opts '()))

  (let ((warnings (memq #:warnings opts)))
    (if (pair? warnings)
        (let ((warnings (cadr warnings)))
          ;; Sanity-check the requested warnings.
          (for-each (lambda (w)
                      (or (lookup-warning-type w)
                          (warning 'unsupported-warning #f w)))
                    warnings))))

  (receive (exp env cenv)
      (compile-fold (compile-passes from to opts) x env opts)
    exp))


;; decompile-passes

;; decompile-fold

;; decompile

;; === boot-9.scm ================

(define* (load-in-vicinity dir file-name #:optional reader)
  "Load source file FILE-NAME in vicinity of directory DIR.  Use a
pre-compiled version of FILE-NAME when available, and auto-compile one
when none is available, reading FILE-NAME with READER."

  ;; The auto-compilation code will residualize a .go file in the cache
  ;; dir: by default, $HOME/.cache/guile/2.0/ccache/PATH.go.  This
  ;; function determines the PATH to use as a key into the compilation
  ;; cache.
  (define (canonical->suffix canon)
    (cond
     ((and (not (string-null? canon))
           (file-name-separator? (string-ref canon 0)))
      canon)
     ((and (eq? (system-file-name-convention) 'windows)
           (absolute-file-name? canon))
      ;; An absolute file name that doesn't start with a separator
      ;; starts with a drive component.  Transform the drive component
      ;; to a file name element:  c:\foo -> \c\foo.
      (string-append file-name-separator-string
                     (substring canon 0 1)
                     (substring canon 2)))
     (else canon)))

  (define compiled-extension
    ;; File name extension of compiled files.
    (cond ((or (null? %load-compiled-extensions)
               (string-null? (car %load-compiled-extensions)))
           (warn "invalid %load-compiled-extensions"
                 %load-compiled-extensions)
           ".go")
          (else (car %load-compiled-extensions))))

  (define (more-recent? stat1 stat2)
    ;; Return #t when STAT1 has an mtime greater than that of STAT2.
    (or (> (stat:mtime stat1) (stat:mtime stat2))
        (and (= (stat:mtime stat1) (stat:mtime stat2))
             (>= (stat:mtimensec stat1)
                 (stat:mtimensec stat2)))))

  (define (fallback-file-name canon-file-name)
    ;; Return the in-cache compiled file name for source file
    ;; CANON-FILE-NAME.

    ;; FIXME: would probably be better just to append
    ;; SHA1(canon-file-name) to the %compile-fallback-path, to avoid
    ;; deep directory stats.
    (and %compile-fallback-path
         (string-append %compile-fallback-path
                        (canonical->suffix canon-file-name)
                        compiled-extension)))

  (define (compile file)
    ;; Compile source FILE, lazily loading the compiler.
    ((module-ref (resolve-interface '(system base compile))
                 'compile-file)
     file
     #:opts %auto-compilation-options
     #:env (current-module)))

  (define (load-thunk-from-file file)
    (let ((loader (resolve-interface '(system vm loader))))
      ((module-ref loader 'load-thunk-from-file) file)))

  ;; Returns a thunk loaded from the .go file corresponding to `name'.
  ;; Does not search load paths, only the fallback path.  If the .go
  ;; file is missing or out of date, and auto-compilation is enabled,
  ;; will try auto-compilation, just as primitive-load-path does
  ;; internally.  primitive-load is unaffected.  Returns #f if
  ;; auto-compilation failed or was disabled.
  ;;
  ;; NB: Unless we need to compile the file, this function should not
  ;; cause (system base compile) to be loaded up.  For that reason
  ;; compiled-file-name partially duplicates functionality from (system
  ;; base compile).

  (define (fresh-compiled-thunk name scmstat go-file-name)
    ;; Return GO-FILE-NAME after making sure that it contains a freshly
    ;; compiled version of source file NAME with stat SCMSTAT; return #f
    ;; on failure.
    (false-if-exception
     (let ((gostat (and (not %fresh-auto-compile)
                        (stat go-file-name #f))))
       (if (and gostat (more-recent? gostat scmstat))
           (load-thunk-from-file go-file-name)
           (begin
             (when gostat
               (format (current-warning-port)
                       ";;; note: source file ~a\n;;;       newer than compiled ~a\n"
                       name go-file-name))
             (cond
              (%load-should-auto-compile
               (%warn-auto-compilation-enabled)
               (format (current-warning-port) ";;; compiling ~a\n" name)
               (let ((cfn (compile name)))
                 (format (current-warning-port) ";;; compiled ~a\n" cfn)
                 (load-thunk-from-file cfn)))
              (else #f)))))
     #:warning "WARNING: compilation of ~a failed:\n" name))

  (define (sans-extension file)
    (let ((dot (string-rindex file #\.)))
      (if dot
          (substring file 0 dot)
          file)))

  (define (load-absolute abs-file-name)
    ;; Load from ABS-FILE-NAME, using a compiled file or auto-compiling
    ;; if needed.
    (define scmstat
      (false-if-exception
       (stat abs-file-name)
       #:warning "Stat of ~a failed:\n" abs-file-name))

    (define (pre-compiled)
      (or-map
       (lambda (dir)
         (or-map
          (lambda (ext)
            (let ((candidate (string-append (in-vicinity dir file-name) ext)))
              (let ((gostat (stat candidate #f)))
                (and gostat
                     (more-recent? gostat scmstat)
                     (false-if-exception
                      (load-thunk-from-file candidate)
                      #:warning "WARNING: failed to load compiled file ~a:\n"
                      candidate)))))
          %load-compiled-extensions))
       %load-compiled-path))

    (define (fallback)
      (and=> (false-if-exception (canonicalize-path abs-file-name))
             (lambda (canon)
               (and=> (fallback-file-name canon)
                      (lambda (go-file-name)
                        (fresh-compiled-thunk abs-file-name
                                              scmstat
                                              go-file-name))))))

    (let ((compiled (and scmstat
                         (or (and (not %fresh-auto-compile)
                                  (pre-compiled))
                             (fallback)))))
      (if compiled
          (begin
            (if %load-hook
                (%load-hook abs-file-name))
            (compiled))
          (start-stack 'load-stack
                       (primitive-load abs-file-name)))))

  (save-module-excursion
   (lambda ()
     (with-fluids ((current-reader reader)
                   (%file-port-name-canonicalization 'relative))
       (cond
        ((absolute-file-name? file-name)
         (load-absolute file-name))
        ((absolute-file-name? dir)
         (load-absolute (in-vicinity dir file-name)))
        (else
         (load-from-path (in-vicinity dir file-name))))))))

(define-syntax nx-load
  (make-variable-transformer
   (lambda (x)
     (let* ((src (syntax-source x))
            (file (and src (assq-ref src 'filename)))
            (dir (and (string? file) (dirname file))))
       (syntax-case x ()
         ((_ arg ...)
          #`(load-in-vicinity #,(or dir #'(getcwd)) arg ...))
         (id
          (identifier? #'id)
          #`(lambda args
              (apply load-in-vicinity #,(or dir #'(getcwd)) args))))))))

;; === nx-load =======================

(define nx-compile-file compile-file)
(define nx-compile-and-load compile-and-load)

;; Local Variables:
;; eval: (put 'call-with-output-file/atomic 'scheme-indent-function 1)
;; End:
;; --- last line ---
