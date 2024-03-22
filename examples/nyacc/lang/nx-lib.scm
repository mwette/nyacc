;; nyacc/lang/nx-util.scm - run-time library

;; Copyright (C) 2018,2024 Matthew R. Wette
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

;;; Description:

;; This module provide run-time procecures for the NYACC extension (nx)
;; languages.  The intent is to provide a consistent data model between
;; nx languages so that they can inter-operate.

;;; Notes:

;; For OO languages we should use single-inheritance with interfaces.
;; An object is a hash table with entries for data and either
;;   A) a single class (type) entry, or
;;   B) one class (type) entry and N interface entries
;; This module will need to provide run-time type determination.  Well,
;; We need a procedure (obj-call obj name args)
;;
;; Idea: for each object add a lang-specific field to decorate
;; (hashq-ref* obj 'lang 'nx-javascript) => ...
;;
;; Feeling: in scheme (lisp) there is really no strong distinction between
;; an integer, float and a procedure.  They are all data.

;; @subheading Object Architecture
;; The principles are
;; @itemize
;; @item campatability among many languages important
;; @item strict language adherence is not priority
;; @item base Scheme compatiblity is priority
;; @item speed is not high priority
;; @end itemize

;; Tcl arrays are ordered -- alist?
;; Tcl dicts are unordered

;;; Todos:

;;  1) add traits (aka interfaces)

;;; Code:

(define-module (nyacc lang nx-lib)
  #:export (nx-get-method
            nx-use-module
            nx-C-predicate
            nx-undefined
            ;;
            make-nx-hash-table nx-hash-ref nx-hash-set!
            nx-hash-add-lang nx-hash-lang-ref nx-hash-lang-set! %nx-lang-key
            mat-disp vec-disp
            ;;
            install-inline-language-evaluator
            uninstall-inline-language-evaluator)
  #:use-module (nyacc lang nx-printf))

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(use-modules (ice-9 pretty-print))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

(define nx-undefined (if #f #f))

;; @deffn {Procedure} nx-get-method obj name
;; find a
;; @end deffn
(define (nx-get-method obj name)
  #f)

(define fooo 1)

;;; hash tables

;; maybe this should be a Guile (record) type

;; This is guile hash table with v keys
;; The hash table has a {lang} key to another hash table.
;; each language gets an entry in the lang entry so ...

(define %nx-lang-key '{nx-lang})

(define (nx-hash-add-lang htab lang)
  (unless (hashq-ref htab %nx-lang-key)
    (hashv-set! htab %nx-lang-key (make-hash-table 7)))
  (let ((ltab (hashv-ref htab '%nx-lang-key)))
    (hashv-set! ltab lang (make-hash-table 7))))

(define (nx-hash-lang-ref htab lang key)
  (let ((ltab (hashv-ref (hashv-ref htab %nx-lang-key) lang)))
    (hashv-ref ltab key)))

(define (nx-hash-lang-set! htab lang key val)
  (let ((ltab (hashv-ref (hashv-ref htab %nx-lang-key) lang)))
    (hashv-set! ltab key val)))

(define* (make-nx-hash-table #:optional (n 31) #:key (lang #f))
  (let ((htab (make-hash-table n)))
    (if lang (nx-hash-add-lang htab lang))
    htab))

(define (nx-hash-ref htab key)
  (hashq-ref htab key))

(define (nx-hash-set! htab key val)
  (hashq-set! htab key val))

;; ====================================
;; code to be organized

;; routine to make a single arg predicate into a zero? predcate

;; ???
(define (nx-C-predicate proc)
  (lambda (arg)
    (let ((res (proc arg)))
      (if res 1 0))))

(use-modules (ice-9 match))
(define (add-src-prop-attr sx)
  (define (p2l pair) (list (car pair) (cdr pair)))
  (define (ins-src-prop sx attr)
    (cons (map p2l (source-properties sx)) attr))
  (match sx
    (`(,tag (@ . ,attr) . ,rest)
     `(tag (@ . ,(ins-src-prop sx attr)) . ,(map add-src-prop-attr rest)))
    (`(,tag . ,rest)
     (let ((src-prop (source-properties sx)))
       (if (pair? src-prop)
	   `(,tag (@ . ,(map p2l src-prop)) . ,(map add-src-prop-attr rest))
	   `(,tag . ,(map add-src-prop-attr rest)))))
    (_ sx)))
(export add-src-prop-attr)

(define (nx-error fmt . args)
  (throw 'error (apply simple-format #f fmt args)))

(define (nx-use-module path)
  (module-use! (current-module) (resolve-interface path)))

;; === matrices and vectors ========

(define (mat-disp/strict array port format)
  (let* ((conv (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-format-string format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (do ((j 0 (1+ j))) ((= j (list-ref dimz 1)))
        (display " " port)
        (apply-fmt port spec (array-ref array i j)))
      (newline port))))

(define* (mat-disp array #:optional (port #t) (format "%12.5e"))
  (cond
   ((eq? #f port)
    (let ((strp (open-output-string)))
      (mat-disp/strict array strp format)
      (get-output-string strp)))
   ((eq? #t port)
    (mat-disp/strict array (current-output-port) format))
   (else
    (mat-disp/strict array port format))))

(define (vec-disp/strict array port format)
  (let* ((conv (array-type array))
         (dims (array-dimensions array))
         (dimz (map (lambda (i) (min i 8)) dims))
         (spec (parse-format-string format)))
    (do ((i 0 (1+ i))) ((= i (list-ref dimz 0)))
      (display " " port)
      (apply-fmt port spec (array-ref array i))
      (newline port))))

(define* (vec-disp array #:optional (port #t) (format "%12.5e"))
  (cond
   ((eq? #f port)
    (let ((strp (open-output-string)))
      (vec-disp/strict array strp format)
      (get-output-string strp)))
   ((eq? #t port)
    (vec-disp/strict array (current-output-port) format))
   (else
    (vec-disp/strict array port format))))

;;; === in-line reading =========================================================

(use-modules (system base language))
(use-modules (system base compile))
(use-modules (language tree-il))

;; @deffn {Procedure} read-inline-code read-char port
;; @example
;; scheme@(guile-user)> (define x #<nx-mlang: [1, 2]; >#)
;; @end example
;; @noindent
;; This executes code like it was written on the command line.
;; So the above is equivalent to:
;; @example
;; scheme@(guile-user)> ,L nx-octave
;; nx-octave@(guile-user)> [1, 2];
;; $1 = #(1 2)
;; nx-octave@(guile-user)> ,L scheme
;; scheme@(guile-user)> (define a $1)
;; @end example
;; @end deffn
(define (read-inline-code reader-char port)
  (let* ((str-port (open-output-string))
         (name (let loop ((chl '()) (ch (read-char port)))
                 (cond
                  ((eof-object? ch) ch)
                  ((char=? ch #\:) (reverse-list->string chl))
                  (else (loop (cons ch chl) (read-char port))))))
         (code (let loop ((ch (read-char port)))
                 (cond
                  ((eof-object? ch) (error "oops"))
                  ((char=? ch #\>)
                   (let ((ch1 (read-char port)))
                     (cond
                      ((eof-object? ch) (error "oops"))
                      ((char=? ch1 #\#)
                       (display "\n" str-port)
                       (get-output-string str-port))
                      (else (display ch str-port) (loop ch1)))))
                  (else
                   (display ch str-port)
                   (loop (read-char port))))))
         ;;
         (lang (lookup-language (string->symbol name)))
         (lread (and lang (language-reader lang)))
         (lcomp (and lang (assq-ref (language-compilers lang) 'tree-il)))
         ;;
         (sxml (and lread
                    (call-with-input-string code
                      (lambda (port) (lread port (current-module))))))
         (itil (and lcomp
                    (call-with-values
                        (lambda () (lcomp sxml (current-module) '()))
                      (lambda (exp env cenv) exp))))
         (xtil (unparse-tree-il itil))
         (scm (decompile itil))
         )
    (unless lang (error "no such language:" name))
    scm))

;; @deffn {Procedure} install-inline-language-evaluator
;; Install the extension language reader macro @code{#<} ... @code{>#}.
;; This reader macro will evaluate statements in extension languages, which
;; often have expression statements can return a value.  Here is an example:
;; @example
;; scheme@@(guile-user)> (define a 1)
;; scheme@@(guile-user)> (define b #<ecmascript: a + 10; >#)
;; scheme@@(guile-user)> b
;; $1 = 11
;; @end example
;; @end deffn
(define (install-inline-language-evaluator)
  "- Procedure: install-inline-language-evaluator
     Install the extension language reader macro '#<' ...  '>#'.  This
     reader macro will evaluate statements in extension languages, which
     often have expression statements can return a value.  Here is an
     example:
          scheme@(guile-user)> (define a 1)
          scheme@(guile-user)> (define b #<ecmascript: a + 10; >#)
          scheme@(guile-user)> b
          $1 = 11"
  (read-hash-extend #\< read-inline-code)
  (if #f #f))

;; @deffn {Procedure} uninstall-inline-language-evaluator
;; Clear the reader macro @code{#<}.
;; @end deffn
(define (uninstall-inline-language-evaluator)
  "- Procedure: uninstall-inline-language-evaluator
     Clear the reader macro '#<'."
  (read-hash-extend #\< #f)
  (if #f #f))

;; --- last line ---
