;;; nyacc/lang/julia/julia.scm - parsing 

;; Copyright (C) 2021,2025 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (nyacc lang julia parser)
  #:export (parse-julia read-julia-file)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (sxml fold)
  #:use-module ((srfi srfi-1) #:select (fold)))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;;; === lexical analyzer

(define julia-read-comm
  (make-comm-reader '(("#" . "\n"))))

(define (elipsis? ch)
  (if (eqv? ch #\.)
      (let ((c1 (read-char)))
        (if (eqv? c1 #\.)
            (let ((c2 (read-char)))
              (if (eqv? c2 #\.)
                  (cons (string->symbol "...") "...")
                  (begin (unread-char c2) (unread-char c1) #f)))
            (begin (unread-char c1) #f)))
      #f))

(define (skip-to-next-line)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch) ch)
     ((eqv? ch #\newline) (read-char))
     (else (loop (read-char))))))

(define space-cs (string->char-set " \t"))

(define (flush-ws)
  (let loop ((ch (read-char)))
    (cond
     ((eof-object? ch))
     ((char-set-contains? space-cs ch) (loop (read-char)))
     (else (unread-char ch)))))

;; @deffn {Procedure} make-julia-lexer-generator match-table
;; This function, given the @var{match-table} from a lalr-generated
;; machine, generates a procedure that returns lexical analyzers for
;; use in Octave parsers. 
;; @end deffn
(define-public (make-julia-lexer-generator match-table)
  ;; There is some trickery here to assure that if the last line
  ;; ends w/o newline then one gets inserted.
  (sferr "TODO: line continuation seq: ;;, also matrix term see p. 275-276\n")
  (let* ((read-string read-c-string)
	 (read-comm julia-read-comm)
	 (read-ident read-c$-ident)
	 (space-cs (string->char-set " \t\r\f"))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt like-c$-ident? strtab)) ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab)) ; keywords in grammar
	 (chrseq (remove-mt like-c-ident? strtab)) ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	; characters in grammar
	 (read-chseq (make-chseq-reader chrseq))
         (nl-val (assoc-ref match-table "\n"))
         (sp-val (assoc-ref match-table 'sp))
	 (assc-$
          (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (if (not nl-val) (error "julia setup error"))
    (if (not sp-val) (error "julia setup error"))
    (lambda ()
      (let ((bol #t))
	(define (loop ch)
	  (cond
	   ((eof-object? ch) (assc-$ (cons '$end ch)))
 	   ;;((elipsis? ch) (loop (skip-to-next-line)))
	   ((eqv? ch #\newline) (set! bol #t) (cons nl-val "\n"))
	   ((char-set-contains? space-cs ch) (flush-ws) (cons sp-val " "))
	   ((read-comm ch bol) => (lambda (p) (set! bol #f) (assc-$ p)))
	   (bol (set! bol #f) (loop ch))
	   ((read-ident ch) =>
	    (lambda (s) ;; s is a string
	      (or (and=> (assq-ref keytab (string->symbol s))
		    (lambda (tval) (cons tval s)))
		  (assc-$ (cons '$ident s)))))
	   ((read-c-num ch) => (lambda (p) (assc-$ p)))
	   ((char=? ch #\") (assc-$ (read-string ch)))
	   ;;((char=? ch #\') (read-chseq ch)))
	   ((read-chseq ch) => identity)
	   ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	   (else (cons ch (string ch)))))
        (lambda ()
          (let* ((lxm (loop (read-char)))
                 (port (current-input-port))
                 (file (port-filename port))
                 (line (1+ (port-line port)))
                 (props `((filename . ,file) (line . ,line) (column . 0))))
            (set-source-properties! lxm props)
            lxm))))))

(define (apply-julia-statics sx) sx)

;; === file parser 

(include-from-path "nyacc/lang/julia/mach.d/julia-tab.scm")
(include-from-path "nyacc/lang/julia/mach.d/julia-act.scm")

(define gen-julia-lexer (make-julia-lexer-generator julia-mtab))

;; Parse given a token generator.
(define raw-parser
  (make-lalr-parser
   (acons 'act-v julia-act-v julia-tables)
   #:skip-if-unexp '($code-comm $lone-comm sp "\n")))

(define* (parse-julia #:key debug)
  (catch
   'nyacc-error
   (lambda ()
     (apply-julia-statics
      (raw-parser (gen-julia-lexer) #:debug debug)))
   (lambda (key fmt . args)
     (apply simple-format (current-error-port) fmt args)
     (newline (current-error-port))
     #f)))

(define (read-julia-file port env)
  (with-input-from-port port
    (lambda ()
      (if (eof-object? (peek-char port))
	  (read-char port)
	  (parse-julia #:debug #t)))))

;; --- last line ---
