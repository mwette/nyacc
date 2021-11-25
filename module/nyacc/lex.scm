;;; nyacc/lex.scm

;; Copyright (C) 2015-2021 - Matthew R.Wette
;; 
;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; A module providing procedures for constructing lexical analyzers.

;; '$fixed '$float '$string '$ident '$chlit '$chlit/L '$chlit/u '$chlit/U

;; todo: change lexer to return @code{cons-source} instead of @code{cons}
;; todo: to be fully compliant, C readers need to deal with \ at end of line

;; todo: figure out what readers return atoms and which pairs
;; tokens: read-c-ident 
;; pairs: num-reader read-c-num read-c-string
;; issue: if returning pairs we need this for hashed parsers:
;;    (define (assc-$ pair) (cons (assq-ref symbols (car pair)) (cdr pair)))
;; read-comm changed to (read-comm ch bol) where bol is begin-of-line cond
;; 
;; read-c-ident 

;;; Code:

(define-module (nyacc lex)
  #:export (make-lexer-generator
	    make-ident-reader make-ident-keyword-reader
	    make-comm-reader
	    make-string-reader
	    make-chseq-reader
	    eval-reader
	    read-basic-num
 	    read-c-ident read-c$-ident
 	    read-c-comm
	    read-c-string
	    read-c-chlit
	    read-c-num
	    read-oct read-hex
	    like-c-ident? like-c$-ident?
	    c-escape
	    cnumstr->scm
	    filter-mt remove-mt map-mt make-ident-like-p
	    c:ws c:if c:ir)
  #:use-module ((srfi srfi-1) #:select (remove append-reverse)))

(define (sf fmt . args) (apply simple-format #t fmt args))
  
;; @section Constructing Lexical Analyzers
;; The @code{lex} module provides a set of procedures to build lexical
;; analyzers.  The approach is to first build a set of @defn{readers} for 
;; MORE TO COME
;;
;; Readers are procecures that take one character (presumably from the
;; current-input-port) and determine try to make a match.   If a match is
;; made something is returned, with any lookaheads pushed back into the
;; input port.  If no match is made @code{#f} is returned and the input
;; argument is still the character to work on.
;;
;; Here are the procedures used:
;; @table @code

(define digit "0123456789")
(define ucase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define lcase "abcdefghijklmnopqrstuvwxyz")

;; C lexemes are popular so include those.
;;(define c:ws (list->char-set '(#\space #\tab #\newline #\return )))
(define c:ws char-set:whitespace)
(define c:if (let ((cs (char-set #\_)))	; ident, first char
	       (string->char-set! ucase cs)
	       (string->char-set! lcase cs)))
(define c:ir (string->char-set digit c:if)) ; ident, rest chars
(define c:nx (string->char-set "eE"))	    ; number exponent
(define c:hx (string->char-set "abcdefABCDEF"))
(define c:sx (string->char-set "lLuU")) ; fixed suffix
(define c:fx (string->char-set "fFlL")) ; float suffix
(define c:px (string->char-set "rRhHkKlLuU")) ; fixed-point suffix
(define c:bx (string->char-set "pP"))	; binary float suffix
(define c:cx (string->char-set "LuU"))	; char prefix

(define char-hex?
  (let ((cs (string->char-set "0123456789abcdefABCDEF")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define char-oct? 
  (let ((cs (string->char-set "01234567")))
    (lambda (ch)
      (char-set-contains? cs ch))))

(define lxlsr reverse-list->string)
(define rls reverse-list->string)	; reverse-list->string

;; @deffn {Procedure} eval-reader reader string => result
;; For test and debug, this procedure will evaluate a reader on a string.
;; A reader is a procedure that accepts a single character argument intended
;; to match a specific character sequence.  A reader will read more characters
;; by evaluating @code{read-char} until it matches or fails.  If it fails, it
;; will pushback all characters read via @code{read-char} and return @code{#f}.
;; If it succeeds the input pointer will be at the position following the
;; last matched character.
;; @end deffn
(define (eval-reader reader string)
  (with-input-from-string string
    (lambda () (reader (read-char)))))

;; @deffn {Procedure} make-space-skipper chset => proc
;; This routine will generate a reader to skip whitespace.
;; @end deffn
(define (make-space-skipper chset)
  (lambda (ch)
    (if (char-set-contains? chset ch)
	(let loop ((ch (read-char)))
	  (cond
	   ((char-set-contains? chset ch)
	    (loop (read-char)))
	   (else
	    (unread-char ch)
	    #t)))
	#f)))
	 
;; @deffn {Procedure} skip-c-space ch => #f|#t
;; If @code{ch} is space, skip all spaces, then return @code{#t}, else
;; return @code{#f}.
;; @end deffn
(define skip-c-space (make-space-skipper c:ws))


;; @deffn {Procedure} make-ident-reader cs-first cs-rest => ch -> #f|string
;; For identifiers, given the char-set for first character and the char-set
;; for following characters, return a return a reader for identifiers.
;; The reader takes a character as input and returns @code{#f} or @code{string}.
;; This will generate exception on @code{#<eof>}.
;; @end deffn
(define (make-ident-reader cs-first cs-rest)
  (lambda (ch)
    (cond
     ((eof-object? ch) #f)
     ((char-set-contains? cs-first ch)
      (let loop ((chl (list ch)) (ch (read-char)))
	(cond
	 ((eof-object? ch)
	  (if (null? chl) #f
	      (lxlsr chl)))
	 ((char-set-contains? cs-rest ch)
	  (loop (cons ch chl) (read-char)))
	 (else (unread-char ch)
	       (lxlsr chl)))))
     (else #f))))

;; @deffn {Procedure} make-ident-like-p ident-reader
;; Generate a predicate, from a reader, that determines if a string qualifies
;; as an identifier. 
;; @end deffn
;; Implementation may not be very efficient.
(define (make-ident-like-p reader)
  (lambda (s) (and (string? s)
		   (positive? (string-length s))
		   (eval-reader reader s)
		   #t)))


;; @deffn {Procedure} make-ident-keyword-reader ident-reader match-table [tval]
;; Generate a procedure from an ident reader and a parser match-table
;; that takes a character and returns @code{#f} or a pair for the parser.
;; The pairs are of the form @code{($ident . "abc")} or @code{(if . "if")}.
;; The optional token is @code{'$ident}.
;; @end deffn
(define* (make-ident-keyword-reader ident-reader match-table
				    #:optional (tval '$ident))
  (let ((ident-like? (make-ident-like-p ident-reader))
	(ident-id (assoc-ref match-table tval)))
    (let loop ((kt '()) (mt match-table))
      (if (null? mt)
	  (lambda (ch)
	    (and=> (ident-reader ch)
		   (lambda (s) (cons (or (assoc-ref kt s) ident-id) s))))
	  (loop (if (ident-like? (caar mt)) (cons (car mt) kt) kt) (cdr mt))))))
	 
;; @deffn {Procedure} read-c-ident ch => #f|string
;; If ident pointer at following char, else (if #f) ch still last-read.
;; @end deffn
(define read-c-ident (make-ident-reader c:if c:ir))

;; @deffn {Procedure} like-c-ident? ch 
;; Determine if a string qualifies as a C identifier.
;; @end deffn
(define like-c-ident? (make-ident-like-p read-c-ident))

;; @deffn {Procedure} read-c$-ident ch => #f|string
;; Similar to @code{read-c-ident}: it allows initial @code{$}.
;; @end deffn
(define read-c$-ident
  (let ((cs (char-set-copy c:if)))
    (string->char-set! "$" cs)
    (make-ident-reader cs c:ir)))

;; @deffn {Procedure} like-c$-ident? ch 
;; Similar to @code{like-c-ident}: it allows initial @code{$}.
;; @end deffn
(define like-c$-ident? (make-ident-like-p read-c$-ident))

;; @deffn {Procedure} make-string-reader delim [tval]
;; Generate a reader for strings for delimiter.  @var{delim} can be a char
;; or a list.  Closing quote escaped with @code{\} is skipped.
;; @example
;; (make-string-reader '((#\" . #\") (#\{ #\})))
;; @end example
;; Pretty printers should be set up to stuff things back.
;; @end deffn
(define* (make-string-reader delim #:optional (tval '$string))
  (define (fail) (throw 'nyacc-error "eof reading string"))
  (define (doit nd)
    (let loop ((cl '()) (ch (read-char)))
      (cond ((eof-object? ch) (fail))
	    ((char=? ch #\\)
	     (let ((c1 (read-char)))
	       (cond
		((eof-object? c1) fail)
		;;((char=? c1 #\t) (loop (cons #\tab cl) (read-char)))
		;;((char=? c1 #\n) (loop (cons #\newline cl) (read-char)))
		;;((char=? c1 #\r) (loop (cons #\return cl) (read-char)))
		(else (loop (cons* c1 #\\ cl) (read-char))))))
	    ((char=? ch nd) (if tval (cons tval (rls cl)) (rls cl)))
	    (else (loop (cons ch cl) (read-char))))))
  (if (char? delim)
      (lambda (ch) (and (char=? ch delim) (doit ch)))
      (lambda (ch) (and=> (assq-ref delim ch) doit))))
;;(export new-make-string-reader)
;; string-literal -> parser -> pretty-print => string-literal

;; @deffn {Procedure} read-oct => 123|#f
;; Read octal number, assuming @code{\0} have already been read.
;; Return integer.
;; @end deffn
(define read-oct
  (let ((cs:oct (string->char-set "01234567")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
	(cond
	 ((eof-object? ch) cv)
	 ;;((> n 3) (unread-char ch) cv)
	 ((char-set-contains? cs:oct ch)
	  (loop (+ (* 8 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
	 (else (unread-char ch) cv))))))

;; @deffn {Procedure} read-hex => 123|#f
;; Read hex number.  Assumes prefix (e.g., "0x" has already been read).
;; Returns integer.
;; @end deffn
(define read-hex
  (let ((cs:dig (string->char-set "0123456789"))
	(cs:uhx (string->char-set "ABCDEF"))
	(cs:lhx (string->char-set "abcdef")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
	(cond
	 ((eof-object? ch) cv)
	 ;;((> n 2) (unread-char ch) cv)
	 ((char-set-contains? cs:dig ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
	 ((char-set-contains? cs:uhx ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 55)) (read-char) (1+ n)))
	 ((char-set-contains? cs:lhx ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 87)) (read-char) (1+ n)))
	 (else (unread-char ch) cv))))))

;; @deffn {Procedure} c-escape seed
;; After @code{\\} in a C string, read the rest of the sequence and cons
;; the character, if exists, with the seed (a list).  Remember that @code{\n}
;; should, and will, just return the seed.
;; @end deffn
(define (c-escape seed)
  (let* ((ch (read-char)))
    (case ch
      ((#\newline) seed)
      ((#\\) (cons #\\ seed))
      ((#\") (cons #\" seed))
      ((#\') (cons #\' seed))
      ((#\n) (cons #\newline seed))
      ((#\r) (cons #\return seed))
      ((#\b) (cons #\bs seed))
      ((#\t) (cons #\tab seed))
      ((#\f) (cons #\page seed))
      ((#\a) (cons #\bel seed))	      ; guile 1.8 doesn't know #\alarm
      ((#\v) (cons #\vt seed))	      ; guile 1.8 doesn't know #\vtab
      ((#\0) (cons (integer->char (read-oct)) seed))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (unread-char ch) (cons (integer->char (read-oct)) seed))
      ((#\x) (cons (integer->char (read-hex)) seed))
      (else (cons ch seed)))))

;; @deffn {Procedure} read-c-string ch => ($string . "foo")
;; Read a C-code string.  Output to code is @code{write} not @code{display}.
;; Return #f if @var{ch} is not @code{"}. @*
;; TODO: parse trigraphs
;; ??=->#, ??/->\, ??'->^, ??(->[, ??)->], ??~->|, ??<->{, ??>->}, ??-->~
;; and digraphs <:->[, :>->], <%->{ %>->} %:->#
;; @end deffn
(define* (read-c-string ch #:optional (tval '$string))
  (if (not (eq? ch #\")) #f
      (let loop ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\) (loop (c-escape cl) (read-char)))
	      ((eq? ch #\") (if tval (cons tval (rls cl)) (rls cl)))
	      (else (loop (cons ch cl) (read-char)))))))

;; @deffn {Procedure} make-chlit-reader
;; Generate a reader for character literals. NOT DONE.
;; For C, this reads @code{'c'} or @code{'\n'}.
;; @end deffn
(define (make-chlit-reader . rest) (error "NOT IMPLEMENTED"))

;; @deffn {Procedure} read-c-chlit ch
;; @example
;; ... 'c' ... => (read-c-chlit #\') => '($chlit . #\c)
;; @end example
;; This will return @code{$chlit}, $code{$chlit/L} for @code{wchar_t},
;; @code{$chlit/u} for @code{char16_t}, or @code{$chlit/U} for @code{char32_t}.
;; @end deffn
(define (read-c-chlit ch)
  (define (read-esc-char)
    (let ((c2 (read-char)))
      (case c2
	((#\t) "\t")		   ; horizontal tab U+0009
	((#\n) "\n")		   ; newline U+000A
	((#\v) "\v")		   ; verticle tab U+000B
	((#\f) "\f")		   ; formfeed U+000C
	((#\r) "\r")		   ; return U+000D
	((#\a) "\x07")		   ; alert U+0007
	((#\b) "\x08")		   ; backspace U+0008 not in guile 1.8
	((#\0) (string (integer->char (read-oct)))) ; octal
	((#\1 #\2 #\3 #\4 #\5 #\6 #\7)		    ; octal
	 (unread-char c2) (string (integer->char (read-oct))))
	((#\x) (string (integer->char (read-hex)))) ; hex
	((#\\ #\' #\" #\? #\|) (string c2))
	(else (error "bad escape sequence" c2)))))
  (define (wchar t)
    (case t ((#\L) '$chlit/L) ((#\u) '$chlit/u) ((#\U) '$chlit/U)))
  (cond
   ((char=? ch #\')
    (let* ((c1 (read-char))
	   (sc (if (eqv? c1 #\\) (read-esc-char) (string c1))))
      (if (not (char=? #\' (read-char)))
	  (throw 'nyacc-error "read-c-chlit: bad char literal"))
      (cons '$chlit sc)))
   ((char-set-contains? c:cx ch)
    (let ((c1 (read-char)))
      (cond
       ((eof-object? c1) #f)
       ((char=? c1 #\') (cons (wchar ch) (cdr (read-c-chlit c1))))
       (else (unread-char c1) #f))))
   (else #f)))

(define (fix-dot l) (if (char=? #\. (car l)) (cons #\0 l) l))

;; @deffn {Procedure} read-basic-num ch [#:signed #t] => #f|pair
;; Reader for numbers in most C-like languages.  The @var{pair} returned
;; is one of @code{($fixed . "1")} or @code{($float . "1.0")}.
;; The @code{fixed} form include decimal, hexidecimal (starting with "0x"),
;; and binary (starting with "0b") integers.  The @code{float} form is
;; floating point.  This will not generate exceptions.  Normally this
;; reads unsigned numbers; with the @code{#:signed #t} option, leading
;; @code{-} is allowed.
;; @end deffn
(define* (read-basic-num ch #:key signed)
  (define (unread-chl chl)
    (let lp ((l chl)) (unless (null? l) (unread-char (car l)) (lp (cdr l)))))
  (let loop ((chl '()) (ty #f) (st 10) (ch ch))
    ;;(sf "lp: ch=~S st=~S ty=~S chl=~S\n" ch st ty chl)
    (case st
      ((10) ;; entry
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char))) 
	((char-numeric? ch) (loop chl '$fixed 20 ch))
	((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char)))
	((and signed (char=? #\- ch)) (loop (cons ch chl) ty 13 (read-char)))
	(else #f)))
      ((11) ;; got 0/fixed, allow x, b
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? ch #\.) (loop (cons ch chl) '$float 30 (read-char)))
	((memq ch '(#\X #\x)) (loop (cons ch chl) ty 21 (read-char)))
	((memq ch '(#\B #\b)) (loop (cons ch chl) ty 22 (read-char)))
	((char-oct? ch) (loop (cons ch chl) ty 23 (read-char)))
	(else (loop chl ty 70 ch))))
      ((12) ;; got `.'
       (cond
	((eof-object? ch) #f)
	((char-numeric? ch) (loop (cons ch chl) '$float 30 (read-char)))
	(else (unread-char ch) #f)))
      ((13) ;; got '-'
       (cond
	((eof-object? ch) (unread-char #\-) #f)
	((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char)))
	((char-numeric? ch) (loop chl '$fixed 20 ch))
	((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char)))
	(else (loop (cons ch chl) ty 73 ch))))
      ((20) ;; parse decimal integer part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 20 (read-char)))
	((char=? ch #\.) (loop (cons #\. chl) '$float 30 (read-char)))
	((memq ch '(#\E #\e)) (loop (cons ch chl) '$float 40 (read-char)))
	(else (loop chl '$fixed 70 ch))))
      ((21) ;; parse fixed hex
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-hex? ch) (loop (cons ch chl) ty 21 (read-char)))
	(else (loop chl ty 70 ch))))
      ((22) ;; parse fixed octal	      
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
	(else (loop chl ty 70 ch))))
      ((23) ;; parse fixed binary 
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\0 #\1)) (loop (cons ch chl) ty 11 (read-char)))
	(else (loop chl ty 70 ch))))
      ((30) ;; parse float fractional part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 30 (read-char)))
	((memq ch '(#\E #\e)) (loop (cons ch chl) ty 40 (read-char)))
	(else (loop chl ty 70 ch))))
      ((40) ;; have float e|p, look for sign
       (cond
	((eof-object? ch) (loop chl ty 73 ch))
	((memq ch '(#\+ #\-)) (loop (cons ch chl) ty 50 (read-char)))
	(else (loop chl ty 50 ch))))
      ((50) ;; parse rest of exponent
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 50 (read-char)))
	(else (loop chl ty 70 ch))))
      ((70)
       (cond
	((eof-object? ch) (cons ty (rls chl)))
	(else (loop chl ty 71 ch))))
      ((71) (unread-char ch) (cons ty (rls chl)))
      ((72) (cons ty (rls chl)))
      ((73) (unread-chl chl) #f)
      (else (error "coding error")))))

;; @deffn {Procedure} read-c-num ch => #f|pair
;; Reader for numbers in C. Here @evar{pair} is of the form
;; @code{($fixed . "1")}, @code{($float . "1.0")},
;; @code{($fixpt . "1.0r")} or @code{($decfl . "1.0DD")}.
;; The above coorespond to integers (including decimal, hexidecimal,
;; octal and binary), fixed-point, floating point (including the
;; C hex format) and decimal floating point.
;; Binary format integers, with prefix @code{0b}, are not part of
;; C, but commonly supported by compilers.  This routine will generate
;; the exception @code{nyacc-error} for some read errors, e.g., bad
;; prefix.
;; @end deffn
(define (read-c-num ch)
  (define (unread-chl chl)
    (let lp ((chl chl))
      (unless (null? (cdr chl)) (unread-char (car chl)) (lp (cdr chl)))))
  (define (bad-sfx chl)
    (let ((num (rls chl)))
      (unread-chl chl)
      (throw 'nyacc-error "bad suffix on number: ~S" num)))
  (define (bad-xfl chl)
    (let ((num (rls chl)))
      (unread-chl chl)
      (throw 'nyacc-error "hex float requires exponent: ~S" num)))

  ;; chl: char list; ty: type; st: state; ch: next char
  ;; st= 10=beg; 20=int; 30=frac; 40=sgn; 50=exp; 60=sfx 70=end
  (let loop ((chl '()) (ty #f) (st 10) (ch ch))
    ;;(sf "loop st=~S ch=~S ty=~S\n" st ch ty)
    (case st
      ((10) ;; entry
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? #\0 ch) (loop (cons ch chl) '$fixed 11 (read-char))) 
	((char-numeric? ch) (loop chl '$fixed 20 ch))
	((char=? #\. ch) (loop (cons ch chl) ty 12 (read-char))) 
	(else #f)))
      ((11) ;; got 0/fixed, allow x, b
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char=? ch #\.) (loop (cons ch chl) '$float 30 (read-char)))
	((memq ch '(#\X #\x)) (loop (cons ch chl) ty 21 (read-char)))
	((memq ch '(#\B #\b)) (loop (cons ch chl) ty 23 (read-char)))
	((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
	((memq ch '(#\U #\u)) (loop (cons ch chl) ty 61 (read-char)))
	((memq ch '(#\L #\l)) (loop (cons ch chl) ty 63 (read-char)))
	(else (loop chl ty 70 ch))))
      ((12) ;; got `.' only
       (cond
	((eof-object? ch) #f)
	((char-numeric? ch) (loop (cons ch chl) '$float 30 (read-char)))
	(else (unread-char ch) #f)))
      ((20) ;; parse decimal integer part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch)
	 (loop (cons ch chl) ty 20 (read-char)))
	((char=? ch #\.)
	 (loop (cons #\. chl) '$float 30 (read-char)))
	((memq ch '(#\E #\e)) ;; float exponent
	 (loop (cons ch chl) '$float 40 (read-char)))
	((memq ch '(#\U #\u)) ;; fixed suffix start
	 (loop (cons ch chl) '$fixed 61 (read-char)))
	((memq ch '(#\L #\l)) ;; fixed suffix start
	 (loop (cons ch chl) '$fixed 63 (read-char)))
	((memq ch '(#\K #\R #\k #\r)) ;; fixpt suffix
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl '$fixed 70 ch))))
      ((21) ;; parse hex, fixed or float
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-hex? ch) (loop (cons ch chl) ty 21 (read-char)))
	((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
	((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
	((char=? ch #\.) (loop (cons ch chl) '$float 35 (read-char)))
	((memq ch '(#\P #\p)) (loop (cons ch chl) '$float 40 (read-char)))
	(else (loop chl ty 70 ch))))
      ((22) ;; parse fixed octal	      
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-oct? ch) (loop (cons ch chl) ty 22 (read-char)))
	((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
	((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
	(else (loop chl ty 70 ch))))
      ((23) ;; parse fixed binary 
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\0 #\1)) (loop (cons ch chl) ty 23 (read-char)))
	((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixed 61 (read-char)))
	((memq ch '(#\L #\l)) (loop (cons ch chl) '$fixed 63 (read-char)))
	(else (loop chl ty 70 ch))))
      ((30) ;; parse float fractional part
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 30 (read-char)))
	((memq ch '(#\E #\e))
	 (loop (cons ch chl) ty 40 (read-char)))
	((memq ch '(#\F #\f))
	 (loop (cons ch chl) ty 69 (read-char)))
	((memq ch '(#\L #\l))
	 (loop (cons ch chl) ty 31 (read-char)))
	((memq ch '(#\U #\u))
	 (loop (cons ch chl) '$fixpt 66 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	((memq ch '(#\D #\d)) ;; decfl
	 (loop (cons ch chl) ty 68 (read-char)))
	(else (loop chl ty 70 ch))))
      ((31) ;; got l/float, check for fixpt
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ((35) ;; parse float hex fraction, requires exponent
       (cond
	((eof-object? ch) (throw 'nyacc-error "hex float requires exponent"))
	((char-hex? ch) (loop (cons ch chl) ty 35 (read-char)))
	((memq ch '(#\P #\p)) (loop (cons ch chl) ty 40 (read-char)))
	(else (bad-xfl chl))))
      ((40) ;; have float e|p, look for sign
       (cond
	((eof-object? ch) (bad-sfx chl))
	((memq ch '(#\+ #\-)) (loop (cons ch chl) ty 50 (read-char)))
	(else (loop chl ty 50 ch))))
      ((50) ;; parse rest of exponent
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 50 (read-char)))
	((memq ch '(#\F #\f)) (loop (cons ch chl) ty 69 (read-char)))
	((memq ch '(#\L #\l)) (loop (cons ch chl) ty 65 (read-char)))
	((memq ch '(#\U #\u)) (loop (cons ch chl) '$fixpt 66 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	((memq ch '(#\D #\d)) (loop (cons ch chl) '$decfl 68 (read-char)))
	(else (loop chl ty 70 ch))))
      ;; suffixes
      ((61) ;; fixed/u
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\L #\l))
	 (loop (cons ch chl) ty 62 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ((62) ;; fixed/ul
       (cond
	((eof-object? ch) (loop chl '$fixed 72 ch))
	((memq ch '(#\L #\l))
	 (loop (cons ch chl) '$fixed 70 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ((63) ;; fixed/l
       (cond
	((eof-object? ch) (loop chl '$fixed 72 ch))
	((memq ch '(#\L #\l))
	 (loop (cons ch chl) ty 64 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl '$fixed 70 ch))))
      ((64) ;; fixed/ll
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\U #\u)) (loop (cons ch chl) ty 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ((65) ;; float/l
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ((66) ;; fixpt/u
       (cond
	((eof-object? ch) (bad-sfx chl))
	((memq ch '(#\L #\l))
	 (loop (cons ch chl) '$fixpt 67 (read-char)))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (bad-sfx chl))))
      ((67) ;; fixpt/ul
       (cond
	((eof-object? ch) (bad-sfx chl))
	((memq ch '(#\K #\R #\k #\r))
	 (loop (cons ch chl) '$fixpt 70 (read-char)))
	(else (bad-sfx chl))))
      ((68) ;; got (d|D), look for dDfFlL
       (cond
	((eof-object? ch) (bad-sfx chl))
	((memq ch '(#\D #\F #\L #\d #\f #\l))
	 (loop (cons ch chl) '$decfl 72 ch))
	(else (bad-sfx chl))))
      ((69) ;; bizarre gcc float suffixes: F128 F32x
       (cond
	((eof-object? ch) (loop chl ty 72 ch))
	((char-numeric? ch) (loop (cons ch chl) ty 69 (read-char)))
	((memq ch '(#\X #\x)) (loop (cons ch chl) ty 70 (read-char)))
	(else (loop chl ty 70 ch))))
      ;; exit
      ((70) ;; check char
       (cond
	((eof-object? ch) (cons ty (rls chl)))
	((char-set-contains? c:ir ch) (bad-sfx (cons ch chl)))
	(else (loop chl ty 71 ch))))
      ((71) ;; pushback 1 char
       (unread-char ch) (cons ty (rls chl)))
      ((72) ;; return
       (cons ty (rls chl)))
      (else
       (unread-chl chl) (error "coding error")))))

;; @deffn {Procedure} cnumstr->scm C99-str => scm-str
;; Convert C number-string (e.g, @code{0x123LL}) to Scheme numbers-string
;; (e.g., @code{#x123}).
;; This probably belongs in @code{(nyacc lang util)}.
;; @end deffn
(define (cnumstr->scm str)
  (define (2- n) (1- (1- n))) (define (3- n) (1- (2- n)))
  (let* ((nd (string-length str)))
    (define (trim-rt st) ;; trim U, UL, ULL (and lowercase) from right
      (if (char-set-contains? c:sx (string-ref str (1- nd)))
	  (if (char-set-contains? c:sx (string-ref str (2- nd)))
	      (if (char-set-contains? c:sx (string-ref str (3- nd)))
		  (substring str st (3- nd))
		  (substring str st (2- nd)))
	      (substring str st (1- nd)))
	  (substring str st nd)))
    (if (< nd 2) str
	(if (char=? #\0 (string-ref str 0))
	    (cond
	     ((char=? #\x (string-ref str 1))
	      (string-append "#x" (trim-rt 2)))
	     ((char=? #\X (string-ref str 1))
	      (string-append "#x" (trim-rt 2)))
	     ((char=? #\b (string-ref str 1))
	      (string-append "#b" (trim-rt 2)))
	     ((char-numeric? (string-ref str 1))
	      (string-append "#o" (trim-rt 1)))
	     (else (trim-rt 0)))
	    (trim-rt 0)))))

;;.@deffn {Procedure} si-map string-list ix => a-list
;; Convert list of strings to alist of char at ix and strings.
;; This is a helper for make-tree.
;; @end deffn
(define (si-map string-list ix)
  (let loop ((sal '()) (sl string-list))
    (cond
     ((null? sl) sal)
     ((= ix (string-length (car sl)))
      (loop (reverse (acons 'else (car sl) sal)) (cdr sl)))
     ((assq (string-ref (car sl) ix) sal) =>
      (lambda (pair)
        (set-cdr! pair (cons (car sl) (cdr pair)))
        (loop sal (cdr sl))))
     (else ;; Add (#\? . string) to alist.
      (loop (cons (cons (string-ref (car sl) ix) (list (car sl))) sal)
            (cdr sl))))))

;;.@deffn {Procedure} make-tree strtab -> tree
;; This routine takes an alist of strings and symbols and makes a tree
;; that parses one char at a time and provide @code{'else} entry for
;; signaling sequence found.  That is, if @code{("ab" . 1)} is an entry
;; then a chseq-reader (see below) would stop at @code{"ab"} and
;; return @code{1}.
;; @end deffn
(define (make-tree strtab)
  (define (si-cnvt string-list ix)
    (map (lambda (pair)
	   (if (pair? (cdr pair))
	       (cons (car pair) (si-cnvt (cdr pair) (1+ ix)))
	       (cons (car pair) (assq-ref strtab (cdr pair)))))
	 (si-map string-list ix)))
  (si-cnvt (map car strtab) 0))

;; @deffn {Procedure} make-chseq-reader strtab
;; Given alist of pairs (string, token) return a function that eats chars
;; until (token . string) is returned or @code{#f} if no match is found.
;; @end deffn
(define (make-chseq-reader strtab)
  ;; This code works on the assumption that the else-part is always last
  ;; in the list of transitions.
  (let ((tree (make-tree strtab)))
    (lambda (ch)
      (let loop ((cl (list ch)) (node tree))
	(cond
	 ((assq-ref node (car cl)) => ;; accept or shift next character
	  (lambda (n)
	    (if (eq? (caar n) 'else) ; if only else, accept, else read on
		(cons (cdar n) (lxlsr cl))
		(loop (cons (read-char) cl) n))))
	 ((assq-ref node 'else) => ; else exists, accept
	  (lambda (tok)
	    (unless (eof-object? (car cl)) (unread-char (car cl)))
	    (cons tok (lxlsr (cdr cl)))))
	 (else ;; reject
	  (let pushback ((cl cl))
	    (unless (null? (cdr cl))
	      (unless (eof-object? (car cl)) (unread-char (car cl)))
	      (pushback (cdr cl))))
	  #f))))))

;; @deffn {Procedure} make-comm-reader comm-table [#:eat-newline #t] => \
;;   ch bol -> ('$code-comm "..")|('$lone-comm "..")|#f
;; comm-table is list of cons for (start . end) comment.
;; e.g. ("--" . "\n") ("/*" . "*/")
;; test with "/* hello **/"
;; If @code{eat-newline} is specified as true then for read comments 
;; ending with a newline a newline swallowed with the comment.
;; The returned procedure has signature
;; @code{(proc ch #:optional bol #:skip-prefix #t|#f)}
;; @* Note: assumes backslash is never part of the end
;; @end deffn
(define* (make-comm-reader comm-table #:key eat-newline)

  (define (mc-read-char) ;; CHECK THIS
    (let ((ch (read-char)))
      (if (eqv? ch #\\)
	  (let ((ch (read-char)))
	    (if (eqv? ch #\newline)
		(read-char)
		(begin (unread-char ch) #\\)))
	  ch)))

  ;; Skip whitespace upto columm @var{col}, and fill in partial tab, if needed.
  ;; @example
  ;; (skip-ws-to-col 4 "    def" '(#\newline) => (#\d #\newline)
  ;; (skip-ws-to-col 6 "\tdef" '(#\newline)) => (#\d #\space #\space #\newline)
  ;; @end example
  (define (skip-to-col col il)
    (let loop ((il il) (cc 0) (ch (read-char)))
      ;;(simple-format #t " skip-to-col loop il=~S cc=~S ch=~S\n" il cc ch)
      (cond
       ((= cc col) (cons ch il))
       ((> cc col) (loop (cons ch il) (1- cc) #\space)) ; tab over-run
       ((char=? ch #\space) (loop il (1+ cc) (read-char)))
       ((char=? ch #\tab) (loop il (* 8 (quotient (+ cc 9) 8)) (read-char)))
       (else (cons ch il)))))
	 
  (let ((tree (make-tree comm-table)))
    (lambda* (ch #:optional bol #:key skip-prefix)
      (letrec
	  ((scol (1- (port-column (current-input-port)))) ;; 1- since ch read
	   (tval (if bol '$lone-comm '$code-comm))
	   (match-beg ;; match start of comment, return end-string
	    (lambda (cl node)
	      (cond
	       ((assq-ref node (car cl)) => ;; shift next character
		(lambda (n) (match-beg (cons (mc-read-char) cl) n)))
	       ((assq-ref node 'else) =>
		(lambda (res) (unread-char (car cl)) res)) ; yuck?
	       (else
		(let pushback ((cl cl))
		  (unless (null? (cdr cl))
		    (unread-char (car cl))
		    (pushback (cdr cl))))
		#f))))
	   (find-end ;; find end of comment, return comment
	    ;; cl: comm char list (cleared from ps);
	    ;; sl: shift list (matched from ps); il: input list;
	    ;; ps: pattern string (e.g., "*/") ; px: pattern index;
	    (lambda (cl sl il ps px)
	      (cond
	       ((eq? px (string-length ps)) ; can Guile optimize this?
		(if (and (not eat-newline) (eq? #\newline (car sl)))
		    (unread-char #\newline))
		(if (and (pair? cl) (eqv? (car cl) #\cr))
		    (cons tval (lxlsr (cdr cl))) ; remove trailing \r 
		    (cons tval (lxlsr cl))))
	       ((null? il) (find-end cl sl (cons (mc-read-char) il) ps px))
	       ((eof-object? (car il))
		(if (char=? (string-ref ps px) #\newline) (cons tval (lxlsr cl))
		    (throw 'nyacc-error "open comment")))
	       ((eqv? (car il) (string-ref ps px))
		(find-end cl (cons (car il) sl) (cdr il) ps (1+ px)))
	       ((and (char=? (car il) #\newline) skip-prefix)
		;; assumes newline can only be end of ps
		;;(simple-format #t "cl=~S sl=~S il=~S\n" cl sl il)
		(find-end (cons #\newline (append sl cl)) '()
			  (skip-to-col scol (cdr il)) ps 0))
	       (else
		(let ((il1 (append-reverse sl il)))
		  (find-end (cons (car il1) cl) '() (cdr il1) ps 0)))))))
	(let ((ep (match-beg (list ch) tree))) ;; ep=end pattern (e.g., "*/")
	  (if ep (find-end '() '() (list (mc-read-char)) ep 0) #f))))))

(define read-c-comm (make-comm-reader '(("/*" . "*/") ("//" . "\n"))))

;; @deffn {Procedure} filter-mt p? al => al
;; Filter match-table based on cars of al.
;; @end deffn
(define (filter-mt p? al) (filter (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} remove-mt p? al => al
;; Remove match-table based on cars of al.
;; @end deffn
(define (remove-mt p? al) (remove (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} map-mt f al => al
;; Map cars of al.
;; @end deffn
(define (map-mt f al) (map (lambda (x) (cons (f (car x)) (cdr x))) al))

;; @deffn {Procedure} make-lexer-generator match-table => lexer-generator
;; @example
;; (define gen-lexer (make-lexer-generator #:ident-reader my-id-rdr))
;; (with-input-from-file "foo" (parse (gen-lexer)))
;; @end example
;;
;; Return a thunk that returns tokens.
;; Change this to have user pass the following routines (optionally?)
;; read-num, read-ident, read-comm
;; reztab = reserved ($ident, $fixed, $float ...
;; chrtab = characters
;; comm-reader : if parser does not deal with comments must return #f
;;               but problem with character ..
;; extra-reader: insert an user-defined reader, (lambda (ch loop) ...)
;; match-table:
;; @enumerate
;; symbol -> (string . symbol)
;; reserved -> (symbol . symbol)
;; char -> (char . char)
;; @end enumerate
;; todo: add bol status
;; todo: maybe separate reading of keywords from identifiers: (keywd ch) =>
;; @end deffn
(define* (make-lexer-generator match-table
			       #:key
			       ident-reader num-reader
			       string-reader chlit-reader
			       comm-reader comm-skipper
			       space-chars extra-reader)
  (let* ((read-ident (or ident-reader (make-ident-reader c:if c:ir)))
	 (read-num (or num-reader read-basic-num))
	 (read-string (or string-reader (make-string-reader #\")))
	 (read-chlit (or chlit-reader (lambda (ch) #f)))
	 (read-comm (or comm-reader (lambda (ch bol) #f)))
	 (skip-comm (or comm-skipper (lambda (ch) #f)))
	 (spaces (or space-chars " \t\r\n"))
	 (space-cs (cond ((string? spaces) (string->char-set spaces))
			 ((list? spaces) (list->char-set spaces))
			 ((char-set? spaces) spaces)
			 (else (error "expecting string list or char-set"))))
	 (read-extra (or extra-reader (lambda (ch lp) #f)))
	 ;;
	 (ident-like? (make-ident-like-p read-ident))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt ident-like? strtab))  ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
	 (chrseq (remove-mt ident-like? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	  ; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (lambda ()
      (let ((bol #t))
	(lambda ()
	  (let loop ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (assc-$ (cons '$end ch)))
	     ;;((eq? ch #\newline) (set! bol #t) (loop (read-char)))
	     ((read-extra ch loop))
	     ((char-set-contains? space-cs ch) (loop (read-char)))
	     ((and (eqv? ch #\newline) (set! bol #t) #f))
	     ((read-comm ch bol) =>
	      (lambda (p) (set! bol #f) (assc-$ p)))
	     ((skip-comm ch) (loop (read-char)))
	     ((read-num ch) => assc-$)	  ; => $fixed or $float
	     ((read-string ch) => assc-$) ; => $string
	     ((read-chlit ch) => assc-$)  ; => $chlit
	     ((read-ident ch) =>
	      (lambda (s) (or (and=> (assq-ref keytab (string->symbol s))
				     (lambda (tval) (cons tval s)))
			      (assc-$ (cons '$ident s)))))
	     ((read-chseq ch) => identity)
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch ch))))))))) ; should be error

;; @end table

;; --- last line ---
