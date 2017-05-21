;;; module/nyacc/util.scm
;;;
;;; Copyright (C) 2015-2017 Matthew R. Wette
;;;
;;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;;; or any later version published by the Free Software Foundation.  See
;;; the file COPYING included with the nyacc distribution.

;; runtime utilities for the parsers -- needs work

(define-module (nyacc lang util)
  #:export (lang-crn-lic 
	    report-error
	    push-input pop-input reset-input-stack
	    make-tl tl->list ;; rename?? to tl->sx for sxml-expr
	    tl-append tl-insert tl-extend tl+attr
	    sx-tag sx-attr sx-tail sx-ref sx-cons* sx-list
	    sx-attr-ref sx-has-attr? sx-set-attr! sx-set-attr*
	    sx-find
	    ;; for pretty-printing
	    make-protect-expr make-pp-formatter make-pp-formatter/ugly
	    ;; for ???
	    move-if-changed
	    fmterr)
  #:use-module ((srfi srfi-1) #:select(find))
  )

;; This is a generic copyright/licence that will be printed in the output
;; of the examples/nyacc/lang/*/ actions.scm and tables.scm files.
(define lang-crn-lic "

This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
or any later version published by the Free Software Foundation.  See
the file COPYING included with the this distribution.")

(define (fmterr fmt . args)
  (apply simple-format (current-error-port) fmt args))

;; @deffn report-error fmt args
;; Report an error: to stderr, providing file and line num info, and add nl.
(define (report-error fmt args)
  (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
	(ln (1+ (port-line (current-input-port)))))
    (apply simple-format (current-error-port)
	   (string-append "~A:~A: " fmt "\n") fn ln args)))

;; === input stack =====================

(define *input-stack* (make-fluid '()))

(define (reset-input-stack)
  (fluid-set! *input-stack* '()))

(define (push-input port)
  (let ((curr (current-input-port))
	(ipstk (fluid-ref *input-stack*)))
    (fluid-set! *input-stack* (cons curr ipstk))
    (set-current-input-port port)))

;; Return #f if empty
(define (pop-input)
  (let ((ipstk (fluid-ref *input-stack*)))
    (if (null? ipstk) #f
	(begin
	  (set-current-input-port (car ipstk))
	  (fluid-set! *input-stack* (cdr ipstk))))))

;; === tl ==============================

;; @section Tagged Lists
;; Tagged lists are
;; They are implemented as a cons cell with the car and the cdr a list.
;; The cdr is used to accumulate appended items and the car is used to
;; keep the tag, attributes and inserted items.
;; @example
;; tl => '(H . T), H => (c a b 'tag); T =>
;; @end example

;; @table code

;; @deffn {Procedure} make-tl tag [item item ...]
;; Create a tagged-list structure.
;; @end deffn
(define (make-tl tag . rest)
  (let iter ((tail tag) (l rest))
    (if (null? l) (cons '() tail)
	(iter (cons (car l) tail) (cdr l)))))

;; @deffn {Procedure} tl->list tl
;; Convert a tagged list structure to a list.  This collects added attributes
;; and puts them right after the (leading) tag, resulting in something like
;; @example
;; (<tag> (@ <attr>) <rest>)
;; @end example
;; @end deffn
(define (tl->list tl)
  (let ((heda (car tl))
	(head (let iter ((head '()) (attr '()) (tl-head (car tl)))
		(if (null? tl-head)
		    (if (pair? attr)
			(cons (cons '@ attr) (reverse head))
			(reverse head))
		    (if (and (pair? (car tl-head)) (eq? '@ (caar tl-head)))
			(iter head (cons (cdar tl-head) attr) (cdr tl-head))
			(iter (cons (car tl-head) head) attr (cdr tl-head)))))))
    (let iter ((tail '()) (tl-tail (cdr tl)))
      (if (pair? tl-tail)
	  (iter (cons (car tl-tail) tail) (cdr tl-tail))
	  (cons tl-tail (append head tail))))))

;; @deffn {Procedure} tl-insert tl item
;; Insert item at front of tagged list (but after tag).
;; @end deffn
(define (tl-insert tl item)
  (cons (cons item (car tl)) (cdr tl)))

;; @deffn {Procedure} tl-append tl item ...
;; Append items at end of tagged list.
;; @end deffn
(define (tl-append tl . rest)
  (cons (car tl)
	(let iter ((tail (cdr tl)) (items rest))
	  (if (null? items) tail
	      (iter (cons (car items) tail) (cdr items))))))

;; @deffn {Procedure} tl-extend tl item-l
;; Extend with a list of items.
;; @end deffn
(define (tl-extend tl item-l)
  (apply tl-append tl item-l))

;; @deffn {Procedure} tl-extend! tl item-l
;; Extend with a list of items.  Uses @code{set-cdr!}.
;; @end deffn
(define (tl-extend! tl item-l)
  (set-cdr! (last-pair tl) item-l)
  tl)

;; @deffn {Procedure} tl+attr tl key val)
;; Add an attribute to a tagged list.  Return the tl.
;; @example
;; (tl+attr tl 'type "int")
;; @end example
;; @end deffn
(define (tl+attr tl key val)
  (tl-insert tl (cons '@ (list key val))))

;; @deffn {Procedure} tl-merge tl tl1
;; Merge guts of phony-tl @code{tl1} into @code{tl}.
;; @end deffn
(define (tl-merge tl tl1)
  (error "not implemented (yet)")
  )

;; === sx ==============================
;; @section SXML Utility Procedures
;; Some lot of these look like existing Guile list procedures (e.g.,
;; @code{sx-tail} versus @code{list-tail} but in sx lists the optional
;; attributea are `invisible'. For example, @code{'(elt (@abc) "d")}
;; is an sx of length two: the tag @code{elt} and the payload @code{"d"}.

;; @deffn {Procedure} sx-ref sx ix => item
;; Reference the @code{ix}-th element of the list, not counting the optional
;; attributes item.  If the list is shorter than the index, return @code{#f}.
;; @example
;; (sx-ref '(abc "def") 1) => "def"
;; (sx-ref '(abc (@ (foo "1")) "def") 1) => "def"
;; @end example
;; @end deffn
(define (sx-ref sx ix)
  (define (list-xref l x) (if (> (length l) x) (list-ref l x) #f))
  (cond
   ((zero? ix) (car sx))
   ((and (pair? (cadr sx)) (eqv? '@ (caadr sx)))
    (list-xref sx (1+ ix)))
   (else
    (list-xref sx ix))))

;; @deffn {Procedure} sx-tag sx => tag
;; Return the tag for a tree
;; @end deffn
(define (sx-tag sx)
  (if (pair? sx) (car sx) #f))

;; @deffn {Procedure} sx-cons* tag (attr|#f)? ... => sx
;; @deffnx {Procedure} sx-list tag (attr|#f)? ... => sx
;; Generate the tag and the attr list if it exists.  Note that
;; @end deffn
(define (sx-cons* tag . rest)
  (cond
   ((null? rest) (list tag))
   ((not (car rest)) (apply cons* tag (cdr rest)))
   (else (apply cons* tag rest))))
(define (sx-list tag . rest)
  (cond
   ((null? rest) (list tag))
   ((not (car rest)) (apply list tag (cdr rest)))
   (else (apply list tag rest))))

;; @deffn {Procedure} sx-tail sx [ix] => (list)
;; Return the ix-th tail starting after the tag and attribut list, where
;; @var{ix} must be positive.  For example,
;; @example
;; (sx-tail '(tag (@ (abc . "123")) (foo) (bar)) 1) => ((foo) (bar))
;; @end example
;; Without second argument @var{ix} is 1.
;; @end deffn
(define sx-tail
  (case-lambda
   ((sx ix)
    (cond
     ((zero? ix) (error "sx-tail: expecting index greater than 0"))
     ((and (pair? (cadr sx)) (eqv? '@ (caadr sx))) (list-tail sx (1+ ix)))
     (else (list-tail sx ix))))
   ((sx)
    (sx-tail sx 1))))

;; @deffn {Procedure} sx-has-attr? sx
;; p to determine if @arg{sx} has attributes.
;; @end deffn
(define (sx-has-attr? sx)
  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx))))

;; @deffn {Procedure} sx-attr sx => '(@ ...)|#f
;; @example
;; (sx-attr '(abc (@ (foo "1")) def) 1) => '(@ (foo "1"))
;; @end example
;; should change this to
;; @example
;; (sx-attr sx) => '((a . 1) (b . 2) ...)
;; @end example
;; @end deffn
(define (sx-attr sx)
  (if (and (pair? (cdr sx)) (pair? (cadr sx)))
      (if (eqv? '@ (caadr sx))
	  (cadr sx)
	  #f)
      #f))

;; @deffn {Procedure} sx-attr-ref sx key => val
;; Return an attribute value given the key, or @code{#f}.
;; @end deffn
(define (sx-attr-ref sx key)
  (and=> (sx-attr sx)
	 (lambda (attr)
	   (and=> (assq-ref (cdr attr) key) car))))

;; @deffn {Procedure} sx-set-attr! sx key val
;; Set attribute for sx.  If no attributes exist, if key does not exist,
;; add it, if it does exist, replace it.
;; @end deffn
(define (sx-set-attr! sx key val . rest)
  (if (sx-has-attr? sx)
      (let ((attr (cadr sx)))
	(set-cdr! attr (assoc-set! (cdr attr) key (list val))))
      (set-cdr! sx (cons `(@ (,key ,val)) (cdr sx))))
  sx)

;; @deffn sx-set-attr* sx key val [key val [key ... ]]
;; Generate sx with added or changed attributes.
;; @end deffn
(define (sx-set-attr* sx . rest)
  (let iter ((attr (or (and=> (sx-attr sx) cdr) '())) (kvl rest))
    (cond
     ((null? kvl) (cons* (sx-tag sx) (cons '@ (reverse attr)) (sx-tail sx 1)))
     (else (iter (cons (list (car kvl) (cadr kvl)) attr) (cddr kvl))))))

;; @deffn sx-find tag sx => ((tag ...) (tag ...))
;; Find the first matching element (in the first level).
;; @end deffn
(define (sx-find tag sx)
  (find (lambda (node)
	  (and (pair? node) (eqv? tag (car node))))
	sx))

;;; === pp ==========================
;; @section Pretty-Print and Other Utility Procedures

;; @deffn {Procedure} make-protect-expr op-prec op-assc => side op expr => #t|#f
;; Generate procedure @code{protect-expr} for pretty-printers, which takes
;; the form @code{(protect-expr? side op expr)} and where @code{side}
;; is @code{'lval} or @code{'rval}, @code{op} is the operator and @code{expr}
;; is the expression.  The argument @arg{op-prec} is a list of equivalent
;; operators in order of decreasing precedence and @arg{op-assc} is an
;; a-list of precedence with keys @code{'left}, @code{'right} and
;; @code{nonassoc}.
;; @example
;; (protect-expr? 'left '+ '(mul ...)) => TBD
;; @end example
;; @end deffn
(define (make-protect-expr op-prec op-assc)

  (define (assc-lt? op)
    (memq op (assq-ref op-assc 'left)))

  (define (assc-rt? op)
    (memq op (assq-ref op-assc 'right)))

  ;; @deffn {Procedure} prec a b => '>|'<|'=|#f
  ;; Returns the prececence relation of @code{a}, @code{b} as
  ;; @code{<}, @code{>}, @code{=} or @code{#f} (no relation).
  ;; @end deffn
  (define (prec a b)
    (let iter ((ag #f) (bg #f) (opg op-prec)) ;; a-group, b-group
      (cond
       ((null? opg) #f)			; indeterminate
       ((memq a (car opg))
	(if bg '<
	    (if (memq b (car opg)) '=
		(iter #t bg (cdr opg)))))
       ((memq b (car opg))
	(if ag '>
	    (if (memq a (car opg)) '=
		(iter ag #t (cdr opg)))))
       (else
	(iter ag bg (cdr opg))))))

  (lambda (side op expr)
    (let ((assc? (case side
		   ((lt lval left) assc-rt?)
		   ((rt rval right) assc-lt?)))
	  (vtag (car expr)))
      (case (prec op vtag)
	((>) #t)
	((<) #f)
	((=) (assc? op))
	(else #f)))))

;; @deffn {Procedure} make-pp-formatter [port] [#:per-line-prefix ""] => fmtr
;; @example
;; (fmtr 'push) ;; push indent level
;; (fmtr 'pop)  ;; pop indent level
;; (fmtr "fmt" arg1 arg2 ...)
;; @end example
;; @end deffn
(define* (make-pp-formatter #:optional (port (current-output-port))
			    #:key per-line-prefix)
  (letrec
      ((maxcol (- 78 (if per-line-prefix (string-length per-line-prefix) 0)))
       ;;(maxcol 78)
       (maxind 36)
       (column 0)
       (ind-lev 0)
       (ind-len 0)
       (blanks "                                            ")
       (ind-str (lambda () (substring blanks 0 ind-len)))
       (cnt-str (lambda () (substring blanks 0 (+ 4 ind-len))))
       ;;(sf-nl (lambda () (newline) (set! column 0)))

       (push-il
	(lambda ()
	  (set! ind-lev (min maxind (1+ ind-lev)))
	  (set! ind-len (* 2 ind-lev))))

       (pop-il
	(lambda ()
	  (set! ind-lev (max 0 (1- ind-lev)))
	  (set! ind-len (* 2 ind-lev))))

       (inc-column!
	(lambda (inc)
	  (set! column (+ column inc))))

       (set-column!
	(lambda (val)
	  (set! column val)))
       
       (sf
	(lambda (fmt . args)
	  (let* ((str (apply simple-format #f fmt args))
		 (len (string-length str)))
	    (cond
	     ((zero? column)
	      (if per-line-prefix (display per-line-prefix port))
	      (display (ind-str) port)
	      (inc-column! ind-len))
	     ((> (+ column len) maxcol)
	      (newline port)
	      (if per-line-prefix (display per-line-prefix port))
	      (display (cnt-str) port)
	      (set-column! (+ ind-len 4))))
	    (display str port)
	    (inc-column! len)
	    (when (and (positive? len)
		       (eqv? #\newline (string-ref str (1- len))))
	      (set! column 0))))))

    (lambda (arg0 . rest)
      (cond
       ;;((string? arg0) (if (> (string-length arg0) 0) (apply sf arg0 rest)))
       ((string? arg0) (apply sf arg0 rest))
       ((eqv? 'push arg0) (push-il))
       ((eqv? 'pop arg0) (pop-il))
       ((eqv? 'nlin arg0) ;; newline if needed
        (cond ((positive? column) (newline) (set! column 0))))
       (else (error "pp-formatter: bad args"))
       ))))

;; @deffn {Procedure} make-pp-formatter/ugly => fmtr
;; Makes a @code{fmtr} like @code{make-pp-formatter} but no indentation
;; and just adds strings on ...
;; This is specific to C/C++ because it will newline if #\# seen first.
;; @end deffn
(define* (make-pp-formatter/ugly)
  (let*
      ((maxcol 78)
       (column 0)
       (sf (lambda (fmt . args)
	     (let* ((str (apply simple-format #f fmt args))
		    (len (string-length str)))
	       (if (and (positive? len)
			(char=? #\newline (string-ref str (1- len))))
		   (string-set! str (1- len) #\space))
	       (cond
		((zero? len) #t)	; we reference str[0] next
		((and (equal? len 1) (char=? #\newline (string-ref str 0))) #t)
		((char=? #\# (string-ref str 0)) ; CPP-stmt: force newline
		 (when (positive? column) (newline))
		 (display str)		; str always ends in \n
		 (set! column		; if ends \n then col= 0 else len
		       (if (char=? #\newline (string-ref str (1- len)))
			   0 len)))
		((zero? column)
		 (display str)
		 (set! column len))
		(else
		 (when (> (+ column len) maxcol)
		   (newline)
		   (set! column 0))
		 (display str)
		 (set! column (+ column len))))))))

    (lambda (arg0 . rest)
      (cond
       ((string? arg0) (apply sf arg0 rest))
       ((eqv? 'nlin arg0) ;; newline if needed
        (cond ((positive? column) (newline) (set! column 0))))
       ((eqv? 'push arg0) #f)
       ((eqv? 'pop arg0) #f)
       (else (error "pp-formatter/ugly: bad args"))))))
  
;; @deffn {Procedure} move-if-changed src-file dst-file [sav-file]
;; Return @code{#t} if changed.
;; @end deffn
(define (move-if-changed src-file dst-file . rest)

  (define (doit)
    (let ((sav-file (if (pair? rest) (car rest) #f)))
      (if (and sav-file (access? sav-file W_OK))
	  (system (simple-format #f "mv ~A ~A" dst-file sav-file)))
      (system (simple-format #f "mv ~A ~A" src-file dst-file))
      #t))
    
  (cond
   ;; src-file does not exist
   ((not (access? src-file R_OK)) #f)

   ;; dst-file does not exist, update anyhow
   ((not (access? dst-file F_OK))
    (system (simple-format #f "mv ~A ~A" src-file dst-file)) #t)

   ;; both exist, but no changes
   ((zero? (system
	    (simple-format #f "cmp ~A ~A >/dev/null" src-file dst-file)))
    (system (simple-format #f "rm ~A" src-file)) #f)

   ;; both exist, update
   ((access? dst-file W_OK)
    (doit))
   
   (else
    (simple-format (current-error-port) "move-if-changed: no write access\n")
    #f)))

;; @end table

;;; --- last line ---
