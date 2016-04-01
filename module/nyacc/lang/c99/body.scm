;;; lang/c99/body.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; C parser body, with cpp and tables makes a parser

(define-record-type cpi
  (make-cpi-1)
  cpi?
  (debug cpi-debug set-cpi-debug!)	; debug #t #f
  (defines cpi-defs set-cpi-defs!)	; #defines
  (incdirs cpi-incs set-cpi-incs!)	; #includes
  (tn-dict cpi-tynd set-cpi-tynd!)	; typename dict (("<x>" foo_t ..
  ;;
  ;;(typnams cpi-tyns set-cpi-tyns!)	; typedef names
  ;;
  (ptl cpi-ptl set-cpi-ptl!)		; parent typename list
  (ctl cpi-ctl set-cpi-ctl!)		; current typename list
  ;;
  ;;(typdcls cpi-tdls set-cpi-tdls!)	; typedef decls
  )

(define std-dict
  '(("time.h" "time_t" "clock_t" "size_t")
    ("stdio.h" "FILE" "size_t")
    ("string.h" "size_t")
    ("stddef.h" "ptrdiff_t" "size_t" "wchar_t")
    ("inttypes.h"
     "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
     "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
     "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
     "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t"
     "imaxdiv_t")
    ("stdint.h"
     "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
     "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
     "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
     "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t")
    ("stdlib.h" "div_t" "ldiv_t" "lldiv_t" "wchar_t")
    ("stdarg.h" "va_list")
    ;;("unistd.h" "div_t" "ldiv_t")
    ("signal.h" "sig_atomic_t")
    ("setjmp.h" "jmp_buf")
    ("float.h" "float_t")
    ("fenv.h" "fenv_t" "fexcept_t")
    ("complex.h" "complex" "imaginary")
    ("wchar.h" "wchar_t" "wint_t" "mbstate_t" "size_t")
    ("wctype.h" "wctrans_t" "wctype_t" "wint_t")
    ))

(define (make-cpi debug defines incdirs tn-dict)
  (let* ((cpi (make-cpi-1)))
    (set-cpi-debug! cpi debug)
    (set-cpi-defs! cpi defines)
    (set-cpi-incs! cpi incdirs)
    (set-cpi-tynd! cpi (append tn-dict std-dict))
    (set-cpi-ptl! cpi '())		; list of lists of strings
    (set-cpi-ctl! cpi '())		; list of strings ?
    cpi))

;; Need to have a "CPI" stack to deal with types (re)defined in multiple
;; branches of a #if...#endif statement.  If we are in "code" mode then we
;; may be skipping code so need to track when to shift and when not to.

(define *info* (make-fluid #f))

;; given tyns
;; cadr is next level
;; caar is list of sibs
;; search (caar car tyns), then (caar cadr tyns), then ...

;; @item typename? name
;; Called by lexer to determine if symbol is a typename.
;; Check current sibling for each generation.
(define (typename? name)
  ;;(simple-format #t "typename? ~S\n" name)
  (let ((cpi (fluid-ref *info*)))
    (when #f ;;(string=? name "SpiceInt")
      (simple-format #t "tn? ~S  ~S\n" (cpi-ctl cpi) (cpi-ptl cpi)))
    (if (member name (cpi-ctl cpi)) #t
        (let iter ((ptl (cpi-ptl cpi)))
	  (if (null? ptl) #f
	      (if (member name (car ptl)) #t
		  (iter (cdr ptl))))))))

;; @item add-typename name
;; Helper for @code{save-typenames}.
(define (add-typename name)
  ;;(simple-format #t "add-typename ~S\n" name)
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ctl! cpi (cons name (cpi-ctl cpi)))
    ;;(simple-format #t "at: ~S  ~S\n" (cpi-ctl cpi) (cpi-ptl cpi))
    ))

(define (cpi-push)	;; on #if
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ptl! cpi (cons (cpi-ctl cpi) (cpi-ptl cpi)))
    (set-cpi-ctl! cpi '())
    ;;(simple-format #t "pu: ~S\n" (cpi-ctl cpi))
    ))

(define (cpi-shift)	;; on #elif #else
  (set-cpi-ctl! (fluid-ref *info*) '()))

(define (cpi-pop)	;; on #endif
  (let ((cpi (fluid-ref *info*)))
    ;;(simple-format #t "po<: ~S ~S\n" (cpi-ctl cpi) (cpi-ptl cpi))
    (set-cpi-ctl! cpi (append (cpi-ctl cpi) (car (cpi-ptl cpi))))
    (set-cpi-ptl! cpi (cdr (cpi-ptl cpi)))
    ;;(simple-format #t "po>: ~S ~S\n" (cpi-ctl cpi) (cpi-ptl cpi))
    ))

(use-modules (ice-9 pretty-print))

;; @item find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
(define (find-new-typenames decl)

  ;; like declr->ident in util2.scm
  (define (declr->id-name declr)
    (case (car declr)
      ((ident) (sx-ref declr 1))
      ((init-declr) (declr->id-name (sx-ref declr 1)))
      ((comp-declr) (declr->id-name (sx-ref declr 1)))
      ((array-of) (declr->id-name (sx-ref declr 1)))
      ((ptr-declr) (declr->id-name (sx-ref declr 2)))
      ((ftn-declr) (declr->id-name (sx-ref declr 1)))
      ((scope) (declr->id-name (sx-ref declr 1)))
      (else (error "coding bug: " declr))))
       
  (let* ((spec (sx-ref decl 1))
	 (stor (sx-find 'stor-spec spec))
	 (id-l (sx-ref decl 2)))
    (if (and stor (eqv? 'typedef (caadr stor)))
	(let iter ((res '()) (idl (cdr id-l)))
	  (if (null? idl) res
	      (iter (cons (declr->id-name (sx-ref (car idl) 1)) res)
		    (cdr idl))))
	'())))

;; @item save-typenames decl
;; Save the typenames for the lexical analyzer and return the decl.
(define (save-typenames decl)
  ;; This finds typenames using @code{find-new-typenames} and adds via
  ;; @code{add-typename}.  Then return the decl.
  (for-each add-typename (find-new-typenames decl))
  decl)


;; ------------------------------------------------------------------------

;; @item read-cpp-line ch => #f | (cpp-xxxx)??
;; Given if ch is #\# read a cpp-statement
;; includes BUG: #define ABC 123 /* \n
(define (read-cpp-line ch)
  (if (not (eq? ch #\#)) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond
	 ((eq? ch #\newline) (list->string (reverse cl)))
	 ((eq? ch #\\)
	  (let ((c2 (read-char)))
	    (if (eq? c2 #\newline)
		(iter cl (read-char))
		(iter (cons* c2 ch cl) (read-char)))))
	 ((eq? ch #\/)
	  (let ((c2 (read-char)))
	    (if (eq? c2 #\*)
		(begin
		  (unread-char c2)
		  (unread-char #\/)
		  (list->string (reverse cl)))
		(iter (cons* c2 ch cl) (read-char)))))
	 (else (iter (cons ch cl) (read-char)))))))

;; @item find-file-in-dirl file dirl => path
(define (find-file-in-dirl file dirl)
  (let iter ((dirl dirl))
    (if (null? dirl) #f
	(let ((p (string-append (car dirl) "/" file)))
	  (if (access? p R_OK) p (iter (cdr dirl)))))))


;; @subsubsection CPP If-Else Processing
;; States are
;; @table code
;; @item skip
;; skip code
;; @item skip-look
;; skipping code, but still looking for true at this level
;; @item keep
;; keep code
;; @item keep1
;; keep one token and pop skip-stack
;; @item skip1
;; skip one token and pop skip-stack
;; @end table

;; @deffn gen-c-lexer [#:mode mode] => thunk
;; Generate a context-sensitive lexer for the C language.
;; The key-arg @var{mode} can be @code{'code} or @code{'file}.  If @code{'code}
;; @enumerate
;; @item
;; CPP defines are expanded (future work)
;; @item
;; CPP if/def is executed
;; @end enumerate
(define gen-c-lexer
  ;; This gets ugly in order to handle cpp.
  ;;.need to add support for num's w/ letters like @code{14L} and @code{1.3f}.
  ;; todo: I think there is a bug wrt the comment reader because // ... \n
  ;; will end up in same mode...  so after
  ;; int x; // comment
  ;; the lexer will think we are not at BOL.
  (let* ((match-table mtab)
	 (read-ident read-c-ident)
	 (read-comm read-c-comm)
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
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 ;;
	 (t-ident (assq-ref symtab '$ident))
	 (t-typename (assq-ref symtab 'typename))
	 (xp1 (sxpath '(cpp-stmt define)))
	 (xp2 (sxpath '(decl))))
    (lambda* (#:key (mode 'code))     ; modes are 'code or 'file
      (let ((bol #t)		      ; begin-of-line condition
	    (skip (list 'keep))	      ; CPP skip-input stack
	    (info (fluid-ref *info*)) ; assume make and run in same thread
	    (pstk '())		      ; port stack
	    )
	;; Return the first (tval lval) pair not excluded by the CPP.
	(lambda ()
      
	  (define (add-define tree)
	    (let* ((tail (cdr tree))
		   (name (car (assq-ref tail 'name)))
		   (args (assq-ref tail 'args))
		   (repl (car (assq-ref tail 'repl)))
		   (cell (cons name (if args (cons args repl) repl))))
	      (set-cpi-defs! info (cons cell (cpi-defs info)))))
	  
	  (define (rem-define name)
	      (set-cpi-defs! info (delete name (cpi-defs info))))
	  
	  (define (exec-cpp line)
	    ;;(simple-format #t "exec-cpp: ~S\n" line)
	    ;; Parse the line into a CPP stmt, execute it, and return it.
	    (let* ((stmt (parse-cpp-stmt line))
		   (perr (lambda (file)
			   (throw 'parse-error "file not found: ~S" file))))
	      (case (car stmt)
		((include)
		 (let* ((parg (cadr stmt)) (leng (string-length parg))
			(file (substring parg 1 (1- leng)))
			(tynd (assoc-ref (cpi-tynd info) file)))
		   ;;(simple-format #t "file: ~S\n" file) ;;(pretty-print tynd)
		   (if tynd
		       (for-each add-typename tynd)
		       (let* ((pth (find-file-in-dirl file (cpi-incs info)))
			      (tree (if pth ; path exists
					(or (with-input-from-file pth run-parse)
					    (throw 'parse-error "~A" pth))
					(perr file))))
			 (for-each add-define (xp1 tree)) ; add def's 
			 ;; Attach tree onto "include" statement.
			 (if (pair? tree) (set! stmt (append stmt (list tree))))
			 ))))
		((define)
		 (add-define stmt))
		((undef)
		 (rem-define (cadr stmt)))
		#;((ifdef) (cpi-push)) ;; ???
		#;((ifndef) (cpi-push))
		((if)
		 (cpi-push)
		 (if (eq? mode 'code)
		     (let ((val (eval-cpp-expr (cadr stmt) (cpi-defs info))))
		       (cond
			((not val)
			 (throw 'parse-error "unresolved: ~S" (cadr stmt)))
			((zero? val)
			 (set! skip (cons* 'skip-1 'skip-look skip)))
			(else
			 (set! skip (cons* 'skip-1 (car skip) skip)))))))
		((elif)
		 (if (eq? mode 'code)
		     (let ((val (eval-cpp-expr (cadr stmt) (cpi-defs info))))
		       (cond
			((not val)
			 (throw 'parse-error "unresolved: ~S" (cadr stmt)))
			((eq? 'keep (car skip))
			 (set! skip (cons* 'skip-1 'skip (cdr skip))))
			((zero? val)
			 (set! skip (cons* 'skip-1 skip)))
			((eq? 'skip-look (car skip))
			 (cpi-shift)
			 (set! skip (cons* 'skip-1 'keep (cdr skip))))
			(else
			 (cpi-shift)
			 (set! skip (cons* 'skip-1 'skip (cdr skip))))))
		     (cpi-shift)))
		((else)
		 (if (eq? mode 'code)
		     (cond
		      ((eq? 'skip-look (car skip))
		       (cpi-shift)
		       (set! skip (cons* 'skip-1 'keep (cdr skip))))
		      (else
		       (set! skip (cons* 'skip-1 'skip (cdr skip)))))
		     (cpi-shift)))
		((endif)
		 (cpi-pop)
		 (if (eq? mode 'code)
		     (set! skip (cons 'skip-1 (cdr skip)))))
		((error)
		 stmt)
		(else
		 (error "unhandled cpp stmt")))
	      (cons 'cpp-stmt stmt)))
	  
	  ;; Composition of @code{read-cpp-line} and @code{exec-cpp}.
	  (define (read-cpp ch)
	    (and=> (read-cpp-line ch) exec-cpp))

	  (define (read-token)
	    (let iter ((ch (read-char)))
	      (cond
	       ((eof-object? ch)
		(if (pop-input) (iter (read-char)) (assc-$ '($end . ""))))
	       ((eq? ch #\newline) (set! bol #t) (iter (read-char)))
	       ((char-set-contains? c:ws ch) (iter (read-char)))
	       (bol
		(cond
		 ((read-comm ch bol) => assc-$)
		 ((read-cpp ch) => assc-$)
		 (else (set! bol #f) (iter ch))))
	       ((read-ident ch) =>
		(lambda (str)
		  (let ((sym (string->symbol str))
			(repl (and ;;(eq? mode 'code)
			       (assoc-ref (cpi-defs info) str))))
		    (cond
		     ((string? repl) ;; expand cpp-defined argless macro
		      (simple-format #t "repl=~S\n" repl)
		      (push-input (open-input-string repl))
		      (iter (read-char)))
		     ((and repl (replace-cpp-def str (cpi-defs info))) =>
		      (lambda (st)
			(push-input (open-input-string st))
			(iter (read-char))))
		     ((assq-ref keytab sym) => (lambda (t) (cons t str)))
		     ((typename? str)
		      (cons (assq-ref symtab 'typename) str))
		     (else (cons (assq-ref symtab '$ident) str))))))
	       ((read-c-num ch) => assc-$)
	       ((read-c-string ch) => assc-$)
	       ((read-c-chlit ch) => assc-$)
	       ((read-comm ch bol) => assc-$)
	       ((read-chseq ch) => identity)
	       ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	       ((eqv? ch #\\) ;; C allows \ at end of line to continue
		(let ((ch (read-char)))
		  (cond ((eqv? #\newline ch) (iter (read-char))) ;; extend line
			(else (unread-char ch) (cons #\\ "\\"))))) ;; parse err
	       (else (cons ch (string ch))))))

	  ;; Loop between reading tokens and skipping tokens.
	  ;; The use of "delayed pop" is not clean IMO.  Cleaner way?
	  (let loop ((pair (read-token)))
	    (case (car skip)
	      ((keep) pair)
	      ((skip skip-look) (loop (read-token)))
	      ((skip-1)
	       (set! skip (cdr skip))
	       (loop (read-token)))))
	  )))))

;; --- last line ---
