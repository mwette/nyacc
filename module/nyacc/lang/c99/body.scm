;;; lang/c99/body.scm
;;;
;;; Copyright (C) 2015-2017 Matthew R. Wette
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

;; Notes on the code design may be found in doc/nyacc/lang/c99-hg.info

;; @section The C99 Parser Body
;; This code provides the front end to the C99 parser, including the lexical
;; analyzer and optional CPP processing.  In @code{'file} mode the lex'er
;; passes CPP statements to the parser; in @code{'code} mode the lex'er
;; parses and evaluates the CPP statements.  In the case of included files
;; (e.g., via @code{#include <file.h>}) the include files are parsed if
;; not in @code{inc-help}.  The a-list @code{inc-help} maps
;; include file names to typenames (e.g., @code{stdio.h} to @code{FILE}) and
;; CPP defines (e.g., "INT_MAX=12344").

(use-modules ((srfi srfi-9) #:select (define-record-type)))
(use-modules ((sxml xpath) #:select (sxpath)))
(use-modules (ice-9 regex))

(define-record-type cpi
  (make-cpi-1)
  cpi?
  (debug cpi-debug set-cpi-debug!)	; debug #t #f
  (defines cpi-defs set-cpi-defs!)	; #defines
  (incdirs cpi-incs set-cpi-incs!)	; #includes
  (inc-tynd cpi-itynd set-cpi-itynd!)	; a-l of incfile => typenames
  (inc-defd cpi-idefd set-cpi-idefd!)	; a-l of incfile => defines
  (ptl cpi-ptl set-cpi-ptl!)		; parent typename list
  (ctl cpi-ctl set-cpi-ctl!)		; current typename list
  )

;;.@deffn Procedure split-cppdef defstr => (<name> . <repl>)| \
;;     ((<name>  <args> . <repl>)|#f
;; Convert define string to a dict item.  Examples:
;; @example
;; "ABC=123" => '("ABC" . "123")
;; "MAX(X,Y)=((X)>(Y)?(X):(Y))" => ("MAX" ("X" "Y") . "((X)>(Y)?(X):(Y))")
;; @end example
;; @end deffn
(define split-cppdef
  (let ((rx1 (make-regexp "^([A-Za-z0-9_]+)\\([^)]*\\)=(.*)$"))
	(rx2 (make-regexp "^([A-Za-z0-9_]+)=(.*)$")))
    (lambda (defstr)
      (let* ((m1 (regexp-exec rx1 defstr))
	     (m2 (or m1 (regexp-exec rx2 defstr))))
	(cond
	 ((regexp-exec rx1 defstr) =>
	  (lambda (m)
	    (let* ((s1 (match:substring m1 1))
		   (s2 (match:substring m1 2))
		   (s3 (match:substring m1 3)))
	      (cons s1 (cons s2 s3)))))
	 ((regexp-exec rx2 defstr) =>
	  (lambda (m)
	    (let* ((s1 (match:substring m2 1))
		   (s2 (match:substring m2 2)))
	      (cons s1 s2))))
	 (else #f))))))

;; @deffn Procedure make-cpi debug defines incdirs inchelp
;; @end deffn
(define (make-cpi debug defines incdirs inchelp)
  ;; convert inchelp into inc-file->typenames and inc-file->defines
  ;; Any entry for an include file which contains `=' is considered
  ;; a define; otherwise, the entry is a typename.

  (define (split-helper helper)
    (let ((file (car helper)))
      (let iter ((tyns '()) (defs '()) (ents (cdr helper)))
	(cond
	 ((null? ents) (values (cons file tyns) (cons file defs)))
	 ((split-cppdef (car ents)) =>
	  (lambda (def) (iter tyns (cons def defs) (cdr ents))))
	 (else (iter (cons (car ents) tyns) defs (cdr ents)))))))

  (let* ((cpi (make-cpi-1)))
    (set-cpi-debug! cpi debug)		; print states debug 
    (set-cpi-defs! cpi (map split-cppdef defines)) ; list of define strings
    (set-cpi-incs! cpi incdirs)		; list of include dir's
    (set-cpi-ptl! cpi '())		; list of lists of typenames
    (set-cpi-ctl! cpi '())		; list of typenames
    ;; itynd idefd:
    (let iter ((itynd '()) (idefd '()) (helpers inchelp))
      (cond ((null? helpers)
	     (set-cpi-itynd! cpi itynd)
	     (set-cpi-idefd! cpi idefd))
	    (else
	     (call-with-values
		 (lambda () (split-helper (car helpers)))
	       (lambda (ityns idefs)
		 (iter (cons ityns itynd) (cons idefs idefd) (cdr helpers)))))))
    cpi))

(define *info* (make-fluid #f))
	  
;; @deffn {Procedure} typename? name
;; Called by lexer to determine if symbol is a typename.
;; Check current sibling for each generation.
;; @end deffn
(define (typename? name)
  (let ((cpi (fluid-ref *info*)))
    (if (member name (cpi-ctl cpi)) #t
        (let iter ((ptl (cpi-ptl cpi)))
	  (if (null? ptl) #f
	      (if (member name (car ptl)) #t
		  (iter (cdr ptl))))))))

;; @deffn {Procedure} add-typename name
;; Helper for @code{save-typenames}.
;; @end deffn
(define (add-typename name)
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ctl! cpi (cons name (cpi-ctl cpi)))))

(define (cpi-push)	;; on #if
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ptl! cpi (cons (cpi-ctl cpi) (cpi-ptl cpi)))
    (set-cpi-ctl! cpi '())))

(define (cpi-shift)	;; on #elif #else
  (set-cpi-ctl! (fluid-ref *info*) '()))

(define (cpi-pop)	;; on #endif
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ctl! cpi (append (cpi-ctl cpi) (car (cpi-ptl cpi))))
    (set-cpi-ptl! cpi (cdr (cpi-ptl cpi)))))

;; @deffn {Procedure} find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
;; @end deffn
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

;; @deffn {Procedure} save-typenames decl
;; Save the typenames for the lexical analyzer and return the decl.
;; @end deffn
(define (save-typenames decl)
  ;; This finds typenames using @code{find-new-typenames} and adds via
  ;; @code{add-typename}.  Then return the decl.
  (for-each add-typename (find-new-typenames decl))
  decl)

;; ------------------------------------------------------------------------

(define (p-err . args)
  (apply throw 'c99-error args))

;; @deffn {Procedure} read-cpp-line ch => #f | (cpp-xxxx)??
;; Given if ch is #\# read a cpp-statement.
;; The standard implies that comments are tossed here but we keep them
;; so that they can end up in the pretty-print output.
;; @end deffn
(define (read-cpp-line ch)
  (if (not (eq? ch #\#)) #f
      (let iter ((cl '()) (ch (read-char)))
	(cond
	 ((eof-object? ch) (throw 'cpp-error "CPP lines must end in newline"))
	 ((eq? ch #\newline) (unread-char ch) (list->string (reverse cl)))
	 ((eq? ch #\\)
	  (let ((c2 (read-char)))
	    (if (eq? c2 #\newline)
		(iter cl (read-char))
		(iter (cons* c2 ch cl) (read-char)))))
	 ((eq? ch #\/) ;; swallow comments, event w/ newlines
	  (let ((c2 (read-char)))
	    (cond
	     ((eqv? c2 #\*)
	      (let iter2 ((cl2 (cons* #\* #\/ cl)) (ch (read-char)))
		(cond
		 ((eq? ch #\*)
		  (let ((c2 (read-char)))
		    (if (eqv? c2 #\/)
			(iter (cons* #\/ #\* cl2) (read-char)) ;; keep comment
			(iter2 (cons #\* cl2) c2))))
		 (else
		  (iter2 (cons ch cl2) (read-char))))))
	     (else
	      (iter (cons #\/ cl) c2)))))
	 (else (iter (cons ch cl) (read-char)))))))

;; @deffn {Procedure} find-file-in-dirl file dirl => path
;; @end deffn
(define (find-file-in-dirl file dirl)
  (let iter ((dirl dirl))
    (if (null? dirl) #f
	(let ((p (string-append (car dirl) "/" file)))
	  (if (access? p R_OK) p (iter (cdr dirl)))))))

(define (def-xdef? name mode)
  (eqv? mode 'code))

;; @deffn {Procedure} gen-c-lexer [#:mode mode] [#:xdef? proc] => procedure
;; Generate a context-sensitive lexer for the C99 language.  The generated
;; lexical analyzer reads and passes comments and optionally CPP statements
;; to the parser.  The keyword argument @var{mode} will determine if CPP
;; statements are passed (@code{'file} mode) or parsed and executed
;; (@code{'file} mode) as described above.  Comments will be passed as
;; ``line'' comments or ``lone'' comments: lone comments appear on a line
;; without code.  The @code{xdef?} keyword argument allows user to pass
;; a predicate which determines whether CPP symbols in code are expanded.
;; The default predicate is
;; @example
;; (define (def-xdef? mode name) (eqv? mode 'code))
;; @end example
;; @end deffn
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
    ;; mode: 'code|'file
    ;; xdef?: (proc name mode) => #t|#f  : do we expand #define?
    (lambda* (#:key (mode 'code) (xdef? #f))
      (let ((bol #t)		      ; begin-of-line condition
	    (ppxs (list 'keep))	      ; CPP execution state stack
	    (info (fluid-ref *info*)) ; assume make and run in same thread
	    (x-def? (or xdef? def-xdef?)))
	;; Return the first (tval . lval) pair not excluded by the CPP.
	(lambda ()

	  (define (exec-cpp?) ; exec (vs pass to parser) CPP stmts?
	    (eqv? mode 'code))

	  (define (cpp-flow? keyw)
	    (memq keyw '(if elif else)))
      
	  (define (add-define tree)
	    (let* ((tail (cdr tree))
		   (name (car (assq-ref tail 'name)))
		   (args (assq-ref tail 'args))
		   (repl (car (assq-ref tail 'repl)))
		   (cell (cons name (if args (cons args repl) repl))))
	      (set-cpi-defs! info (cons cell (cpi-defs info)))))
	  
	  (define (rem-define name)
	      (set-cpi-defs! info (delete name (cpi-defs info))))
	  
	  (define (apply-helper file)
	    (let* ((tyns (assoc-ref (cpi-itynd info) file))
		   (defs (assoc-ref (cpi-idefd info) file)))
	      (when tyns
		(for-each add-typename tyns)
		(set-cpi-defs! info (append defs (cpi-defs info))))
	      tyns))
	  
	  ;; Evaluate expression text in #if of #elif statement.
	  (define (eval-cpp-cond-text text)
	    (with-throw-handler
	     'cpp-error
	     (lambda ()
	       (let* ((defs (cpi-defs info))
		      (rhs (cpp-expand-text text defs))
		      (exp (parse-cpp-expr rhs)))
		 (eval-cpp-expr exp defs)))
	     (lambda (key fmt . args)
	       (report-error fmt args)
	       (throw 'c99-error "CPP error"))))

	  (define (inc-stmt->file stmt)
	    (let* ((arg (cadr stmt)) (len (string-length arg)))
	      (substring arg 1 (1- len))))

	  (define (inc-file->path file)
	    (find-file-in-dirl file (cpi-incs info)))

	  (define (eval-cpp-stmt-1/code stmt)
	    ;; eval control flow states: {skip-look, keep, skip-done, skip}
	    (case (car stmt)
	      ((if)
	       (case (car ppxs)
		 ((skip-look skip-done skip) ;; don't eval if excluded
		  (set! ppxs (cons 'skip ppxs)))
		 (else
		  (let ((val (eval-cpp-cond-text (cadr stmt))))
		    (if (not val) (p-err "unresolved: ~S" (cadr stmt)))
		    (if (eq? 'keep (car ppxs))
			(if (zero? val)
			    (set! ppxs (cons 'skip-look ppxs))
			    (set! ppxs (cons 'keep ppxs)))
			(set! ppxs (cons 'skip-done ppxs)))))))
	      ((elif)
	       (case (car ppxs)
		 ((skip) #t) ;; don't eval if excluded
		 (else
		  (let ((val (eval-cpp-cond-text (cadr stmt))))
		    (if (not val) (p-err "unresolved: ~S" (cadr stmt)))
		    (case (car ppxs)
		      ((skip-look) (if (not (zero? val)) (set-car! ppxs 'keep)))
		      ((keep) (set-car! ppxs 'skip-done)))))))
	      ((else)
	       (case (car ppxs)
		 ((skip-look) (set-car! ppxs 'keep))
		 ((keep) (set-car! ppxs 'skip-done))))
	      ((endif)
	       (set! ppxs (cdr ppxs)))
	      (else
	       (if (eqv? 'keep (car ppxs))
		   (eval-cpp-stmt-2/code stmt)))))

	  (define (eval-cpp-stmt-2/code stmt)
	    ;; eval non-control flow
	    (case (car stmt)
	      ;; actions
	      ((include)
	       (let* ((file (inc-stmt->file stmt))
		      (path (inc-file->path file)))
		 (cond
		  ((apply-helper file)) ; use helper
		  ((not path) (p-err "not found: ~S" file)) ; file not found
		  (else (set! bol #t) (push-input (open-input-file path))))))
	      ((define) (add-define stmt))
	      ((undef) (rem-define (cadr stmt)))
	      ((error) (p-err "error: #error ~A" (cadr stmt)))
	      ((pragma) #t) ;; ignore for now
	      (else (error "bad cpp flow stmt"))))

	  (define (eval-cpp-stmt-1/file stmt)
	    (case (car stmt)
	      ((if) (cpi-push))
	      ((elif else) (cpi-shift))
	      ((endif) (cpi-pop))
	      (else (eval-cpp-stmt-2/file stmt))))
	    
	  (define (eval-cpp-stmt-2/file stmt)
	    ;; eval non-control flow
	    (case (car stmt)
	      ;; includes
	      ((include)
	       (let* ((file (inc-stmt->file stmt))
		      (path (inc-file->path file)))
		 (cond
		  ((apply-helper file)) ; use helper
		  ((not path) (p-err "not found: ~S" file)) ; file not found
		  ((with-input-from-file path run-parse) => ; include tree
		   (lambda (tree) (for-each add-define (xp1 tree))))
		  (else (p-err "included from ~S" path)))))
	      ((define) (add-define stmt))
	      ((undef) (rem-define (cadr stmt)))
	      ((error) #f)
	      ((pragma) #t) ;; need to work this
	      (else (error "bad cpp flow stmt"))))
	    
	  (define (eval-cpp-stmt stmt)
	    (with-throw-handler
	     'cpp-error
	     (lambda ()
	       (case mode
		 ((code) (eval-cpp-stmt-1/code stmt))
		 ((file) (eval-cpp-stmt-1/file stmt))))
	     (lambda (key fmt . rest)
	       (report-error fmt rest)
	       (throw 'c99-error "CPP error"))))

	  ;; Composition of @code{read-cpp-line} and @code{eval-cpp-line}.
	  (define (read-cpp-stmt ch)
	    (and=> (read-cpp-line ch) cpp-line->stmt))

	  (define (read-token)
	    (let iter ((ch (read-char)))
	      (cond
	       ((eof-object? ch)
		(if (pop-input) (iter (read-char)) (assc-$ '($end . ""))))
	       ((eq? ch #\newline) (set! bol #t) (iter (read-char)))
	       ((char-set-contains? c:ws ch) (iter (read-char)))
	       (bol
		(set! bol #f)
		(cond ;; things that depend on bol only
 		 ((read-comm ch #t) => assc-$)
		 ((read-cpp-stmt ch) =>
		  (lambda (stmt)
		    (eval-cpp-stmt stmt)
		    (case mode
		      ((code) (iter (read-char)))
		      ((file) (assc-$ `(cpp-stmt . ,stmt))))))
		 (else (iter ch))))
	       ((read-ident ch) =>
		(lambda (name)
		  (let ((symb (string->symbol name)))
		    (cond
		     ((and (x-def? name mode)
			   (expand-cpp-macro-ref name (cpi-defs info)))
		      => (lambda (st)
			   (push-input (open-input-string st))
			   (iter (read-char))))
		     ((assq-ref keytab symb)
		      => (lambda (t) (cons t name)))
		     ((typename? name)
		      (cons (assq-ref symtab 'typename) name))
		     (else
		      (cons (assq-ref symtab '$ident) name))))))
	       ((read-c-num ch) => assc-$)
	       ((read-c-string ch) => assc-$)
	       ((read-c-chlit ch) => assc-$)
	       ((read-comm ch #f) => assc-$)
	       ((read-chseq ch) => identity)
	       ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	       ((eqv? ch #\\) ;; C allows \ at end of line to continue
		(let ((ch (read-char)))
		  (cond ((eqv? #\newline ch) (iter (read-char))) ;; extend line
			(else (unread-char ch) (cons #\\ "\\"))))) ;; parse err
	       (else (cons ch (string ch))))))

	  ;; Loop between reading tokens and skipping tokens via CPP logic.
	  (let iter ((pair (read-token)))
	    (case (car ppxs)
	      ((keep) ;;(simple-format #t "lx=>~S\n" pair)
	       pair)
	      ((skip-done skip-look skip)
	       (iter (read-token)))
	      (else (error "coding error"))))
	  )))))

;; --- last line ---
