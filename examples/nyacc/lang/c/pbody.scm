;;; lang/c/pbody.scm
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
  (defines cpi-defs set-cpi-defs!)	; #defines
  (incdirs cpi-incs set-cpi-incs!)	; #includes
  (typnams cpi-tyns set-cpi-tyns!)	; typedef names
  (typdcls cpi-tdls set-cpi-tdls!)	; typedef decls
  )

(define (make-cpi defines incdirs)
  (let* ((cpi (make-cpi-1)))
    (set-cpi-defs! cpi (if defines defines '()))
    (set-cpi-incs! cpi (if incdirs incdirs '()))
    (set-cpi-tyns! cpi '())
    (set-cpi-tdls! cpi '())
    cpi))

(define *info* (make-fluid #f))

;; @item typename? name
;; Called by lexer to determine if symbol is a typename.
(define (typename? name)
  (let ((info (fluid-ref *info*)))
    (memq name (cpi-tyns info))))

;; @item add-typename name
;; Helper for @code{save-typenames}.
(define (add-typename name)
  ;;(simple-format #t "add-typename: ~S\n" name)
  (let ((info (fluid-ref *info*)))
    (set-cpi-tyns! info (cons (string->symbol name) (cpi-tyns info)))))

;; @item find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
(define find-new-typenames
  (let ((sxtd (sxpath '(stor-spec typedef)))
	(sxid (sxpath '(init-declr ident *text*))))
    (lambda (decl)
      (cond
       ((not (eq? 'decl (car decl))) '())
       ((< (length decl) 3) '())
       (else (let* ((spec-list (list-ref decl 1))
		    (init-list (list-ref decl 2)))
	       (if (pair? (sxtd spec-list)) (sxid init-list) '())))))))

;; @item add-typdecl name decl
;; Helper for @code{save-typenames}.
;; Adds type declaration.
(define (add-typedecl name decl)
  (let ((info (fluid-ref *info*)))
    (set-cpi-tdls! info (cons (cons name decl) (cpi-tdls info)))))

;; @item find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
(define find-new-typedecls
  (let ((sxtd (sxpath '(stor-spec typedef)))
	(sxid (sxpath '(init-declr ident *text*))))
    (lambda (decl)
      (cond
       ((not (eq? 'decl (car decl))) '())
       ((< (length decl) 3) '())
       (else (let* ((spec-list (list-ref decl 1))
		    (init-list (list-ref decl 2)))
	       ;;(simple-format #t "~S => ~S\n" decl dcl)
	       (if (pair? (sxtd spec-list))
		   (map (lambda (tid) (cons tid spec-list)) (sxid init-list))
		   '())))))))

;; @item save-typenames decl
;; Save the typenames for the lexical analyzer and return the decl.
(define (save-typenames-only decl)
  ;; This finds typenames using @code{find-new-typenames} and adds via
  ;; @code{add-typename}.  Then return the decl.
  (for-each add-typename (find-new-typenames decl))
  decl)

(define (save-typenames/decls decl)
  ;; This finds typenames using @code{find-new-typenames} and adds via
  ;; @code{add-typename}.  Then return the decl.
  (for-each
   (lambda (d-pair)
     (add-typename (car d-pair))
     (add-typedecl (car d-pair) (cdr d-pair)))
   (find-new-typedecls decl))
  decl)

(define save-typenames save-typenames-only)

;; ------------------------------------------------------------------------

;; @item read-cpp-line ch => #f | (cpp-xxxx)??
;; Given if ch is #\# read a cpp-statement
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
	 (else (iter (cons ch cl) (read-char)))))))

;; @item find-file-in-dirl file dirl => path
(define (find-file-in-dirl file dirl)
  (let iter ((dirl dirl))
    (if (null? dirl) #f
	(let ((p (string-append (car dirl) "/" file)))
	  (if (access? p R_OK) p (iter (cdr dirl)))))))

;; @item gen-c-lexer => thunk
;; Generate a context-sensitive lexer for the C language.
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
    (lambda ()
      (let ((bol #t)		       ; begin-of-line condition
	    (skip (list #f))	       ; CPP skip-input stack
	    (info (fluid-ref *info*))) ; assume make and run in same thread
	;; Return the first (tval lval) pair not excluded by the CPP.
	(lambda ()
      
	  (define (add-define tree)
	    (let* ((tail (cdr tree))
		   (name (string->symbol (car (assq-ref tail 'name))))
		   (args (assq-ref tail 'args))
		   (repl (car (assq-ref tail 'repl)))
		   (cell (cons name (if args (cons args repl) repl))))
	      (set-cpi-defs! info (cons cell (cpi-defs info)))))
	  
	  (define (exec-cpp line)
	    ;; Parse the line into a CPP stmt, execute it, and return it.
	    (let* ((stmt (parse-cpp-line line)) (cppx (cdr stmt)))
	      (case (car cppx)
		((include)
		 (let* ((parg (cadr cppx)) (leng (string-length parg))
			(file (substring parg 1 (1- leng)))
			(path (find-file-in-dirl file (cpi-incs info)))
			(tree (with-input-from-file path run-parse)))
		   (for-each add-define (xp1 tree)) ; add def's 
		   ;; Attach tree onto "include" statement:
		   ;;(set! stmt (append stmt (list tree)))
		   ))
		((define)
		 (add-define cppx))
		((if)
		 (let ((val (eval-cpp-expr (cadr cppx) (cpi-defs info))))
		   (cond ((not val)
			  (fmterr "*** unresolved: ~S" (cadr cppx)))
			 ((zero? val) (set! skip (cons #t skip)))
			 (else (set! skip (cons #f skip))))))
		((endif)
		 (set! skip (cons 'd skip))))
	      stmt))
	  
	  (define (read-token)
	    ;;(define (echo lp) (simple-format #t "tok=~S\n" lp) lp)
	    (define (echo lp) lp)
	    (echo
	     (let iter ((ch (read-char)))
	       (cond
		((eof-object? ch) (assc-$ '($end . "")))
		((eq? ch #\newline) (set! bol #t) (iter (read-char)))
		((char-set-contains? c:ws ch) (iter (read-char)))
		(bol
		 (cond
		  ((read-comm ch) =>
		   (lambda (c) (assc-$ (cons '$lone-comm (cdr c)))))
		  ((read-cpp-line ch) => (lambda (s) (assc-$ (exec-cpp s))))
		  (else (set! bol #f) (iter ch))))
		((read-ident ch) =>
		 (lambda (str)
		   (let ((sym (string->symbol str)))
		     (cond ((assq-ref keytab sym) => (lambda (t) (cons t str)))
			   ((typename? sym)
			    (cons (assq-ref symtab 'typename) str))
			   (else (cons (assq-ref symtab '$ident) str))))))
		((read-c-num ch) => assc-$)
		((read-c-string ch) => assc-$)
		((read-comm ch) =>
		 (lambda (c) (assc-$ (cons '$code-comm (cdr c)))))
		;;((and (simple-format #t "chs=>~S\n" (read-chseq ch)) #f))
		((read-chseq ch) => identity)
		((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
		(else (cons ch (string ch))))))
	    )

	  ;; Loop between reading tokens and skipping tokens.
	  ;; The use of "delayed pop" is not clean IMO.  Cleaner way?
	  (let loop ((pair (read-token)))
	    (case (car skip)
	      ((#f) pair)
	      ((#t) (loop (read-token)))
	      ((d) (let ((fl (cadr skip))) ;; d for endif: delayed pop
		     (set! skip (cddr skip))
		     (if fl (loop (read-token)) pair))))))))))

;; --- last line
