;;; nyacc/lang/c99/parser.scm - C parser execution

;; Copyright (C) 2015-2026 Matthew Wette
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

(define-module (nyacc lang c99 parser)
  #:export (parse-c99 parse-c99x gen-c99-lexer gen-c99x-lexer gen-c-lexer)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang util)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc lang c99 mach)
  #:re-export (c99-def-help c99-std-help))

;; === body ==========================

(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules ((srfi srfi-1) #:select (fold fold-right append-reverse)))
(use-modules ((srfi srfi-9) #:select (define-record-type)))
(use-modules (ice-9 pretty-print))      ; for debugging
(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)

;; C parser info
(define-record-type cpi
  (make-cpi-1)
  cpi?
  (debug cpi-debug set-cpi-debug!)      ; debug #t #f
  (shinc cpi-shinc set-cpi-shinc!)      ; show includes
  (defines cpi-defs set-cpi-defs!)      ; #defines
  (incdirs cpi-incs set-cpi-incs!)      ; #includes
  (ptl cpi-ptl set-cpi-ptl!)            ; parent typename list
  (ctl cpi-ctl set-cpi-ctl!)            ; current typename list
  (tna cpi-itn set-cpi-itn!)            ; inhibit typename
  (inc-tynd cpi-itynd set-cpi-itynd!)   ; a-l of incfile => typenames
  (inc-defd cpi-idefd set-cpi-idefd!)   ; a-l of incfile => defines
  (blev cpi-blev set-cpi-blev!))        ; curr brace/block level

;;.@deffn Procedure split-cppdef defstr => form | #f
;; Convert string @var{defstr} to CPP def for use in macro expansion.
;; If a macro replacment is @code{"#f"}, then effectively it has been
;; @code{undef}'d.  The replacment part is tokenized.
;; @* Examples:
;; @example
;; "ABC=123" => '("ABC" ($fixed . "123"))
;; "SUM(X,Y)=X+Y" => '("SUM" ("X" "Y") ($ident . "X") (#\+ . "+") ($ident . "Y"))
;; "FOO" => '("FOO" ($fixed . "1"))
;; @end example
;; @end deffn
(define (split-cppdef defstr)
  (let* ((ex (string-index defstr #\=))
         (lhs (if ex (substring defstr 0 ex) defstr))
         (rhs (if ex (substring defstr (1+ ex)) "1"))
         (lpx (string-index lhs #\())
         (rpx (string-index lhs #\))))
    ;; We will tokenize on demand in cpp.scm.
    (cond
     ((string=? rhs "#f") (cons lhs #f))
     (lpx (cons* (substring lhs 0 lpx)
                 (string-split (substring lhs (1+ lpx) rpx) #\,)
                 rhs))
     (else (cons lhs
                 rhs)))))

;; @deffn Procedure make-cpi debug defines incdirs inchelp
;; I think there is a potential bug here in that the alist of cpp-defs/helpers
;; should be last-in-first-seen ordered.  Probably helpers low prio.
;; The (CPP) defines can appear as pairs: then they have already been split.
;; (This is used by @code{parse-c99x}.)
;; @end deffn
(define (make-cpi debug shinc defines incdirs inchelp)
  ;; convert inchelp into inc-file->typenames and inc-file->defines
  ;; Any entry for an include file which contains `=' is considered
  ;; a define; otherwise, the entry is a typename.

  (define (split-helper helper)
    (let ((file (car helper)))
      (let loop ((tyns '()) (defs '()) (ents (cdr helper)))
        (cond
         ((null? ents) (values (cons file tyns) (cons file defs)))
         ((string-contains (car ents) "=")
          (loop tyns (cons (split-cppdef (car ents)) defs) (cdr ents)))
         (else (loop (cons (car ents) tyns) defs (cdr ents)))))))

  (define (split-if-needed def tail)
    (cons (if (pair? def) def (split-cppdef def)) tail))

  (let* ((cpi (make-cpi-1)))
    (set-cpi-debug! cpi debug)          ; print states debug
    (set-cpi-shinc! cpi shinc)          ; print includes
    (set-cpi-defs! cpi (fold split-if-needed '() defines)) ; def's as pairs
    (set-cpi-incs! cpi incdirs)         ; list of include dir's
    (set-cpi-ptl! cpi '())              ; list of lists of typenames
    (set-cpi-ctl! cpi '())              ; list of current typenames
    (set-cpi-itn! cpi #f)               ; don't inhibit typename
    (set-cpi-blev! cpi 0)               ; brace/block level
    ;; Break up the helpers into typenames and defines.
    (let loop ((itynd '()) (idefd '()) (helpers inchelp))
      (cond ((null? helpers)
             (set-cpi-itynd! cpi itynd)
             (set-cpi-idefd! cpi idefd))
            (else
             (call-with-values
                 (lambda () (split-helper (car helpers)))
               (lambda (ityns idefs)
                 (loop (cons ityns itynd) (cons idefs idefd) (cdr helpers)))))))
    ;; Assign builtins.
    (and=> (assoc-ref (cpi-itynd cpi) "__builtin")
           (lambda (tl) (set-cpi-ctl! cpi (append (cpi-ctl cpi) tl))))
    (and=> (assoc-ref (cpi-idefd cpi) "__builtin")
           (lambda (dl) (set-cpi-defs! cpi (append (cpi-defs cpi) dl))))
    cpi))

(define *info* (make-parameter #f))

(define cpi-inc-blev!
  (case-lambda
    ((info) (set-cpi-blev! info (1+ (cpi-blev info))))
    (() (cpi-inc-blev! (*info*)))))
(define cpi-dec-blev!
  (case-lambda
    ((info) (set-cpi-blev! info (1- (cpi-blev info))))
    (() (cpi-dec-blev! (*info*)))))
(define cpi-top-blev?
  (case-lambda
    ((info) (zero? (cpi-blev info)))
    (() (cpi-top-blev? (*info*)))))

(define cpi-push
  (case-lambda
    ((info)
     (set-cpi-ptl! info (cons (cpi-ctl info) (cpi-ptl info)))
     (set-cpi-ctl! info '())
     #t)
    (()
     (cpi-push (*info*)))))

(define cpi-pop
  (case-lambda
    ((info)
     (set-cpi-ctl! info (car (cpi-ptl info)))
     (set-cpi-ptl! info (cdr (cpi-ptl info)))
     #t)
    (()
     (cpi-pop (*info*)))))

(define (cpi-push-x)    ;; on #if
  (let ((cpi (*info*)))
    (set-cpi-ptl! cpi (cons (cpi-ctl cpi) (cpi-ptl cpi)))
    (set-cpi-ctl! cpi '())))

(define (cpi-shift-x)   ;; on #elif #else
  (set-cpi-ctl! (*info*) '()))

(define (cpi-pop-x)     ;; on #endif
  (let ((cpi (*info*)))
    (set-cpi-ctl! cpi (append (cpi-ctl cpi) (car (cpi-ptl cpi))))
    (set-cpi-ptl! cpi (cdr (cpi-ptl cpi)))))

;; @deffn {Procedure} typename? name
;; Called by lexer to determine if symbol is a typename.
;; Check current sibling for each generation.
;; @end deffn
(define (typename? name)
  (let ((cpi (*info*)))
    (if (member name (cpi-ctl cpi)) #t
        (let loop ((ptl (cpi-ptl cpi)))
          (if (null? ptl) #f
              (if (member name (car ptl)) #t
                  (loop (cdr ptl))))))))

;; @deffn {Procedure} add-typename name
;; Helper for @code{save-typenames}.
;; @end deffn
(define (add-typename name)
  (let ((cpi (*info*)))
    (set-cpi-ctl! cpi (cons name (cpi-ctl cpi)))))

;; @deffn {Procedure} find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
;; @end deffn
(define (find-new-typenames decl)
  (define (declr->id-name declr)
    (case (car declr)
      ((ident) (sx-ref declr 1))
      ((init-declr) (declr->id-name (sx-ref declr 1)))
      ((comp-declr) (declr->id-name (sx-ref declr 1)))
      ((ary-declr) (declr->id-name (sx-ref declr 1)))
      ((ptr-declr) (declr->id-name (sx-ref declr 2)))
      ((ftn-declr) (declr->id-name (sx-ref declr 1)))
      ((scope) (declr->id-name (sx-ref declr 1)))
      (else
       (pp (car declr))
       (throw 'c99-error "parser.scm: find-new-typenames: ~S" declr))))

  (let* ((spec (sx-ref decl 1))
         (stor (sx-find 'stor-spec spec))
         (id-l (sx-ref decl 2)))
    (if (and stor (eqv? 'typedef (caadr stor)))
        (let loop ((res '()) (idl (cdr id-l)))
          (if (null? idl) res
              (loop (cons (declr->id-name (sx-ref (car idl) 1)) res)
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

;; (string "abc" "def") -> (string "abcdef")
;; In the case that declaration-specifiers only returns a list of
;; attribute-specifiers then this has to be an empty-statemnet with
;; attributes.  See:
;;   https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Statement-Attributes.html
(define (XXX-only-attr-specs? specs)
  (let loop ((specs specs))
    (cond
     ((null? specs) #t)
     ((not (eqv? 'attributes (sx-tag (car specs)))) #f)
     (else (loop (cdr specs))))))

;; ------------------------------------------------------------------------

(define (c99-err . args)
  (apply throw 'c99-error args))

;; @deffn {Procedure} read-cpp-line ch => #f | (cpp-xxxx)??
;; Given if ch is #\# read a cpp-statement.
;; The standard implies that comments are tossed here but we keep them
;; so that they can end up in the pretty-print output.
;; @end deffn
(define (read-cpp-line ch)
  (if (not (eq? ch #\#)) #f
      (let loop ((cl '()) (ch (read-char)))
        (cond
         ;;((eof-object? ch) (throw 'cpp-error "CPP lines must end in newline"))
         ((eof-object? ch) (reverse-list->string cl))
         ((eq? ch #\newline) (unread-char ch) (reverse-list->string cl))
         ((eq? ch #\\)
          (let ((c2 (read-char)))
            (if (eq? c2 #\return)
                (let ((c3 (read-char)))
                  (if (eq? c3 #\newline)
                      (loop cl (read-char))
                      (loop (cons* c3 c2 ch cl) (read-char))))
                (if (eq? c2 #\newline)
                    (loop cl (read-char))
                    (loop (cons* c2 ch cl) (read-char))))))
         ((eq? ch #\/) ;; swallow comments, even w/ newlines
          (let ((c2 (read-char)))
            (cond
             ((eqv? c2 #\*)
              (let loop2 ((cl2 (cons* #\* #\/ cl)) (ch (read-char)))
                (cond
                 ((eq? ch #\*)
                  (let ((c2 (read-char)))
                    (if (eqv? c2 #\/)
                        (loop (cons* #\/ #\* cl2) (read-char)) ;; keep comment
                        (loop2 (cons #\* cl2) c2))))
                 (else
                  (loop2 (cons ch cl2) (read-char))))))
             (else
              (loop (cons #\/ cl) c2)))))
         (else (loop (cons ch cl) (read-char)))))))

(define (def-xdef? name mode)
  (not (eqv? mode 'file)))


;; @deffn {Procedure} make-c99-lexer-generator match-table raw-parser => proc
;; This generates a procedure which has the signature
;; @example
;; proc [#:mode mode] [#:xdef? proc] => procedure
;; @end example
;; to be passed to the c99 parsers.
;; The proc will generate a context-sensitive lexer for the C99 language.
;; The arg @var{match-table} is an element of a specification returned
;; by @code{make-lalr-spec} or machine generated by @code{make-lalr-machine}.
;; The argument @var{raw-parse} must be ...
;; The generated
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
(define (make-c99-lexer-generator match-table raw-parser)

  (define (getdefs stmts)               ; extract defines
    (fold-right
     (lambda (stmt seed)
       (if (and (eqv? 'cpp-stmt (sx-tag stmt))
                (eqv? 'define (sx-tag (sx-ref stmt 1))))
           (cons (sx-ref stmt 1) seed)
           seed))
     '() stmts))

  (let* ((ident-like? (make-ident-like-p read-c-ident))
         (strtab (filter-mt string? match-table)) ; strings in grammar
         (kwstab (filter-mt ident-like? strtab))  ; keyword strings =>
         (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
         (chrseq (remove-mt ident-like? strtab))  ; character sequences
         (cs-rmap (chseq->canon-map chrseq))     ; remap
         (read-chseq (make-chseq-reader cs-rmap))
         (cs-umap (chseq->canon-unmap chrseq))     ; unmap
         (symtab (append (filter-mt symbol? match-table) cs-umap))
         (t-ident (assq-ref symtab '$ident))
         (t-typename (assq-ref symtab 'typename))
         (tkl '()))

    ;; mode: 'code|'file|'decl
    ;; xdef?: (proc name mode) => #t|#f  : do we expand #define?
    ;; (gen-lexer* (#:key (mode 'code) xdef? show-incs) => gettok
    (define* (gen-lexer #:key (mode 'code) xdef? show-incs)

      (define (run-parse)
        (let ((info (*info*)))
          (raw-parser (gen-lexer #:mode 'decl #:show-incs (cpi-shinc info))
                      #:debug (cpi-debug info))))

      (let ((bol #t)             ; begin-of-line condition
            (ppxs (list 'keep))  ; CPP execution state stack
            (info (*info*))      ; info shared w/ parser
            (x-def? (cond ((procedure? xdef?) xdef?)
                          ((eq? xdef? #t) (lambda (n m) #t))
                          (else def-xdef?))))

        ;; Return the first (tval . lval) pair not excluded by the CPP.
        (lambda ()

          (define (add-define tree)
            (let* ((tail (cdr tree))
                   (name (car (assq-ref tail 'name)))
                   (args (assq-ref tail 'args))
                   (rtxt (car (assq-ref tail 'repl)))
                   (rtkl (tokenize-cpp-string rtxt))
                   (cell (cons name (if args (cons args rtkl) rtkl))))
              (set-cpi-defs! info (cons cell (cpi-defs info)))))

          (define (rem-define name)
            (set-cpi-defs! info (acons name #f (cpi-defs info))))

          (define (apply-helper file)
            ;; file will include <> or "", need to strip
            (let* ((tyns (assoc-ref (cpi-itynd info) file))
                   (defs (assoc-ref (cpi-idefd info) file)))
              (when tyns
                (for-each add-typename tyns)
                (set-cpi-defs! info (append defs (cpi-defs info))))
              tyns))

          (define (inc-stmt->file-spec stmt) ;; retain <> or ""
            (let* ((arg (cadr stmt)))
              (if (ident-like? arg) ;; #include MYFILE
                  (macro-expand-text arg (cpi-defs info))
                  arg)))

          (define (file-spec->file spec)
            (substring/shared spec 1 (1- (string-length spec))))

          (define (inc-file-spec->path spec next)
            (find-incl-in-dirl spec (cpi-incs info) next))

          (define (code-if stmt)
            (case (car ppxs)
              ((skip-look skip-done skip) ;; don't eval if excluded
               (set! ppxs (cons 'skip ppxs)))
              (else
               (let* ((defs (cpi-defs info))
                      (val (eval-cpp-cond-text (cadr stmt) defs
                                               #:inc-dirs (cpi-incs info))))
                 (if (not val) (c99-err "unresolved: ~S" (cadr stmt)))
                 (if (eq? 'keep (car ppxs))
                     (if (zero? val)
                         (set! ppxs (cons 'skip-look ppxs))
                         (set! ppxs (cons 'keep ppxs)))
                     (set! ppxs (cons 'skip-done ppxs))))))
            stmt)

          (define (code-elif stmt)
            (case (car ppxs)
              ((skip) #t) ;; don't eval if excluded
              (else
               (let* ((defs (cpi-defs info))
                      (val (eval-cpp-cond-text (cadr stmt) defs
                                               #:inc-dirs (cpi-incs info))))
                 (if (not val) (c99-err "unresolved: ~S" (cadr stmt)))
                 (case (car ppxs)
                   ((skip-look) (if (not (zero? val)) (set-car! ppxs 'keep)))
                   ((keep) (set-car! ppxs 'skip-done))))))
            stmt)

          (define (code-else stmt)
            (case (car ppxs)
              ((skip-look) (set-car! ppxs 'keep))
              ((keep) (set-car! ppxs 'skip-done)))
            stmt)

          (define (code-endif stmt)
            (set! ppxs (cdr ppxs))
            stmt)

          (define* (eval-cpp-incl/here stmt #:optional next) ;; => stmt
            (let* ((spec (inc-stmt->file-spec stmt))
                   (file (file-spec->file spec))
                   (path (inc-file-spec->path spec next)))
              (if show-incs (sf "include ~A => ~S\n" spec path))
              (cond
               ((apply-helper file) stmt)
               ((not path) (c99-err "not found: ~S" file))
               (else (set! bol #t)
                     (push-input (open-input-file path))
                     (if path (sx-attr-add stmt 'path path) stmt)))))

          (define* (eval-cpp-incl/tree stmt #:optional next) ;; => stmt
            ;; include file as a new tree
            (let* ((spec (inc-stmt->file-spec stmt))
                   (file (file-spec->file spec))
                   (path (inc-file-spec->path spec next)))
              (if show-incs (sf "include ~A => ~S\n" spec path))
              (cond
               ((apply-helper file) stmt)
               ((not path) (c99-err "not found: ~S" file))
               ((with-input-from-file path run-parse) =>
                (lambda (tree) ;; add tree
                  (for-each add-define (getdefs tree))
                  (append (if path (sx-attr-add stmt 'path path) stmt)
                          (list tree)))))))

          #|
          (define (eval-pragma stmt)
            (define wspace (list->char-set '(#\space #\tab)))
            (define (skip-ws ch)
              (cond
               ((eof-object? ch) ch)
               ((char-set-contains? wspace ch) (skip-ws (read-char)))
               (else ch)))

            (define (pop_macro key)
              (let loop ((head '()) (tail (cpi-defs info)) (val #t))
                (cond
                 ((null? tail) (reverse head))
                 ((equal? (caar tail) key)
                  (if val
                      (loop head (cdr tail) #f)
                      (if (cdar tail)
                          (set-cpi-defs! info (append-reverse head tail))
                          (loop head (cdr tail) val))))
                 (else (loop head (cdr tail) val)))))

            (with-input-from-string (cadr stmt)
              (lambda ()
                (let ((key (read-c-ident (read-char))))
                  (when key
                    (case (string->symbol key)
                      ((pop_macro) (pop_macro key))
                      (else #f)))))))
          |#

          (define (eval-cpp-stmt/code stmt) ;; => stmt
            (case (car stmt)
              ((if) (code-if stmt))
              ((elif) (code-elif stmt))
              ((else) (code-else stmt))
              ((endif) (code-endif stmt))
              (else
               (if (eqv? 'keep (car ppxs))
                   (case (car stmt)
                     ((include) (eval-cpp-incl/here stmt))
                     ((include-next) (eval-cpp-incl/here stmt 'next))
                     ((define) (add-define stmt) stmt)
                     ((undef) (rem-define (cadr stmt)) stmt)
                     ((error) (c99-err "error: #error ~A" (cadr stmt)))
                     ((warning) (report-error "warning: ~A" (cdr stmt)))
                     ((pragma) stmt)
                     ;;((pragma) (eval-pragma stmt) stmt)
                     ((line) stmt)
                     (else
                      (throw 'c99-error "eval-cpp-stmt/code: ~S" stmt)))
                   stmt))))

          (define (eval-cpp-stmt/decl stmt) ;; => stmt
            (case (car stmt)
              ((if) (code-if stmt))
              ((elif) (code-elif stmt))
              ((else) (code-else stmt))
              ((endif) (code-endif stmt))
              (else
               (if (eqv? 'keep (car ppxs))
                   (case (car stmt)
                     ((include)         ; use tree unless inside braces
                      (if (cpi-top-blev? info)
                          (eval-cpp-incl/tree stmt)
                          (eval-cpp-incl/here stmt)))
                     ((include-next)    ; gcc extension
                      (if (cpi-top-blev? info)
                          (eval-cpp-incl/tree stmt 'next)
                          (eval-cpp-incl/here stmt 'next)))
                     ((define) (add-define stmt) stmt)
                     ((undef) (rem-define (cadr stmt)) stmt)
                     ((error) (c99-err "error: #error ~A" (cadr stmt)))
                     ((warning) (report-error "warning: ~A" (cdr stmt)) stmt)
                     ((pragma) stmt) ;; ignore for now
                     ((line) stmt)
                     (else
                      (throw 'c99-error "eval-cpp-stmt/decl: ~S" stmt)))
                   stmt))))

          (define (eval-cpp-stmt/file stmt) ;; => stmt
            (case (car stmt)
              ((if) (cpi-push-x) stmt)
              ((elif else) (cpi-shift-x) stmt)
              ((endif) (cpi-pop-x) stmt)
              ((include) (eval-cpp-incl/tree stmt))
              ((define) (add-define stmt) stmt)
              ((undef) (rem-define (cadr stmt)) stmt)
              ((error) stmt)
              ((warning) stmt)
              ((pragma) stmt)
              ((line) stmt)
              (else
               (throw 'c99-error "eval-cpp-stmt/file: ~S" stmt))))

          ;; Maybe evaluate the CPP statement.
          (define (eval-cpp-stmt stmt)
            (with-throw-handler
                'cpp-error
              (lambda ()
                (case mode
                  ((code) (eval-cpp-stmt/code stmt))
                  ((decl) (eval-cpp-stmt/decl stmt))
                  ((file) (eval-cpp-stmt/file stmt))
                  (else (throw 'c99-error "eval-cpp-stmt: coding error"))))
              (lambda (key fmt . rest)
                (report-error fmt rest)
                (throw 'c99-error "CPP error"))))

          ;; Predicate to determine if we pass the cpp-stmt to the parser.
          ;; @itemize
          ;; If code mode, never
          ;; If file mode, all except includes between { }
          ;; If decl mode, only defines and includes outside {}
          ;; @end itemize
          (define (pass-cpp-stmt stmt)
            (if (and (pair? stmt) (eq? 'pragma (car stmt)))
                (if (eq? mode 'file)
                    `(cpp-stmt ,stmt)
                    `($pragma . ,(cadr stmt)))
                (case mode
                  ((code) #f)
                  ((decl)
                   (and (cpi-top-blev? info)
                        (memq (car stmt) '(include include-next define))
                        `(cpp-stmt . ,stmt)))
                  ((file)
                   (and (or (cpi-top-blev? info)
                            (not (memq (car stmt) '(include include-next))))
                        `(cpp-stmt . ,stmt)))
                  (else (throw 'c99-error "pass-cpp-stmt: coding error")))))

          ;; Composition of @code{read-cpp-line} and @code{eval-cpp-line}.
          (define (read-cpp-stmt ch)
            (and=> (read-cpp-line ch) cpp-line->stmt))

          (define (make-loc-info)
            (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
                  (ln (1+ (port-line (current-input-port)))))
              `((line . ,ln) (filename . ,fn))))

          (define (w/ ss res) (set-source-properties! res ss) res)

          (define (read-token)
            (let loop ((ch (read-char)) (ss #f)) ;; ss is source loc
              (cond
               ((not ss) (loop ch (make-loc-info)))
               ((eof-object? ch)
                (if (pop-input)
                    (loop (read-char) ss)
                    (if (pair? (cdr ppxs))
                        (c99-err "unterminated #if")
                        (w/ ss (cons '$end "#<eof>")))))
               ((eq? ch #\newline) (set! bol #t) (loop (read-char) #f))
               ((char-set-contains? c:ws ch) (loop (read-char) #f))
               (bol
                (set! bol #f)
                (cond ;; things that require bol
                 ((read-c-comm ch #t #:skip-prefix #t) =>
                  (lambda (p) (w/ ss p)))
                 ((read-cpp-stmt ch) =>
                  (lambda (stmt)
                    (cond ((pass-cpp-stmt (eval-cpp-stmt stmt)) =>
                           (lambda (p) (w/ ss p)))
                          (else (loop (read-char) ss)))))
                 (else (loop ch ss))))
               ((read-c-comm ch #f #:skip-prefix #t) =>
                (lambda (p) (w/ ss p)))
               ((and (not (eq? (car ppxs) 'keep)) (eq? mode 'code))
                (loop (read-char) ss))
               ((read-c-mclit ch) =>    ; mclit over chlit for gobject bd
                (lambda (p) (w/ ss p)))
               ((read-c-ident ch) =>
                (lambda (name) (cons '$ident name)))
               ((read-c-num ch) => (lambda (p) (w/ ss p)))
               ((read-c-string ch) => (lambda (p) (w/ ss p)))
               ((and (char=? ch #\{) (eqv? 'keep (car ppxs))
                     (cpi-inc-blev! info) #f) #f) ; incr brace level
               ((and (char=? ch #\}) (eqv? 'keep (car ppxs))
                     (cpi-dec-blev! info) #f) #f) ; decr brace level
               ((read-chseq ch) => (lambda (p) (w/ ss p)))
               ((eqv? ch #\\) ;; C allows \ at end of line to continue
                (if (eqv? (read-char) #\newline)
                    (loop (read-char) ss)
                    (throw 'c99-error "bad use of \\")))
               (else (w/ ss (cons ch (string ch)))))))

          ;; Loop between reading tokens and skipping tokens via CPP logic.
          ;; ptl: processed token list, rtl: raw token list
          ;; (lexer #:mode mode #:xdef? xdef? #:show-incs show-incs)
          (define (encode-token tok)
            (let ((key (car tok)) (val (cdr tok)))
              (cond
               ((eq? '$ident key)
                (let ((symb (string->symbol val)))
                  (cond
                   ((typename? val) (cons-source tok t-typename val))
                   ((assq-ref keytab symb)
                    => (lambda (v) (cons-source tok v val)))
                   (else (cons-source tok t-ident val)))))
               ((symbol? key) (cons-source tok (assq-ref symtab key) val))
               (else (error "parser.scm: coding error")))))

            (if (pair? tkl)
              (let ((tok (car tkl)))
                (set! tkl (cdr tkl))
                (encode-token tok))
              (let loop ((token (read-token)))
                (case (car ppxs)
                  ((keep)
                   (cond
                    ((eq? '$ident (car token))
                     (let ((mx (expand-cpp-macro-ref
                                (cdr token) (cpi-defs info))))
                       (cond
                        ((not mx) (encode-token token))
                        ((null? mx) (loop (read-token)))
                        (else (set! tkl (cdr mx)) (encode-token (car mx))))))
                    (else (encode-token token))))
                  ((skip-done skip-look skip) (loop (read-token)))
                  (else (throw 'c99-error "parser.scm: coding error"))))))))

    gen-lexer))

;; ===================================

;; Routines to process specifier-lists and declarators, indended
;; to provide option to convert attribute-specifiers elements into
;; SXML attributes.  See move-attributes in util.scm.
;;(define process-specs identity)
;;(define process-declr identity)
(define (process-specs exp) (move-attributes exp))
(define (process-declr exp) (move-attributes exp))

(define (make-user-hook mtab)
  (let ((typename (assq-ref mtab 'typename))
        (ident (assq-ref mtab '$ident)))
    (lambda (tal tok stk)
      (and (eq? tok typename)
           (or (assq-ref tal tok) (assq-ref tal ident))))))

;; === file parser ====================

(define c99-tables
  (map (lambda (key) (cons key (assq-ref c99-mach key)))
       '(mtab ntab len-v rto-v pat-v)))
(define c99-act-v (assq-ref c99-mach 'act-v))
(define c99-mtab (assq-ref c99-mach 'mtab))

(define c99-raw-parser
  (make-lalr-parser
   (acons 'act-v c99-act-v c99-tables)
   #:skip-if-unexp '($lone-comm $code-comm $pragma cpp-stmt)
   #:user-hook (make-user-hook (assq-ref c99-tables 'mtab))))

(define gen-c99-lexer
  (make-c99-lexer-generator c99-mtab c99-raw-parser))

;; @deffn {Procedure} parse-c99 [options]
;; where options are
;; @table code
;; @item typename-list
;; a list of typenames (as strings)
;; @item #:cpp-defs @i{defs-list}
;; @i{defs-list} is a list of strings where each string is of the form
;; @i{NAME} or @i{NAME=VALUE}.
;; @item #:inc-dirs @i{dir-list}
;; @{dir-list} is a list of strings of paths to look for directories.
;; @item #:inc-help @i{helpers}
;; @i{helpers} is an a-list where keys are include files (e.g.,
;; @code{"stdint.h"}) and the value is a list of type aliases or CPP define
;; (e.g., @code{"foo_t" "FOO_MAX=3"}).
;; @item #:mode @i{mode}
;; @i{mode} is one literal @code{'code}, @code{'file}, or @code{'decl}.
;; The default mode is @code{'code}.
;; @item #:return-defs @i{bool}
;; Return two values: the parse tree and the list of CPP definitions.
;; @item #:debug @i{bool}
;; a boolean which if true prints states from the parser
;; @end table
;; @example
;; (with-input-from-file "abc.c"
;;   (parse-c #:cpp-defs '("ABC=123"))
;;            #:inc-dirs '(("." "./incs" "/usr/include"))
;;            #:inc-help (append '("myinc.h" "foo_t" "bar_t") c99-std-help)
;;            #:mode 'file))
;; @end example
;; Note: for @code{file} mode user still needs to make sure CPP conditional
;; expressions can be fully evaluated, which may mean adding compiler generated
;; defines (e.g., using @code{gen-cpp-defs}).
;; @end deffn
(define* (parse-c99 #:optional (tyns '())
                    #:key
                    (cpp-defs '())          ; CPP defines
                    (inc-dirs '())          ; include dirs
                    (inc-help c99-def-help) ; include helpers
                    (mode 'code)        ; mode: 'file, 'code or 'decl
                    (xdef? #f)          ; pred to determine expand
                    (show-incs #f)      ; show include files
                    (return-defs #f)    ; return (values tree defs)
                    (debug #f))         ; debug
  (let ((info
         (make-cpi debug show-incs cpp-defs (cons "." inc-dirs) inc-help)))
    (set-cpi-ptl! info (cons tyns (cpi-ptl info)))
    (parameterize ((*info* info)
                   (*input-stack* '()))
      (catch 'c99-error
        (lambda ()
          (catch 'nyacc-error
            (lambda () (let ((sx (c99-raw-parser
                                  (gen-c99-lexer #:mode mode
                                                 #:xdef? xdef?
                                                 #:show-incs show-incs)
                                  #:debug debug)))
                         (if return-defs (values sx (cpi-defs info)) sx)))
            (lambda (key fmt . args) (apply throw 'c99-error fmt args))))
        (lambda (key fmt . args)
          (report-error fmt args)
          (if return-defs (values #f '()) #f))))))

;; === expr parser ====================

(use-modules (nyacc lang c99 mach))
(define c99x-tables
  (map (lambda (key) (cons key (assq-ref c99x-mach key)))
       '(mtab ntab len-v rto-v pat-v)))
(define c99x-act-v (assq-ref c99x-mach 'act-v))
(define c99x-mtab (assq-ref c99x-mach 'mtab))

(define c99x-raw-parser
  (make-lalr-parser
   (acons 'act-v c99x-act-v c99x-tables)
   #:skip-if-unexp '($lone-comm $code-comm $pragma)
   #:user-hook (make-user-hook (assq-ref c99x-tables 'mtab))))

(define gen-c99x-lexer
  (make-c99-lexer-generator c99x-mtab c99x-raw-parser))

;; @deffn {Procedure} parse-c99x string [typenames] [options]
;; where @var{string} is a string C expression, @var{typenames}
;; is a list of strings to be treated as typenames
;; and @var{options} may be any of
;; @table
;; @item cpp-defs
;; a list of strings to be treated as preprocessor definitions
;; @item xdef?
;; this argument can be a boolean a predicate taking a string argument
;; @item debug
;; a boolean which if true prints states from the parser
;; @end table
;; This needs to be explained in some detail.
;; [tyns '("foo_t")]
;; @end deffn
(define* (parse-c99x expr-string
                     #:optional
                     (tyns '())         ; defined typenames
                     #:key
                     (cpp-defs '())     ; CPP defines
                     (xdef? #f)         ; pred to determine expand
                     (debug #f))        ; debug?
  (let ((info (make-cpi debug #f cpp-defs '(".") '())))
    (set-cpi-ptl! info (cons tyns (cpi-ptl info)))
    (parameterize ((*info* info)
                   (*input-stack* '()))
      (with-input-from-string expr-string
        (lambda ()
         (catch 'c99-error
            (lambda ()
              (catch 'nyacc-error
                (lambda ()
                  (c99x-raw-parser (gen-c99x-lexer #:mode 'code #:xdef? xdef?)
                                   #:debug debug))
                (lambda (key fmt . args)
                  (apply throw 'c99-error fmt args))))
            (lambda (key fmt . rest)
              (report-error fmt rest)
              #f)))))))

;; --- last line ---
