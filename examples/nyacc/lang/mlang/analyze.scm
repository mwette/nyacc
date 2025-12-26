;; analyze.scm - mlang analysis
;;
;; $ guile analyze.scm -s top.m

;;; Notes:
;; 1) We will create a new form
;;    appl: appl-item* where appl-item: script-file | function-file | classdef-
;; 2) also need a universal symbol table
;; 3) also need effects record per node
;; 4) idea: if struct member reference by key not in original struct() abort

;;; Code:

#;(define-module (nyacc lang mlang mltoc) #:export (mlang->c99))

(use-modules (ice-9 format))
(use-modules (ice-9 regex))
(use-modules (ice-9 match))
(use-modules ((srfi srfi-1) #:select (fold last lset-union)))
(use-modules (srfi srfi-9))             ; define-record-type
(use-modules (srfi srfi-11))
(use-modules (srfi srfi-43))
(use-modules (sxml match))
(use-modules (sxml fold))               ; fold-values
(use-modules (sxml xpath))

(use-modules (nyacc foreign cdata))
(use-modules (nyacc lang sx-util))
;;(use-modules (nyacc lang nx-util))

(use-modules (language nx-mlang parser))
(use-modules (language nx-mlang pprint))
(use-modules (language nx-mlang compile-tree-il))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (sf fmt . args) (apply simple-format (current-output-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))
(define (pp exp)
  (pretty-print exp (current-output-port) #:per-line-prefix "  "))


;; ============================================================================
;; info on builtins

(define builtins
  '("abs" "acos" "any" "atan2" "blkdiag" "cart2sph" "cos" "cosd" "cross"
    "deg2rad" "diag" "double" "exp" "eye" "false" "find" "flipud"
    "interp1" "length" "logical" "min" "norm" "ones" "pagemtimes"
    "permute" "pinv" "rad2deg" "reshape" "sign" "sin" "sind"
    "size" "sqrt" "squeeze" "struct" "sum" "vecnorm" "zeros"))

;;(define (builtin-out name)
(define-syntax string-case
  (let-syntax
      ((check-str-case
        (syntax-rules (else)
          ((_ str (set ex ...) kt kf)
           (if (member str 'set) kt kf)))))
    (syntax-rules (else)
      ((_ str (set ex ...) c1 ...)
       (let ((kf (lambda () (string-case str c1 ...))))
         (check-str-case str set (ex ...) kf)))
      ((_ str (else ex ...)) (begin (if #f #f) ex ...))
      ((_ str) (error "no match")))))

;; strategy here
;; 1) convert tree into vector of (tag rx ...)
;;    where rx is an index in the vector, or a string
;; 2) then iterate using vector-fold ...
;;      (lambda (upd ix te infx infy infz)
;;    where ve is the converted tree element and infx infy infz are
;;    vectors of specid ypes of info (see BETTER below);
;;    top is last element

;; number of sub-elements
;; Q: should we retain @ or not?
(define (sxml-count node)
  (cond
   ;;((pair? node) (fold (lambda (e s) (+ (sxml-count e) s)) 1 (sx-tail node)))
   ((pair? node) (fold (lambda (e s) (+ (sxml-count e) s)) 1 (cdr node)))
   ((string? node) 0)
   (else (error "not an sxml node!"))))

;; @deffn {Procedure} sxml->vxml sx-tree => vxml
;; Convert an SXML AST to VXML, a vector where each entry
;; is an element with element nodes replaced by index in the vector.
;; @*Q: should we retain @ or not?
;; @*TODO: add source properties to each entry
;; @end deffn
(define (sxml->vxml sx-tree)
  (let* ((ne (sxml-count sx-tree))
         (vx (make-vector ne)))
    (sferr "sxml->vxml count=~s\n" ne)
    ;; no tail recursion here
    (let loop ((ix 0) (node sx-tree))
      (cond
       ((pair? node)
        (let* ((ln (length node))
               (ev (make-vector ln)))
          (vector-set! vx ix ev)
          (fold
           (lambda (ex el ix)
             (vector-set! ev ex (if (pair? el) ix el))
             (loop ix el))
           (1+ ix) (iota ln) node)))
       (else ix)))
    vx))

;; @deffn {Procedure} vxml->sxml vx-tree [index] => sxml
;; Convert an VXML AST to SXML starting at index @var{index}, default 0.
;; @end deffn
(define* (vxml->sxml vx #:optional (ix 0))
  (let ((ev (vector-ref vx ix)))
    (map (lambda (ex)
           (let ((ee (vector-ref ev ex)))
             (if (integer? ee) (vxml->sxml vx ee) ee)))
         (iota (vector-length ev)))))

;; @deffn {Procedure} display-vxmll vx-tree [port]
;; Convert an VXML AST to SXML starting at index @var{index}, default 0.
;; @end deffn
(define* (display-vxml vx-tree #:optional (port (current-output-port)))
  (vector-for-each
   (lambda (ix vx)
     (simple-format port "~s ~s\n" ix vx)
     #;(cond
      ((< ix 2) (simple-format port "~s ~s\n" ix vx))
      ((= ix (1- (vector-length vx-tree))) (simple-format port "~s ~s\n" ix vx))
      (else #f))
     )
   vx-tree))


;; ============================================================================
;; identify tree

;; need to do a full pass to convert 
;; (ident "name") => (toplevel name)|(lexical name name-123)
;; (ident "name") => (toplevel "name")|(lexical "name" "name-123")

;; loop ((full-tree '()) (next '()) (curr (list "top.m")))
;;   (probe (parse-file (car curr))
;;     if ident convert to lexical|toplevel
;;     function call and implies file, add file to next

(define lvl 0)

(define (push-scope dict)
  (set! lvl (1+ lvl))
  ;;(sferr "lvl=~s\n" lvl)
  (list (cons '@P dict)))

(define (pop-scope dict)
  (set! lvl (1- lvl))
  ;;(sferr "lvl=~s\n" lvl)
  (when (negative? lvl) (error "yuck"))
  (unless (assoc-ref dict '@P) (error "yukkie"))
  (assoc-ref dict '@P))

(define (lookup name dict)
  (cond
   ((not dict) (error "not dict") #f)
   ((assoc-ref dict name))
   ((assoc-ref dict '@P) => (lambda (dict) (lookup name dict)))
   (else #f)))

(define (ensure-lexical ident dict)
  (let* ((name (sx-ref ident 1)))
    (cond
     ((assoc-ref dict name) dict)
     (else 
      (set-car! (cdr ident) (symbol->string (gensym (string-append name "-"))))
      (acons name ident dict)))))

(define (ensure-toplevel ident dict)
  (let ((name (sx-ref ident 1))
        (topd (let loop ((dict dict))
                (or (and=> (assq-ref dict '@P) loop) dict))))
    (unless (assoc-ref topd name)
      (set-cdr! topd (acons name ident (cdr topd))))
    dict))
         
(define (ensure-variable ident dict)
  (let* ((name (sx-ref ident 1)))
    (if (lookup name dict)
        dict
        (if (assq-ref dict '@P)
            (ensure-lexical ident dict)
            (ensure-toplevel ident dict)))))


;; convert (ident ,name) to one of
;;   (toplevel ,name)
;;   (lexical ,name ,name-1234)
;;   (name ,name)
;; for is a case where an ident lives after the form
;; but a new form may use for another purpose, so I
;; create a new lexical in this case
;; if later we see they are used the same, we can merge.
;;
(define* (identify-tree tree #:optional (gbls '((@top . #t))))

  (define (fD tree seed dict)
    ;; Here we update symbol table where identifiers are defined.
    (sx-match tree
      ((fctn-defn (fctn-decl (ident ,name) (ident-list . ,inargs)
                             (ident-list . ,outargs) . ,_) ,stmt-list)
       (let* ((dict (ensure-variable (sx-ref* tree 1 1) dict))
              (dict (push-scope dict))
              (dict (fold ensure-lexical dict inargs))
              (dict (fold ensure-lexical dict outargs))
              (dict (acons '@F #t dict)))
         (values tree '() dict)))
      ((fctn-defn . ,_)
       (error "missed fctn-defn"))
      ((assn (ident ,name) ,rhsx)
       (values tree '() (ensure-variable (sx-ref tree 1) dict)))
      ((assn (aref-or-call (ident ,name) ,expl) ,rhsx)
       (values tree '() (ensure-variable (sx-ref* tree 1 1) dict)))
      ((assn (sel (ident ,name) ,expr) ,rhsx)
       (values tree '() (ensure-variable (sx-ref* tree 1 1) dict)))
      ;;((multi-assn ,
      #;((for (ident ,name) . ,_)
       (values tree '() (ensure-lexical (sx-ref tree 1) (push-scope dict))))
      ((call (ident ,name) . ,_)
       (values tree '() (ensure-toplevel (sx-ref tree 1) dict)))
      ;; ident used as name
      ((sel (ident ,name) ,expr)        ; TODO: src-props
       (values `(sel (name ,name) ,expr) '() dict))
      ((obj-prop (ident ,name) ,expr)        ; TODO: src-props
       (values `(obj-prop (name ,name) ,expr) '() dict))
      ((command "global" . ,names)
       ;; (assert (assoc-ref '@F dict))
       (values tree '() (fold (lambda (name)
                                (let* ((ident `(ident ,name))
                                       (dict (ensure-toplevel ident dict)))
                                  (acons name ident dict)))
                              dict names)))
      (,_
       (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (let ((form (reverse kseed)))        ; TODO: src-props
      ;; Here we update identifiers where they are used.
      (sx-match form
        ((*TOP* ,subform)
         (values subform kdict))
        ((ident ,name)
         (values (cons (or (lookup name kdict) form) seed) kdict))
        ((fctn-defn (fctn-decl (ident ,name) (ident-list . ,inargs)
                               (ident-list . ,outargs) . ,_) ,stmt-list)
         (values (cons form seed) (pop-scope kdict)))
        ((assn . ,_)
         (values (cons form seed) kdict))
        #;((for (ident ,name) . ,_)
         (values (cons form seed) (pop-scope kdict)))
        ;; todo branch completion
        ;;  (if exp (assn x 1)) => (if exp (assn x 1) (assn x x))
        (,_
         (values (cons form seed) kdict)))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  (call-with-values
      (lambda () (foldts*-values fD fU fH `(*TOP* ,tree) '() gbls))
    (lambda (seed dict) seed)))


;; ============================================================================

(define *path* (make-parameter '("." "ex")))
(define *trees* (make-parameter '())) ;; alist of basename, tree

;; in a file containing "addpath" calls add the paths
(define (pathload filename)
  (let* ((tree (read-mfile filename))
         (calls ((sxpath '(script-file expr-stmt call)) `(*TOP* ,tree)))
         (dirs (map (lambda (sx) (sx-ref* sx 2 1 1)) calls)))
    (*path* (append (*path*) dirs))
    dirs))

(define (find-in-path file-name dirl)
  "file-name is never a path"
  (if (null? dirl) #f
      (let ((path (string-append (car dirl) "/" file-name)))
        (if (access? path R_OK) path (find-in-path file-name (cdr dirl))))))

(define (find-file filename)
  (find-in-path filename (*path*)))

(define (load-function name)
  (let* ((path (find-in-path (string-append name ".m") (*path*)))
         (tree (and path (read-mlang-file path '())))
         (tree (and tree (identify-tree tree))))
    tree))

;; This will be used to initialize file-scope functions
;; 1. for function-file: (cdr result) for (car result)
;; 2. for script-file, result for code prior to functions
(define (WTF-tree-function tree) ;; what is this for again?
  "return a list of functions in the tree (script-file or function-file)"
  (fold
   (lambda (form name)
     (or name
         (sx-match form
           ((fctn-defn (ident ,name) . ,_) name)
           ((fctn-defn (toplevel ,name) . ,_) name)
           (,__ name))))
   #f
   (sx-match tree
     ((script-file . ,forms) forms)
     ((function-file . ,forms) forms)
     ((classdef-file . ,forms) forms)
     (,_ (pperr tree) (error "missed form")))))

(define (tree-calls tree)
  "return calls or aref-or-call (script-file or function-file)
  should be w/o duplicates.
  "
  (foldts
   (lambda (seed tree) tree)
   (lambda (seed kseed tree)
     (sx-match tree
       ;;((assn (ident ,name) (handle ,_0)) (cons `(skip ,name) seed))
       ((call (ident ,name) . ,_) (cons name seed))
       ((aref-or-call (ident ,name) . ,_) (cons name seed))
       (,__ (lset-union equal? kseed seed))))
   (lambda (seed leaf) '())
   '() tree))

(define (gen-program script-file)
  ;; parse script-file => tree
  ;; extract calls and aref-or-call
  ;; add to next refs w/ .m
  ;; assumes tree-calls provides no duplicates (hence lset-union there)
  (parameterize ((*path* (cons (dirname script-file) (*path*))))
    (let loop ((trees '()) (done builtins) (curr '())
               (next (list (basename script-file ".m"))))
      (cond
       ((pair? curr)
        (cond
         ((member (car curr) done) (loop trees done (cdr curr) next))
         ((find-file (string-append (car curr) ".m")) =>
          (lambda (file)
            (let* ((tree (call-with-input-file file
                           (lambda (port)
                             (set-port-filename! port file)
                             (read-mlang-file port '()))))
                   (next (fold (lambda (fn nx)
                                 (if (or (member fn done) (member fn nx))
                                     nx (cons fn nx)))
                               next (tree-calls tree))))
              (loop (cons tree trees) (cons (car curr) done) (cdr curr) next))))
         (else
          (sferr "gen-program: not found: ~s\n" (car curr))
          (loop trees done (cdr curr) next))))
       ((pair? next) (loop trees done next '()))
       (else `(program ,@(reverse trees)))))))


;; ============================================================================
;; attempt to do the effects analysis

;; strategy here
;; 1) convert tree into vector of (tag rx ...)
;;    where rx is an index in the vector, or a string
;; 2) then iterate using vector-fold ...
;;      (lambda (upd ix te infx infy infz)
;;    where ve is the converted tree element and infx infy infz are
;;    vectors of specid ypes of info (see BETTER below);
;;    top is last element


;; I think the first thing to do is convert all identifiers to unique
;; ones.  (ident "foo") => (ftn|fil|gbl foo foo-123)
;; ??? and keep a global symbol table w/ uses ???
;; remember: beta-reduction would need to relabel all function variables

;; what about
;; multiple function calls => overloading

(define (ident-name id)
  (sx-match id
    ((toplevel ,name) name)
    ((lexical ,name ,lname) lname)
    (,_ #f)))

;; used?? structs ...
(define (struct-sig fields)
  ;; determine struct signature
  ;; sum of hash of fields
  ;; use size 999983 <= largest prime under 1,000,000
  (map + (lambda (f) (hash f 999983)) fields))

;; need symbol tables : global, per-file, per-function
(define (cstring len)
  (carray (cbase 'char) len))


;; BETTER:
;; struct { num: 1, str: 1, sym: 1, obj: 1 }  use-flags only one of these
;; struct { int: 1, flt: 1, cpx: 1 }          num-flags any of these
;; struct { rk0: 1, rk1: 1, rk2: ... rk7: 1 } rnk-flags of these

;; dims : list of dims only for rk > 0
;; challenge is to map expressions to transformation of dims

(define-syntax vx-match
  (syntax-rules ()
    ((_ vx ix c0 c1 ...)
     (let ((vy (vector-ref vx ix)))
        (vx-m-case vx vy c0 c1 ...)))))

(define-syntax vx-m-case
  (syntax-rules (else)
    ((_ vx vy ((tag n1 ...) ex ...) c1 ...)
     (let ((kf (lambda () (vx-m-case vx vy c1 ...))))
        (if (eq? (vector-ref vy 0) 'tag)
            (vx-m-exp vx vy 1 (n1 ...) (begin (if #f #f) ex ...) (kf))
            (kf))))
    ((_ vx vy (else ex ...)) (begin (if #f #f) ex ...))
    ((_ vx vy) (error "vx-match: nothing matches"))))

(define-syntax vx-m-exp
  (syntax-rules (unquote)
    ((_ vx vy iy () kt kf)
     kt)
    ((_ vx vy iy (unquote v) kt kf)
     (let ((v (vector-ref vy iy))) kt))
    ((_ vx vy iy (n1 n2 ...) kt kf)
     (vx-m-exp vx vy iy n1 (vx-m-exp vx vy (1+ iy) (n2 ...) kt kf) kf))
    ((_ vx vy iy u kt kf)
     (let ((vyi (vector-ref vy iy)))
       (cond
        ((and (integer? vyi) (symbol? u)
              (eq? (vector-ref (vector-ref vx vyi) 0) u)) kt)
        ((and (string? u) (string=? vyi u)) kt)
        (else kf))))))


;; @deffn {Syntax} gen-flags flag-set flag1 flag2 ...
;; Define a set of variables with name @var{flag-set}-@var{flag1} ....
;; @end deffn
(define-syntax gen-flags
  (lambda (x)
    (define (genid ctx pfx id)
      (datum->syntax
       ctx (symbol-append (syntax->datum pfx) '- (syntax->datum id))))
    (syntax-case x ()
      ((_ name flag0 ...)
       #`(begin .
           #,(let loop ((ids #'(flag0 ...)) (ofx 0))
               (if (null? ids) '()
                   (cons #`(define #,(genid x #'name (car ids)) #,(ash 1 ofx))
                         (loop (cdr ids) (1+ ofx))))))))))
  
(define (set-flag flags flag)
  (logior flags flag))
(define (clr-flag flags flag)
  (logand flags (lognot flag)))
(define (flag-set? flags flag)
  (not (zero? (logand flags flag))))

(gen-flags USE
           NUM STR SYM OBJ
           INT FLT CPX
           RK0 RK1 RK2 RK3 RK4 RK5 RK6 RK7)

(define (fl-join fv ix seed . vl) ;; or #f if they are the same
  (let ((v0 (vector-ref fv ix)))
    (let loop ((v1 v0) (vl vl))
      (if (pair? vl)
          (loop (set-flag v0 (car vl)) (cdr vl))
          (cond
           ((eq? v0 v1) seed)
           (else (vector-set! fv ix v1) #t))))))

(define (tryme vx)
  (let* ((nx (vector-length vx))
         (flv (make-vector nx 0))       ; flag vector
         ;;(szv (make-vector vl 0))       ; max size of array data
         )

    (let loop ((changed #t))
      (if changed
          (fold
           (lambda (ix c?) ;; c? = changed?
             ;; (sf "~s ~s\n" ix (vector-ref vx ix))
             (vx-match vx ix
               ((fixed ,v)
                (fl-join flv ix c? USE-NUM USE-INT))
               ((float ,v)
                (fl-join flv ix c? USE-NUM USE-FLT))
               ((string ,v)
                (fl-join flv ix c? USE-STR))
               ((add ,lt ,rt)
                (fl-join flv ix c? USE-NUM))
               ((sub ,lt ,rt)
                (fl-join flv ix c? USE-NUM))
               ((mul ,lt ,rt)
                (fl-join flv ix c? USE-NUM))
               ((div ,lt ,rt)
                (fl-join flv ix c? USE-NUM))
               ((assn ,lval ,rval)
                (fl-join flv lval (vector-ref flv rval)))
               (else)))
           #f (iota nx))))))


;; ============================================================================

(use-modules (srfi srfi-37))

(define (fail fmt . args)
  (apply simple-format (current-error-port)
         (string-append "mltoc: " fmt "\n")
         args)
  (exit 1))

(define (show-usage)
  (simple-format #t "Usage: mltoc [OPTION] FILE ...
Convert a mlang dot-m file to c.
  -h, --help           print this help message

Report bugs to https://github.com/mwette/nyacc/issues.\n"))

(define options
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'help #t opts) files)))
   (option '(#\s "sxml") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'sxml #t opts) files)))
   (option '(#\p "pprint") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'pprint #t opts) files)))
   (option '(#\e "envload") #t #f
           (lambda (opt name arg opts files)
             (values (acons 'envload arg opts) files)))
   (option '(#\t "tree-il") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'tree-il #t opts) files)))
   (option '(#\i "identify") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'identify #t opts) files)))
   (option '(#\j "probe") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'probe #t opts) files)))
   (option '(#\x "misc") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'misc #t opts) files)))
   ))

(define (parse-args args)
  (args-fold args options
             (lambda (opt name arg files opts)
               (fail "unrecognized option: ~S" name)
               (exit 1))
             (lambda (file opts files)
               (unless (string-suffix? ".m" file)
                 (fail "expecting .m file"))
               (values opts (cons file files)))
             '() '()))

(define (read-mfile srcfile)
  (call-with-input-file srcfile
    (lambda (port)
      (set-port-filename! port srcfile)
      (read-mlang-file port '()))))
      

(define (main . args)
  (call-with-values
      (lambda () (parse-args args))
    (lambda (opts files)
      (when (or (assq-ref options 'help) (null? files)) (show-usage) (exit 0))
      (and=> (assq-ref opts 'envload) (lambda (file) (pathload file)))
      (for-each 
       (lambda (srcfile)
         (and (assq-ref opts 'sxml)
              (pp (read-mfile srcfile)))
         (and (assq-ref opts 'pprint)
              (pretty-print-ml (read-mfile srcfile)))
         (and (assq-ref opts 'identify)
              (pp (identify-tree (read-mfile srcfile) '((@top . #t)))))
         (and (assq-ref opts 'tree-il)
              (pp (mlang-sxml->xtil (read-mfile srcfile) (current-module) '())))
         (and (assq-ref opts 'misc)
              (let* ((tree (read-mfile srcfile))
                     (prog (gen-program srcfile))
                     (idsx (identify-tree prog))
                     ;;(nelt (sxml-count prog))
                     (idvx (sxml->vxml idsx))
                     )
                ;;(pp prog)
                ;;(pp idsx)
                ;;(pp idvx)
                ;;(display-vxml idvx)
                (tryme idvx)
                ;;(format #t "~b\n" USE-INT)
                #t))
         #f)
       files)))
  0)


(apply main (cdr (program-arguments)))

;; --- last line ---
