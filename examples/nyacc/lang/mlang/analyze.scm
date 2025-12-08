;; analyze.scm - mlang analysis
;;
;; $ guile analyze.scm -s top.m

;;; Notes:
;; 1) We will create a new form
;;    appl: appl-item* where appl-item: script-file | function-file
;; 2) also need a universal symbol table
;; 3) also need effects record per node

;;; Code:

(define-module (nyacc lang mlang mltoc)
  #:export (mlang->c99))

(use-modules (ice-9 regex))
(use-modules (ice-9 match))
(use-modules ((srfi srfi-1) #:select (fold last)))
(use-modules (srfi srfi-9))             ; define-record-type
(use-modules (srfi srfi-11))
(use-modules (sxml match))
(use-modules (sxml fold))               ; fold-values

(use-modules (nyacc foreign cdata))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang nx-util))

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
  '(
    "struct"
    ))


;; ============================================================================
;; identify tree

;; need to do a full pass to convert 
;; (ident "name") => (toplevel name)|(lexical name name-123)
;; (ident "name") => (toplevel "name")|(lexical "name" "name-123")

;; loop ((full-tree '()) (next '()) (curr (list "top.m")))
;;   (probe (parse-file (car curr))
;;     if ident convert to lexical|toplevel
;;     function call and implies file, add file to next

(define (sx-ize nref)
  (match nref
    (`(toplevel ,n)
     (set-cdr! nref (list (symbol->string n))))
    (`(lexical ,n ,r)
     (set-cdr! nref (list (symbol->string n) (symbol->string r))))
    (__
     (pperr nref)
     (error "add-variable error")))
  nref)

(define (add-variable name dict)
  (let* ((dict (or (nx-add-framelevel name dict)
                   (nx-add-toplevel name dict)))
         (nref (nx-lookup name dict)))
    (sx-ize nref)
    dict))

(define (add-lexical name dict)
  (let* ((dict (nx-add-lexical name dict))
         (nref (nx-lookup name dict)))
    (sx-ize nref)
    dict))

(define (add-toplevel name dict)
  (let* ((dict (nx-add-toplevel name dict))
         (nref (nx-lookup name dict)))
    (sx-ize nref)
    dict))

(define (ensure-variable name dict)
  (if (nx-lookup name dict)
      dict
      (add-variable name dict)))

(define (ensure-variable/frame name dict)
  (if (nx-lookup-in-frame name dict)
      dict
      (add-variable name dict)))

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
                             (ident-list . ,outargs) . ,comms) ,stmt-list)
       (let* ((dict (ensure-variable name dict))
              (dict (nx-push-scope dict))
              (dict (fold add-lexical dict (map cadr inargs)))
              (dict (fold add-lexical dict (map cadr outargs)))
              (dict (nx-add-tag dict name)))
         (values tree '() dict)))
      ((assn (@ . ,attr) (ident ,name) ,rhsx)
       (values tree '() (ensure-variable name dict)))
      ((assn (@ . ,attr) (aref-or-call (ident ,name) ,expl) ,rhsx)
       (values tree '() (ensure-variable name dict)))
      ((assn (@ . ,attr) (sel (ident ,name) ,expr) ,rhsx)
       (values tree '() (ensure-variable name dict)))
      ;;((multi-assn ,
      ((for (ident ,name) ,range ,stmt-list) 
       (values tree '() (add-lexical name dict)))
      ((call (ident ,name) . ,_)
       (values tree '() (add-toplevel name dict)))
      ;; ident used as name
      ((sel (ident ,name) ,expr)
       (values `(sel (name ,name) ,expr) '() dict))
      ((obj-prop (ident ,name) ,expr)
       (values `(obj-prop (name ,name) ,expr) '() dict))
      (,_ (values tree '() dict))))

  (define (fU tree seed dict kseed kdict) ;; => seed dict
    (let ((form (reverse kseed)))
      ;; Here we update identifiers where they are used.
      (sx-match form
        ((*TOP* ,subform) (values subform kdict))
        ((ident ,name)
         (values (cons (or (nx-lookup name kdict) form) seed) kdict))
        ((fctn-defn . ,_)
         (values (cons form seed) (nx-pop-scope kdict)))
        ((assn . ,_)
         (values (cons form seed) kdict))
        (,_ (values (cons form seed) kdict)))))

  (define (fH leaf seed dict)
    (values (cons leaf seed) dict))

  (call-with-values
      (lambda () (foldts*-values fD fU fH `(*TOP* ,tree) '() gbls))
    (lambda (seed dict) seed)))


;; ============================================================================

(define *path* (make-parameter '(".")))
(define *trees* (make-parameter '())) ;; alist of basename, tree

(define (find-in-path file-name dirl)
  "file-name is never a path"
  (if (null? dirl) #f
      (let ((path (string-append (car dirl) "/" file-name)))
        (if (access? path R_OK) path (find-in-path file-name (cdr dirl))))))

(define (find-file file-name)
  (find-in-path file-name (*path*)))

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
     (,_ (pperr tree) (error "missed form")))))

(define (tree-calls tree)
  "return calls or aref-or-call (script-file or function-file)"
  (foldts
   (lambda (seed tree) tree)
   (lambda (seed kseed tree)
     (sx-match tree
       ((call (ident ,name) . ,_) (cons name seed))
       ((aref-or-call (ident ,name) . ,_) (cons name seed))
       (,__ (append kseed seed))))
   (lambda (seed leaf) '())
   '() tree))

(define (gen-program script-file)
  ;; parse script-file => tree
  ;; extract calls and aref-or-call
  ;; add to next refs w/ .m
  (let loop ((treed '()) (done '()) (curr '())
             (next (list (basename script-file ".m"))))
    (cond
     ((pair? curr)
      (let* ((file (find-file (string-append (car next) ".m")))
             (tree (call-with-input-file file
                    (lambda (port)
                      (set-port-filename! port file)
                      (read-mlang-file port '()))))
             (next (fold (lambda (fn nx)
                           (if (or (member fn done) (member fn nx))
                               nx (cons fn nx)))
                         next (tree-calls tree))))
        (loop (acons file tree treed) done (cdr curr) next)))
     ((pair? next) (loop treed done next '()))
     (else treed))))
    
      

;; ============================================================================

(define (ident-name id)
  (sx-match id
    ((toplevel ,name) name)
    ((lexical ,name ,lname) lname)
    (,_ #f)))

;; A variable is a unique symbol, a string name and a list of uses.
;; var : (x-123 "x" () ((kind . struct) (rank . 2) (type . float))

;; I think the first thing to do is convert all identifiers to unique
;; ones.  (ident "foo") => (ftn|fil|gbl foo foo-123)
;; ??? and keep a global symbol table w/ uses ???
;; remember: beta-reduction would need to relabel all function variables

;; what about
;; multiple function calls => overloading

;; BETTER:
;; struct { num: 1, str: 1, sym: 1, obj: 1 }  use-flags only one of these
;; struct { int: 1, flt: 1, cpx: 1 }          num-flags any of these
;; struct { rk0: 1, rk1: 1, rk2: ... rk7: 1 } rnk-flags of these
;; size : prod of dims                        nelt
;; OLD BAD:
;; mlang expression usage (include identifiers)
(define-record-type <mlxuse>
  (%make-mlxuse kind rank dims type var?)
  mlxuse?
  (kind mlxuse-kind set-mlxuse-kind!)   ; ftn num flt int struct dict var
  (rank mlxuse-rank set-mlxuse-rank!)
  (dims mlxuse-dims set-mlxuse-dims!)
  (type mlxuse-type set-mlxuse-type!)   ; element type: ctype !!
  (var? mlxuse-var? set-mlxuse-var?!)   ; used as variable #f does not ftn
  )
;; NEEDED: something to denote struct type.   Maybe we add `info'
;; field with an alist
(define* (make-use #:key kind rank dims type var?)
  (%make-mlxuse kind rank dims type var?))

;; need symbol tables : global, per-file, per-function
(define (cstring len)
  (carray (cbase 'char) len))

(define ml-uses (make-object-property))
;; (set! (ml-uses expr) (list (make-mlxuse x x x)))

;; need idea here
;; go through uses
;; if uses[ix].key matches +1 & go on, if uses[ix].key is #f go on,
;;    else reject
(define (find-use obj use)
  (let* ((uses (ml-uses obj))
         )
    #f))

(define (use-equal? a b)
  (if (or (not (mlxuse? a)) (not (mlxuse? b))) (error "bad call: use-equal?"))
  (cond
   ((not (eq? (mlxuse-kind a) (mlxuse-kind b))) #f)
   ((not (eq? (mlxuse-rank a) (mlxuse-rank b))) #f)
   ((not (equal? (mlxuse-dims a) (mlxuse-dims b))) #f)
   ((not (equal? (mlxuse-type a) (mlxuse-type b))) #f)
   ((not (eq? (mlxuse-var? a) (mlxuse-var? b))) #f)
   (else #t)))

(define* (new-use*? obj #:key kind rank dims type var?)
  (and (ml-uses obj)
       (fold
        (lambda (use new?)
          (and new?
               (not (and (eq? (mlxuse-kind use) kind)
                         (eq? (mlxuse-rank use) rank)
                         (equal? (mlxuse-dims use) dims)
                         (equal? (mlxuse-type use) type)
                         (eq? (mlxuse-var? use) var?)
                         ))))
        #t (ml-uses obj))))

;; maybe kind is symbol or expr
(define* (add-use*! sx #:key kind rank dims type var?)
  (if (new-use*? sx #:kind kind #:rank rank #:dims dims #:type type #:var? var?)
      (set! (ml-uses sx)
            (cons (%make-mlxuse kind rank dims type var?) (ml-uses sx)))))

(define (new-use? obj use)
  (and (ml-uses obj)
       (fold
        (lambda (use new?) (and new? (not (equal? obj use))))
        #t (ml-uses obj))))

(define (add-use! sx use)
  (if sx
      (if (new-use? sx use)
          (let ((uses (ml-uses sx)))
            (set! (ml-uses sx)
                  (if (ml-uses sx) (cons use (ml-uses sx)) (list use)))))))

(define (used-as-var sx)
  ;; need to rethink all this
  #f)

(define typein-dict
  '((int int num)                   ; int is int or num
    (flt flt num)                   ; flt is flt or num
    ;;(cpl cpl num)
    ;;(str str)
    (class class)
    (struct struct) (cell cell) (var var) (ftn ftn) (num num)))

(define (type-in a b)
  (or (eq? a b)
      (and (memq b (assq-ref typein-dict a)) #t)))

;; (define (type-compat a b) ...

;;(sf "type-in int num => ~s\n" (type-in 'int 'num))
;;(sf "type-in num int => ~s\n" (type-in 'num 'int))

;; structs ...
(define (struct-sig fields)
  ;; determine struct signature
  ;; sum of hash of fields
  ;; use size 999983 <= largest prime under 1,000,000
  (map + (lambda (f) (hash f 999983)) fields))

;; objects ...


(define* (used-as? sx #:key kind rank dims type var?)
  (let ((uses (ml-uses sx)))
    (cond
     ;;(kind
     (type (fold
            (lambda (use ans)
              (or ans
                  (memq (mlxuse-type use) (assq-ref typein-dict type))))
            #f uses))
     (else #f)))
  )

;; try w/o struct or class at first
(define (unop-use op ex)
  ;; transpose swaps dims
  #f)
  
(define (binop-use op lt rt)
  "use of binary operation"
  ;; scalar*array ->
  (let* ((lkind (mlxuse-kind lt)) (rkind (mlxuse-kind rt))
         (lrank (mlxuse-rank lt)) (rrank (mlxuse-rank rt))
         (ldims (mlxuse-dims lt)) (rdims (mlxuse-dims rt))
         (ltype (mlxuse-type lt)) (rtype (mlxuse-type rt))
         (rkind (cond
                 ((eq? lkind rkind) lkind)
                 ((or (and (eq? lkind 'int) (eq? rkind 'flt))
                      (and (eq? lkind 'flt) (eq? rkind 'int)))
                  'flt)
                 ((type-in lkind rkind) lkind)
                 ((type-in rkind lkind) rkind)
                 (else #f)))
         (rrank (let ((pr (cons lrank rrank)))
                  (cond
                   ((equal? pr '(#f . #f)) #f)
                   ((equal? pr '(0 . 0)) 0)
                   ((member pr '((0 . 2) (2 . 0))) 2)
                   ((member pr '((1 . 2) (2 . 1))) 1)
                   (else
                    ;; need to look at dims nx1*1xm vs 1xn*nx1
                    #f))))
         (rdims #f)
         (rtype #f))
    (make-use #:kind rkind #:rank rrank #:dims rdims #:type rtype)))

(define (uses-cover uses)
  ;; reduce a list of uses to something minimal.
  (car uses))


;; return 
(define (expr-use expr)
  (sx-match expr
    ((fixed ,v) (make-use #:kind 'int #:rank 0 #:type (cbase 'int)))
    ((float ,v) (make-use #:kind 'flt #:rank 0 #:type (cbase 'double)))
    ;;((lexical ,g ,l) 
    ((add ,l ,r) (binop-use 'add l r))
    ((sub ,l ,r) (binop-use 'sub l r))
    ((mul ,l ,r) (binop-use 'mul l r))
    ((div ,l ,r) (binop-use 'div l r))
    ;;((neg ,x) (add-use! expr (x))

    ((call (toplevel "struct") (expr-list . ,expr-list))
     (let loop ((key #f) (xl expr-list))
       (cond
        ((null? xl) #f)
        ((not key) (loop (car xl) (cdr xl)))
        (else (add-use! (ident-name key) (expr-use (car xl)))
              (loop #f (cdr xl))))))
    
    ((call (toplevel ,name) (expr-list . ,exprs))
     ;;(maybe-add-to-trees name)
     #f)
    
    (,__ (sferr "expr-use missed:\n") (pperr expr) (quit))))

;; conjecture: expressions have one use; identifiers maybe multiple uses

(define (probe-fctn tree)
  #f) ;; ==> ("name" . info)

(define (probe-tree tree)

  (define (make-struct-type args) ;; make struct type from struct() call
    (let loop ((names '()) (vals '()) (args args))
      #f))

  ;; need to add var
  (define (probe tree gbl fil ftn)
    ;; fil means function file; script file goes to gbl
    (sx-match tree
      ((script-file (@ (filename ,filename)) . ,stmts)
       (call-with-values
           (lambda () (fold-values probe stmts gbl #f #f))
         (lambda (gbl fil ftn)
           (values (acons filename fil gbl) '() '()))))
      ;;((assn (ident ,name) ,rhs)
      ;;((call (toplevel ,name) . ,_)
        
      ((assn ,ident ,rhs)
       (let* ((name (ident-name ident))
              (use (expr-use rhs)))
         (used-as-var ident)
         (add-use! name use)
         (cond
          (ftn (values gbl fil (cons name ftn)))
          (fil (values gbl (cons name fil) ftn))
          (else (values (cons name gbl) fil ftn)))))
      ((empty-stmt) (values gbl fil ftn))
      (,_ (sferr "missed:\n") (pperr tree)
          (values gbl fil ftn))))

  (sf "TODO: set up code to deal with structures\n") (quit)
  ;; needs list of fields => struct type (struct-123)
  ;; need then to tune use to be of int flt struct-123, struct-124, ...
  ;; matching proc: (match-struct fields) => list of struct-types

  (call-with-values
      (lambda () (probe tree '() #f #f))
    (lambda (gbl fil lcl) gbl)))


;;  patterns, first pass:
;;    (assn (ident ,n) (call (ident "struct") . ,_) => '((kind . struct))
;;    (assn (ident ,n) (call (ident "cell") . ,_) => kind = cell
;;    (assn (ident ,n) (cell-array . ,_) => kind = cell
;;    (assn (ident ,n) (float . ,_) => '((kind . float) (rank . 0))
;;    (assn (ident ,n) (call (ident "zeros") . ,_) => ...
;;    (assn (ident ,n) (call (ident "ones") . ,_) => ...
;;    (assn (ident ,n) (matrix . ,_) => ... maybe rank, dims, type

;;  patterns, multi pass:
;;    (assn (ident ,n) (call (ident ,f) . ,_)  if f out has use use it

;;    (if (call (ident "isstruct") (ident ,n) ...
;;    (if (call (ident "iscell") (ident ,n) ...

;;  "In function foo, bar is used as struct, cell, ....
;;  Please use different variables.

;;  



;; ============================================================================
;; 3) matrix: "[ int, int, int ]" => ivec
;; 4) matrix: "[ ... (fixed) ... ]" => error
;; 5) colon-expr => fixed-colon-expr (removed)
;; dot notation: obj.meth(args ...) | obj.(property)

#|
(define (lval-root lval)
  (sx-match lval
    ((ident ,name) lval)
    ((aref-or-call ,par ,exl) (lval-root par))
    ((cell-ref ,par ,exl) (lval-root par))
    ((sel ,kid ,par) (lval-root par))
    (,otherwise #f)))

(define (lval-name lval)
  (and=> (lval-root lval) cadr))

(define (fix-lval lval)
  (sx-match lval
    ((aref-or-call ,par ,exl) `(array-ref ,par ,exl))
    (,__ lval)))
|#

#|
((colon-expr . ,rest)
 (let ((form (reverse kseed)))
   (values (if (fixed-colon-expr? tree)
               (cons-source tree 'fixed-colon-expr (cdr form))
               form) gbl lcl)))
((matrix . ,rest)
 (values (cons (check-matrix form seed) gbl lcl)))

(define (fixed-colon-expr? expr)
  (sx-match expr
    ((colon-expr) #t)
    ((colon-expr (fixed ,s) (fixed ,e)) #t)
    ((colon-expr (fixed ,s) (fixed ,i) (fixed ,e)) #t)
    (,_ #f)))

(define (fixed-expr? expr)
  (define (fixed-primary-expr? expr)
    (sx-match expr
      ((fixed ,val) #t)
      ((add ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((sub ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      ((mul ,lt ,rt) (and (fixed-expr? lt) (fixed-expr? rt)))
      (,_ #f)))
  (or (fixed-primary-expr? expr)
      (eq? 'fixed-colon-expr (sx-tag expr))))

(define (float-expr? expr)
  (define (float-primary-expr? expr)
    (sx-match expr
      ((float ,val) #t)
      ((add ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((sub ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((mul ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      ((div ,lt ,rt) (and (float-expr? lt) (float-expr? rt)))
      (,_ #f)))
  (float-primary-expr? expr))

(define (fixed-vec? row)
  (fold (lambda (elt fx) (and fx (fixed-expr? elt))) #t (sx-tail row)))

(define (float-vec? row)
  (fold (lambda (elt fx) (and fx (float-expr? elt))) #t (sx-tail row)))

(define (float-mat? mat)
  (fold (lambda (row fx) (and fx (float-vec? row))) #t
        (map sx-tail (sx-tail mat))))

(define (check-matrix mat)
  (let* ((rows (sx-tail mat))
         (nrow (length rows))
         (row1 (if (positive? nrow) (car rows) #f)))
    (cond
     ((zero? nrow) mat)
     ((and (= 1 nrow) (fixed-vec? row1))
      (cons-source row1 'fixed-vector (cdr row1)))
     ((float-mat? mat)
      (cons-source mat 'float-matrix (cdr mat)))
     (else mat))))

;; @deffn {Procedure} apply-mlang-statics tree => tree
;; Apply static semantics for Octave.  Currently, this includes
;; @itemize
;; @item Change @code{assn} with matrix expression on LHS to a
;; multiple value assignment (@code{assn-many}).
;; @end itemize
;; TODO: aref-or-call:
;; @end deffn
(define (apply-old-mlang-statics tree)

  (define (fU tree)
    (sx-match tree
      ((assn (@ . ,attr) (matrix (row . ,elts)) ,rhs)
       (cons-source tree 'assn-many `((@ . ,attr) (lval-list . ,elts) ,rhs)))
      ((colon-expr . ,rest)
       (if (fixed-colon-expr? tree)
           (cons-source tree 'fixed-colon-expr (cdr tree))
           tree))
      ((matrix . ,rest)
       (check-matrix tree))
      (,_ tree)))
  
  (define (fH tree) tree)
  
  (cadr (foldt fU fH `(*TOP* ,tree))))

|#

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

(define (main . args)
  (call-with-values
      (lambda () (parse-args args))
    (lambda (opts files)
      (when (or (assq-ref options 'help) (null? files)) (show-usage) (exit 0))
      (for-each 
       (lambda (srcfile)
         (let ((tree (call-with-input-file srcfile
                       (lambda (port)
                         (set-port-filename! port srcfile)
                         (read-mlang-file port '())))))
           (when (assq-ref opts 'sxml) (pp tree))
           (when (assq-ref opts 'pprint) (pretty-print-ml tree))
           (when (assq-ref opts 'identify)
             (pp (identify-tree tree '((@top . #t)))))
           (when (assq-ref opts 'probe)
             (let* ((tree (identify-tree tree '((@top . #t)))))
               (*trees* (acons (basename srcfile) tree (*trees*)))
               (let loop ((done #f) (trees (*trees*)))
                 (cond
                  (done #t)
                  ((null? trees) (loop done (*trees*)))
                  (else
                   (probe-tree (cdar trees))
                   ;; until we find how to terminate
                   (loop #t (cdr trees)))))))
           (when (assq-ref opts 'tree-il)
             (pp (mlang-sxml->xtil tree (current-module) '())))
           (when (assq-ref opts 'misc)
             (pp (tree-calls tree)))
           #f))
       files)))
  0)


(apply main (cdr (program-arguments)))

;; --- last line ---
