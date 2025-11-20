;; mlan.scm - mlang analysis

(define-module (nyacc lang mlang mltoc)
  #:export (mlang->c99))

(use-modules (language nx-mlang parser))
(use-modules (language nx-mlang pprint))
(use-modules (language nx-mlang compile-tree-il))

(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang sx-util))

(use-modules (nyacc lang nx-util))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 pprint))

(use-modules ((srfi srfi-1) #:select (fold last)))
(use-modules (srfi srfi-9))             ; define-record-type
(use-modules (srfi srfi-11))
(use-modules (sxml match))
(use-modules (sxml fold))               ; fold-values
(use-modules (ice-9 regex))
(use-modules (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))
(define (pp exp)
  (pretty-print exp (current-output-port) #:per-line-prefix "  "))

;; =================================

;; A variable is a unique symbol, a string name and a list of uses.
;; var : (x-123 "x" () ((kind . struct) (rank . 2) (type . float))

;; I think the first thing to do is convert all identifiers to unique
;; ones.  (ident "foo") => (ftn|fil|gbl foo foo-123)
;; ??? and keep a global symbol table w/ uses ???
;; remember: beta-reduction would need to relabel all function variables

;; what about
;; multiple function calls => overloading

;; mlang expression usage (include identifiers)
(define-record-type <mlxuse>
  (%make-mlxuse kind rank dims type)
  mlxuse?
  (kind mlxuse-kind set-mlxuse-kind!)   ; ftn num flt int struct dict
  (rank mlxuse-rank set-mlxuse-rank!)
  (dims mlxuse-dims set-mlxuse-dims!)
  (type mlxuse-type set-mlxuse-type!))      ; element type
;; NEEDED: something to denote struct type.   Maybe we add `info'
;; field with an alist
(define make-use %make-mlxuse)

;; need symbol tables : global, per-file, per-function

(define ml-uses (make-object-property))
;; (set! (ml-uses expr) (list (make-mxluse x x x)))

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
   (else #t)))

(define* (new-use*? obj #:key kind rank dims type)
  (and (ml-uses obj)
       (fold
        (lambda (use new?)
          (and new?
               (not (and (eq? (mlxuse-kind use) kind)
                         (eq? (mlxuse-rank use) rank)
                         (equal? (mlxuse-dims use) dims)
                         (equal? (mlxuse-type use) type)))))
        #t (ml-uses obj))))

;; maybe kind is symbol or expr
(define* (add-use*! sx #:key kind rank dims type)
  (if (new-use*? sx #:kind kind #:rank rank #:dims dims #:type type)
      (set! (ml-uses sx) (cons (make-use kind rank dims type) (ml-uses sx)))))

(define (new-use? obj use)
  (and (ml-uses obj)
       (fold
        (lambda (use new?) (and new? (not (equal? obj use))))
        #t (ml-uses obj))))

(define (add-use! sx use)
  (if (new-use? sx use)
      (let ((uses (ml-uses sx)))
        (set! (ml-uses sx)
              (if (ml-uses sx) (cons use (ml-uses sx)) (list use))))))

(define typein-dict
  '((int int num)                       ; int is int or num
    (flt flt num)                       ; flt is flt or num
    ;;(cpl cpl num)
    ;;(str str)
    (class class)
    (struct struct)
    (cell cell)))

(define* (used-as? sx #:key kind rank dims type)
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

(define (probe-fctn tree)
  #f) ;; ==> ("name" . info)

(define (expr-use expr)
  (sx-match expr
    ;;((call (ident "struct")
    ;;^ this can also be used to find member types

    ((fixed ,v) (add-use*! expr #:type 'int #:rank 0))
    ((float ,v) (add-use*! expr #:type 'flt #:rank 0))
    ((add ,l ,r) '())

    ((call (ident "struct") (expr-list . ,exprs))
     ;;we can do this
     #f)
    
    ((call (ident ,name) (expr-list . ,exprs))
     ;;how to do this
     #f)
    
    (,__ (sferr "expr-use missed:\n") (pperr expr) (quit))))

(define (probe-script tree)

  (define (make-struct-type args) ;; make struct type from struct() call
    (let loop ((names '()) (vals '()) (args args))
      #f))

  (define (probe tree gbl fil ftn)
    ;; fil means function file; script file goes to gbl
    (sx-match tree
      ((script (@ (filename ,filename)) . ,stmts)
       (call-with-values (lambda () (fold-values probe stmts gbl #f #f))
         (lambda (gbl fil ftn) (values (cons filename gbl) #f #f))))
      ((assn (ident ,name) ,rhs)
       (let ((use (expr-use rhs)))
         (add-use! name use)
         (cond
          (ftn (values gbl fil (cons name ftn)))
          (fil (values gbl (cons name fil) ftn))
          (else (values (cons name gbl) fil ftn)))))
      ((empty-stmt) (values gbl fil ftn))
      (,_ (sferr "missed:\n") (pperr tree)
          (values gbl fil ftn))))

  (call-with-values
      (lambda () (probe tree '() #f #f))
    (lambda (gbl fil ftn) gbl)))

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

;; =================================

;; need to do a full pass to convert 
;; (ident "name") => (toplevel name)|(lexical name name-123)
;; (ident "name") => (toplevel "name")|(lexical "name" "name-123")

;; loop ((full-tree '()) (next '()) (curr (list "top.m")))
;;   (probe (parse-file (car curr))
;;     if ident convert to lexical|toplevel
;;     function call and implies file, add file to next

(define (ensure-variable name dict)
  (let* ((dict (nx-ensure-variable name dict))
         (nref (nx-lookup name dict)))
    (match nref
      ((toplevel ,n) `(toplevel ,(symbol->string n)))
      ((lexical ,n ,r) `(lexical ,(symbol->string n) ,(symbol->string r))))
    dict))

(define (identify-tree tree env)

  (define (fD tree seed dict)
    ;; Here we update symbol table where identifiers are defined.
    (sx-match tree
      ((fctn-defn (fctn-decl (ident ,name) (ident-list . ,inargs)
                             (ident-list . ,outargs) . ,comms))
       (let* ((dict (ensure-variable name dict))
              (dict (nx-push-scope dict))
              (dict (fold (lambda (sx dt) (nx-add-lexical (sx-ref sx 1) dt))
                          dict inargs))
              (dict (fold (lambda (sx dt) (nx-add-lexical (sx-ref sx 1) dt))
                          dict outargs))
              (dict (acons '@F name dict)))
         (values
          `(fctn-defn (fctn-decl (ident ,name) (ident-list . ,inargs)
                                 (ident-list . ,outargs)) ,stmt-list)
          '() dict)))
      ((assn (@ . ,attr) (ident ,name) ,rhsx)
       (let* ((dict (ensure-variable name dict))
              (idxp (nx-lookup name dict)))
         (values `(assn ,idxp ,rhsx) '() dict)))
      ((assn (@ . ,attr) (aref-or-call (ident ,name) ,expl) ,rhsx)
       (let* ((dict (ensure-variable name dict))
              (idxp (nx-lookup name dict)))
         (values `(assn (aref-or-call ,idxp ,expl ,rhsx) '() dict))))
      ((assn (@ . ,attr) (sel (ident ,name) ,expr) ,rhsx)
       (let* ((dict (ensure-variable name dict))
              (idxp (nx-lookup name dict)))
         (values `(assn (sel ,idxp ,expr)) '() dict)))
      (,_ (values tree '() dict))))

  (define (fU tree seed dict kseed kdict)
    ;; Here we update identifiers where they are used.
    (sx-match leaf
      ((ident ,name) (values (cons (nx-lookup name) seed) kdict))
      (,_ (values tree seed kdict))))

  (define (fH leaf seed dict)
    (values (if (null? leaf) seed (cons leaf seed)) dict))

  (foldts*-values fD fU fH `(*TOP* ,exp) '() env))
    
;; =================================

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
   (option '(#\t "tree-il") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'tree-il #t opts) files)))
   (option '(#\p "pprint") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'pprint #t opts) files)))
   (option '(#\j "probe") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'probe #t opts) files)))
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
           (when (assq-ref opts 'probe) (pp (probe-script tree)))
           (when (assq-ref opts 'tree-il)
             (pp (mlang-sxml->xtil tree (current-module) '())))
           #f))
       files)))
  0)

(sferr "program-args: ~s\n" (program-arguments))
(apply main (cdr (program-arguments)))

;; --- last line ---
