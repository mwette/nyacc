;; ctest.scm

(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules (system foreign))
(use-modules (system foreign-library))
(use-modules (nyacc lang arch-info))
(use-modules (nyacc lang cdata))

(use-modules (ice-9 pretty-print))
(define (pp exp)
  (pretty-print exp (current-output-port) #:per-line-prefix "  "))
(define (sf fmt . args)
  (apply simple-format (current-output-port) fmt args))


;;(define (compile code

;; void struct_init(foo_t* *s, int a, double b, ...)

(set! *random-state* (random-state-from-platform))

;; make random struct with n fields
(define names #(a a b c d e f g h i j k l m n o p q r))
(define types #(int double unsigned-int short-int unsigned-short))

(define nname (vector-length names))
(define ntype (vector-length types))

(define (make-rand-pairs n)
  (let loop ((pairs '()) (n n))
    (if (zero? n) pairs
        (loop (cons (cons (vector-ref names n)
                          (vector-ref types (random n))) pairs) (1- n)))))


;; ----- generate c99 code --------------

(use-modules (nyacc lang c99 pprint))

;; SCM test1(struct foo *foo, int a, uint3_t b)
(define (gen-c99-test-code kase seed)
  (define (xmk-fparam type name)
    (let () ;;(t (symbol->string type)) (n (symbol->string name)))
      (case type
        ;;((double float) (mk-param `(float-type ,t) `(ident ,n)))
        (else #f)))) ;; (mk-param `(fixed-type ,t) `(ident ,n))))))
  (define case-num (car kase))
  (define pairs (cdr kase))
  (define sn (string-append "test" (number->string case-num)))
  (define (name pair) (symbol->string (car pair)))
  (define (type pair) (symbol->string (cdr pair)))
  (define (mk-param tspecv pdeclrv)
    `(param-decl (decl-spec-list (type-spec ,tspecv)) (param-declr ,pdeclrv)))
  (define (mk-sparam)
    (mk-param `(struct-ref (ident ,sn)) `(ptr-declr (pointer) (ident "t"))))
  (define (mk-fparam type name)
    (let ((t (symbol->string type)) (n (symbol->string name)))
      (case type
        ((double float) (mk-param `(float-type ,t) `(ident ,n)))
        (else (mk-param `(fixed-type ,t) `(ident ,n))))))
  (define (assn fldname)
    `(expr-stmt (assn-expr (i-sel (ident ,fldname) (p-expr (ident "t")))
                           (op "=") (p-expr (ident ,fldname)))))
  (cons*
   `(decl
     (decl-spec-list
      (type-spec
       (struct-def
        (ident ,sn)
        (field-list
         ,@(map (lambda (pair)
                  `(comp-decl
                    (decl-spec-list (type-spec (fixed-type ,(type pair))))
                    (comp-declr-list (comp-declr (ident ,(name pair))))))
                pairs))))))
   `(fctn-defn
     (decl-spec-list (type-spec (fixed-type "int")))
     (ftn-declr
      (ident ,sn)
      (param-list
       ,(mk-sparam)
       ,@(map (lambda (pair) (mk-fparam (cdr pair) (car pair))) pairs)))
     (compd-stmt
      (block-item-list
       ,@(map (lambda (pair) (assn (symbol->string (car pair)))) pairs)
       (return (p-expr (fixed "0"))))))
   seed))

;; ----- generate scm code --------------

(define (make-struct-from-pairs pairs)
  (let loop ((fields '()) (pairs pairs))
    (if (null? pairs)
        (cstruct fields)
        (loop (cons (list (caar pairs) (cbase (cdar pairs))) fields)
              (cdr pairs)))))

(define (rand-mtype-val mtype)
  (define (urand bits) (random (expt 2 bits)))
  (define (srand bits) (random (- (expt 2 bits) (expt 2 (1- bits)))))
  (case mtype
    ((u8) (urand 8)) ((i8) (srand 8))
    ((u16 u16le u16be) (urand 16)) ((s16 s16le s16be) (srand 16))
    ((u32 u32le u32be) (urand 32)) ((s32 s32le s32be) (srand 32))
    ((u64 u64le u64be) (urand 64)) ((s64 s64le s64be) (srand 64))
    ((f32 f32le f32be f64 f64le f64be)
     (* (1- (* 2 (random 2))) (* (random (expt 2 8)))
        (expt (exact->inexact 2) (- (random 8) (random 8)))))))

(define (gen-scm-test-code pairs case-num seed)
  (let ((name (string-append "test" (number->string case-num))))
    ;; for each field, generate a random value
    (cons
     `(define ,(string->symbol name)
        (pointer->procedure
         int (foreign-library-function "ztest" ,name)
         (cons '* ,@(map (lambda (pair)
                           (mtype->ffi-type-name (mtypeof-basetype (cdr pair))))
                         pairs))))
     seed)))

;; ----- run it ---------------------------

;;(sf "~s ~s\n" 'f32 (rand-mtype-val 'f32))
;;(sf "~s ~s\n" 'i32 (rand-mtype-val 'i32))
;;(sf "~s ~s\n" 'u16 (rand-mtype-val 'u16))

;;(define pairs-1 (make-rand-pairs 3))
;;(define struct-1 (make-struct-from-pairs pairs-1))
;;(sf "caggte:\n") (pretty-print-caggate (ctype-info struct-1))
;;(sf "ffi struct:\n") (pp (cstruct->ffi-struct struct-1))
;;(sf "c struct\n") (pp (cstruct->c-struct struct-1))

;;(sf "pairs-1:\n") (pp pairs-1)
;;(pp (gen-c99-test-code pairs-1 1))
;;(pretty-print-c99 (gen-c99-test-code pairs-1 1))
;;(pp (gen-scm-test-code pairs-1 1))

(define *nfld* 5)
(define *ntst* 1)
(use-modules (system foreign))
(use-modules (system foreign-library))

(define cases
  (map (lambda (ix) (cons ix (make-rand-pairs *nfld*))) (iota 3)))

(define (gen-code cases)
  (with-output-to-file "ztest.c"
    (lambda ()
      (pretty-print-c99
       (cons 'trans-unit (fold gen-c99-test-code '() cases)))))
  (system "gcc -o ztest.so -shared -fPIC ztest.c")
  "ztest")

#|
(define (foo)
  (call-with-values
      (lambda ()
        (let loop ((scmxl '()) (c99xl '()) (n *ntst*))
          (if (zero? n)
              (values scmxl c99xl)
              (let ((pairs (make-rand-pairs *nfld*)))
                (loop (gen-scm-test-code pairs n scmxl)
                      (gen-c99-test-code pairs n c99xl)
                      (1- n))))))
    (lambda (scmxl c99xl)
      (let* ((c99port (open-output-file "ztest.c")))
        (pretty-print-c99 `(trans-unit . c99xl) c99port)
        (close-port c99port)
        (system "gcc -o ztest.so -shared -fPIC ztest.c")
        (let loop ((n 1) (xl scmxl))
          (let* ((fname (string-append "test" (number->string n)))
                 (ftn (foreign-library-function "ztest" fname)))
            #f))))))
|#

;; build:
;; 1. -> random list of pairs
;; 2. NOPE pairs -> ctype (setters, getters)
;; 3. pairs -> cfctn
;; 4. pairs -> pointer->procedure (pairs -> cbases -> ffi-struct)
;; use:
;; 1. pairs -> random call (setterswasher
;; 2. pairs -> check

(define (test1)
  (let* ((case-num 1)
         (pairs '((a . int) (b . int) (c . unsigned-int) (d . double)
                  (e . double)))
         (tname (string-append "test" (number->string case-num)))
         (names (map car pairs))
         (types (map cdr pairs))
         (foo-ct (cstruct (map (lambda (name type) (list name (cbase type)))
                             names types)))
         (foo-cd (make-cdata foo-ct))
         (f (pointer->procedure
             int (foreign-library-pointer "ztest" tname)
             (cons '* (map (lambda (t) (mtype->ffi-type (mtypeof-basetype t)))
                           types))))
         (sptr (cdata-val (pointer-to foo-cd)))
         (args (map (lambda (t) (rand-mtype-val (mtypeof-basetype t))) types))
         (res (apply f sptr args))
         )
    ;;(pperr foo-cd)
    (sf "sptr: ~s\n" sptr)
    (sf "args: ~s\n" args)
    ;;(sf "sval.a: ~s\n" (cdata-val foo-cd 'a))
    (if #f #f)
    foo-cd))

(define v (test1))
;; --- last line ---
