;; ctest.scm

(use-modules (system foreign))
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

(define (make-struct-from-pairs pairs)
  (let loop ((fields '()) (pairs pairs))
    (if (null? pairs)
        (cstruct fields)
        (loop (cons (list (caar pairs) (cbase (cdar pairs))) fields)
              (cdr pairs)))))

(use-modules (nyacc lang c99 pprint))

(define pairs-1 (make-rand-pairs 3))
(define struct-1 (make-struct-from-pairs pairs-1))
(sf "pairs-1:\n") (pp pairs-1)
;;(sf "caggte:\n") (pretty-print-caggate (ctype-info struct-1))
;;(sf "ffi struct:\n") (pp (cstruct->ffi-struct struct-1))
;;(sf "c struct\n") (pp (cstruct->c-struct struct-1))

(define (rand-mtype-val mtype)
  (define (urand bits) (random (expt 2 bits)))
  (define (srand bits) (random (- (expt 2 bits) (expt 2 (1- bits)))))
  (case mtype
    ((u8) (urand 8)) ((i8) (srand 8))
    ((u16 u16le u16be) (urand 16)) ((i16 i16le i16be) (srand 16))
    ((u32 u32le u32be) (urand 32)) ((i32 i32le i32be) (srand 32))
    ((u64 u64le u64be) (urand 64)) ((i64 i64le i64be) (srand 64))
    ((f32 f32le f32be f64 f64le f64be)
     (* (1- (* 2 (random 2)))
        (* (random (expt 2 8)))
        (expt (exact->inexact 2) (* (1- (* 2 (random 2))) (random 5)))))))

(define (gen-scm-test-ftn n struct)
  ;; for each field, generate a random value
  #f)

;; SCM test1(struct foo *foo, int a, uint3_t b)
(define (gen-c99-test-ftn n struct)
  (define sn (string-append "test" (number->string n)))
  (define (mk-param tspecv pdeclrv)
    `(param-decl (decl-spec-list (type-spec ,tspecv)) (param-declr ,pdeclrv)))
  (define (mk-sparam)
    (mk-param `(struct-ref (ident ,sn)) `(ptr-declr (pointer) (ident ,sn))))
  (define (mk-fparam type name)
    (mk-param `(ident ,sn) `(ident ,sn)))
  (define (assn fldname)
    `(expr-stmt (assn-expr (i-sel (ident ,fldname) (p-expr (ident ,sn)))
                           (op "=") (p-expr (ident ,fldname)))))
  (let (
        )
    `(fctn-defn
      (decl-spec-list (type-spec (fixed-type "long int")))
      (ftn-declr
       (ident ,sn)
       (param-list
        ,(mk-sparam)
        ,(mk-fparam "uint32_t" "a")
        ,(mk-fparam "uint16_t" "b")))
      (compd-stmt
       (block-item-list
        ,(assn "a")
        ,(assn "b"))))))

(sf "~s ~s\n" 'f32 (rand-mtype-val 'f32))
(sf "~s ~s\n" 'i32 (rand-mtype-val 'i32))
(sf "~s ~s\n" 'u16 (rand-mtype-val 'u16))

;; --- last line ---
