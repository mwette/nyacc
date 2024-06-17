;; ctest.scm

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules (rnrs bytevectors))
(use-modules (system foreign))
(use-modules (system foreign-library))
(use-modules (nyacc lang arch-info))
(use-modules (nyacc lang cdata))

(use-modules (ice-9 pretty-print))
(define (pp exp)
  (pretty-print exp (current-output-port) #:per-line-prefix "  "))
(define (sf fmt . args)
  (apply simple-format (current-output-port) fmt args))


(define (tsym->str tsym)
  (string-map (lambda (c) (if (char=? #\- c) #\space c)) (symbol->string tsym)))

;; void struct_init(foo_t* *s, int a, double b, ...)

(set! *random-state* (random-state-from-platform))

;; make random struct with n fields
(define names #(a a b c d e f g h i j k l m n o p q r))
(define types #(int unsigned-int short unsigned-short float double))

(define nname (vector-length names))
(define ntype (vector-length types))
(define nitype (- ntype 2))

(define* (rand-mtype-val mtype #:optional width)
  (define (urand bits) (1+ (random (1- (expt 2 (or width bits))))))
  (define (srand bits) (- (random (expt 2 (or width bits)))
                          (expt 2 (1- (or width bits)))))
  (case mtype
    ((u8) (urand 8)) ((i8) (srand 8))
    ((u16 u16le u16be) (urand 16)) ((s16 s16le s16be) (srand 16))
    ((u32 u32le u32be) (urand 32)) ((s32 s32le s32be) (srand 32))
    ((u64 u64le u64be) (urand 64)) ((s64 s64le s64be) (srand 64))
    ((f32 f32le f32be f64 f64le f64be)
     (* (1- (* 2 (random 2))) (* (random (expt 2 8)))
        (expt (exact->inexact 2) (- (random 8) (random 8)))))))

(define (make-rand-fields n)
  (define (rtype n) (vector-ref types (random n)))
  (let loop ((flds '()) (pbf #f) (n n))
    (if (zero? n) flds
        (let* ((rndN (random 3))
               (cbf (if pbf (positive? rndN) (zero? rndN)))
               (name (vector-ref names n)))
          (loop (cons (if cbf
                          (list name (rtype nitype) (1+ (random 8)))
                          (list name (rtype ntype)))
                      flds)
                cbf (1- n))))))

;; ----- generate c99 code --------------

(use-modules (nyacc lang c99 pprint))

;; SCM test1(struct foo *foo, int a, uint3_t b)
(define (gen-c99-test-code kase seed)
  (define case-num (car kase))
  (define fields (cdr kase))
  (define sn (string-append "test" (number->string case-num)))
  (define (mk-field fld)
    (if (= 3 (length fld))
        (format #f "  ~a ~a: ~a;\n" (tsym->str (cadr fld)) (car fld) (caddr fld))
        (format #f "  ~a ~a;\n" (tsym->str (cadr fld)) (car fld))))
  (define (mk-tparam fld)
    (format #f ", ~a ~a" (tsym->str (list-ref fld 1)) (list-ref fld 0)))
  (define (mk-setter fld)
    (format #f "  t->~a = ~a;\n" (list-ref fld 0) (list-ref fld 0)))

  (cons*
   (string-append
    "struct " sn " {\n"
    (apply string-append (map mk-field fields))
    "};\n\n")
   (string-append
    "unsigned long " sn "(struct " sn " *t"
    (apply string-append (map mk-tparam fields)) ") {\n"
    (apply string-append (map mk-setter fields))
    "  return sizeof(*t);\n}\n\n\n")
   seed))

;; ----- generate scm code --------------

(define (make-struct-from-pairs pairs)
  (let loop ((fields '()) (pairs pairs))
    (if (null? pairs)
        (cstruct fields)
        (loop (cons (list (caar pairs) (cbase (cdar pairs))) fields)
              (cdr pairs)))))

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

(define *nfld* 5)
(define *ntst* 1)
(use-modules (system foreign))
(use-modules (system foreign-library))

(define cases
  (map (lambda (ix) (cons ix (make-rand-fields *nfld*))) (iota 3)))

(define (gen-code cases)
  (with-output-to-file "ztest.c"
    (lambda ()
      (pretty-print-c99
       (cons 'trans-unit (fold gen-c99-test-code '() cases)))))
  (system "gcc -o ztest.so -shared -fPIC ztest.c")
  "ztest")

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
         (fields '((a int) (b int) (c unsigned-int) (d double)))
         (tname (string-append "test" (number->string case-num)))
         (names (map car fields))
         (types (map cadr fields))
         (foo-ct (cstruct (map (lambda (name type) (list name (cbase type)))
                               names types)))
         (foo-cd (make-cdata foo-ct))
         (f (pointer->procedure
             int (foreign-library-pointer "ztest" tname)
             (cons '* (map (lambda (t) (mtype->ffi-type (mtypeof-basetype t)))
                           types))))
         (sptr (cdata-val (pointer-to foo-cd)))
         (args (map (lambda (t) (rand-mtype-val (mtypeof-basetype t))) types))
         (res (apply f sptr args)))
    (sf "test1 args: ~s\n" args)
    foo-cd))

(define (exec-test case-num fields)
  (let* ((tname (string-append "test" (number->string case-num)))
         (names (map car fields))
         (types (map cadr fields))
         (t-ct (cstruct
                (map (lambda (fld)
                       (match fld
                         ((name type) (list name (cbase type)))
                         ((name type width) (list name (cbase type) width))))
                     fields)))
         (t-cd (make-cdata t-ct))
         (ftn (pointer->procedure
               unsigned-long (foreign-library-pointer "ztest" tname)
               (cons '* (map (lambda (t) (mtype->ffi-type (mtypeof-basetype t)))
                             types))))
         (sptr (cdata-val (pointer-to t-cd)))
         (args (map (lambda (fld)
                      (rand-mtype-val (mtypeof-basetype (cadr fld))
                                      (and (pair? (cddr fld)) (caddr fld))))
                    fields))
         (res (apply ftn sptr args))
         (size (ctype-size t-ct))
         (bv (cdata-bv t-cd))
         )
    (and
     (eqv? (ctype-size t-ct) res)
     (fold
      (lambda (fld arg seed)
        (and (eqv? (cdata-val (cdata-ref t-cd) 'b) arg) seed))
      #t fields args))))

;;(define d2 (exec-test 2 '((a int) (b int) (c unsigned-int) (d double))))
;;(sf "~s\n" (make-rand-fields 4))
;;(sf "~s\n" (make-rand-fields 5))
;;(sf "~s\n" (make-rand-fields 6))
;;(sf "~s\n" (make-rand-fields 7))
;;(pp cases)

(sf "~a" (gen-c99-test-code (car cases) '()))

;; --- last line ---
