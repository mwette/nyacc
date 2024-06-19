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

(define (unload-foreign-library lib)
  ((@@ (system foreign-library) dlclose)
   ((@@ (system foreign-library) foreign-library-handle) lib)))


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
     (* (1- (* 2.0 (random 2))) (* (random (expt 2 8)))
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
(define (gen-c99-test-code kase)
  (define fields (cdr kase))
  (define sn (string-append "test" (number->string (car kase))))
  (define (mk-field fld)
    (if (= 3 (length fld))
        (format #f "  ~a ~a: ~a;\n" (tsym->str (cadr fld)) (car fld) (caddr fld))
        (format #f "  ~a ~a;\n" (tsym->str (cadr fld)) (car fld))))
  (define (mk-tparam fld)
    (format #f ", ~a ~a" (tsym->str (list-ref fld 1)) (list-ref fld 0)))
  (define (mk-setter fld)
    (format #f "  t->~a = ~a;\n" (list-ref fld 0) (list-ref fld 0)))
  (string-append
   "struct " sn " {\n"
   (apply string-append (map mk-field fields)) "};\n\n"
   "unsigned long " sn "(struct " sn " *t"
   (apply string-append (map mk-tparam fields)) ") {\n"
   (apply string-append (map mk-setter fields))
   "  return sizeof(*t);\n}\n\n\n"))

;; ----- generate scm code --------------

(define (make-struct-from-pairs pairs)
  (let loop ((fields '()) (pairs pairs))
    (if (null? pairs)
        (cstruct fields)
        (loop (cons (list (caar pairs) (cbase (cdar pairs))) fields)
              (cdr pairs)))))

(define (gen-scm-test-code fields case-num seed)
  (let ((name (string-append "test" (number->string case-num))))
    ;; for each field, generate a random value
    (cons
     `(define ,(string->symbol name)
        (pointer->procedure
         int (foreign-library-function "ztest" ,name)
         (cons '* ,@(map (lambda (fld)
                           (mtype->ffi-type-name (mtypeof-basetype (cdr fld))))
                         fields))))
     seed)))

;; ----- run it ---------------------------

(define *nfld* 5)
(define *ntst* 1)
(define c99-basename "ztest")
(use-modules (system foreign))
(use-modules (system foreign-library))

(define (gen-code cases)
  (with-output-to-file (string-append c99-basename ".c")
    (lambda ()
      (for-each (lambda (code) (display code)) (map gen-c99-test-code cases))
      (display "#include <stdio.h>\n")
      (display "int Zmain() { printf(\"%ld\\n\", sizeof(struct test0)); }\n\n")
      ))
  (system (simple-format #f "gcc -o ~a.so -shared -fPIC ~a.c"
                         c99-basename c99-basename))
  c99-basename)

;; build:
;; 1. -> random list of pairs
;; 2. NOPE pairs -> ctype (setters, getters)
;; 3. pairs -> cfctn
;; 4. pairs -> pointer->procedure (pairs -> cbases -> ffi-struct)
;; use:
;; 1. pairs -> random call (setterswasher
;; 2. pairs -> check

(define (exec-test case-num fields)

  (define (field->rand-val fld)
    (rand-mtype-val (mtypeof-basetype (cadr fld))
                    (and (pair? (cddr fld)) (caddr fld))))

  (define testlib #f)

  (dynamic-wind

    (lambda () (set! testlib (load-foreign-library c99-basename)) )

    (lambda ()
      (let* ((tname (string-append "test" (number->string case-num)))
             (names (map car fields))
             (types (map cadr fields))
             (t-ct (cstruct
                    (map (lambda (fld)
                           (match fld
                             ((name type) (list name (cbase type)))
                             ((name type width) (list name (cbase type) width))))
                         fields)))
             (size (ctype-size t-ct))
             (t-cd (make-cdata t-ct))
             (bv (cdata-bv t-cd))
             (ftn (pointer->procedure
                   ;;unsigned-long (foreign-library-pointer c99-basename tname)
                   unsigned-long
                   (foreign-library-pointer testlib tname)
                   (cons '* (map (lambda (t)
                                   (mtype->ffi-type (mtypeof-basetype t)))
                                 types))))
             (sptr (cdata-val (pointer-to t-cd))))
        (fold
         (lambda (n seed)
           (and seed
                (let* ((args (map field->rand-val fields))
                       (res (apply ftn sptr args)))
                  (unless (eqv? res size)
                    (format #t "size mismatch: c99=~s vs scm=~s\n" res size)
                    (format #t "               ~s\n" fields)
                    (quit))
                  (fold
                   (lambda (name type value seed)
                     (unless (eqv? (cdata-val (cdata-ref t-cd name)) value)
                       (format #t "value mismatch: ~s ~s got ~s\n" name value
                               (cdata-val (cdata-ref t-cd name))))
                     (and seed))
                   (eqv? res size) names types args))))
         #t (iota 3))))

    (lambda () (unload-foreign-library testlib))))


;; executing do-test twice makes it always crash
(define* (do-test #:optional (n 3))
  (define cases
    (map (lambda (ix) (cons ix (make-rand-fields *nfld*))) (iota n)))
  (define so-file (gen-code cases))
  (fold
   (lambda (kase seed) (and seed (exec-test (car kase) (cdr kase))))
   #t cases))

(define (show-cstruct ct)
  (let* ((nf (ctype-info ct))
         (fl (cstruct-fields nf)))
    (for-each
     (lambda (f)
       (let ((ct (cfield-type f)) (n (cfield-name f)) (o (cfield-offset f)))
         (sf "~s ~s ~s\n" n o (ctype-info ct))))
     fl)))

(define (foo case-num fields values)
  (let* ((tname (string-append "test" (number->string case-num)))
         (names (map car fields))
         (types (map cadr fields))
         (t-ct (cstruct
                (map (lambda (fld)
                       (match fld
                         ((name type) (list name (cbase type)))
                         ((name type width) (list name (cbase type) width))))
                     fields)))
         (size (ctype-size t-ct))
         (t-cd (make-cdata t-ct))
         (bv (cdata-bv t-cd))
         (ftn (pointer->procedure
               unsigned-long (foreign-library-pointer "ztest" tname)
               (cons '* (map (lambda (t) (mtype->ffi-type (mtypeof-basetype t)))
                             types))))
         (sptr (cdata-val (pointer-to t-cd))))
    (apply ftn sptr values)
    (show-cstruct t-ct)
    (pp values)
    (sf "b\n")
    (sf "  ~s\n" (cdata-val (cdata-ref t-cd 'b))))
    #;(for-each
     (lambda (n)
       (sf "~s\n" n)
     names)
    ))

(define xfields `((a ,(cbase 'unsigned-short))
                  (b ,(cbase 'short) 5)
                  (c ,(cbase 'short) 8)
                  (d ,(cbase 'short) 5)
                  (e ,(cbase 'unsigned-int) 6)))
(define fields '((a unsigned-short)
                 (b short 5)
                 (c short 8)
                 (d short 5)
                 (e unsigned-int 6)))
(define values '(1 -1 1 1 1))

;;(define xx (cstruct fields))
;;(sf "sizeof xx = ~s\n" (ctype-size xx))
;;(show-cstruct xx)
;;(pp values)
;;(foo 0 fields values)

(define ct1 (cstruct fields))
(define cd1 (make-cdata ct1))
(define bv1 (cdata-bv cd1))
(for-each
 (lambda (ix) (bytevector-u8-set! bv1 ix 255))
 (iota (ctype-size ct1)))
(cdata-set! (cdata-ref cd1 'b) 2)
(cdata-set! (cdata-ref cd1 'c) 6)

;; --- last line ---
