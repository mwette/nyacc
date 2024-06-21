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

;; to make random struct with n fields
(define names #(0 a b c d e f g h i j k l m n o p q r))
(define types #(int unsigned-int short unsigned-short float double))
(define ntype (vector-length types))
(define nitype (- ntype 2))

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

;; @deffn {Procedure} rand-mtype-val mtype [width]
;; make random value for base type (e.g., s16le), with optional restriction
;; to bitfield width.
;; @end deffn
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

;; ----- run it ---------------------------

(define *nfld* 5)
(define *ntst* 1)
(define c99-basename "ztest")
(use-modules (system foreign))
(use-modules (system foreign-library))

(define (gen-c99-test-code kase)
  (define fields (cdr kase))
  (define sn (string-append "test" (number->string (car kase))))
  (define (mk-field fld)
    (if (= 3 (length fld))
        (format #f "  ~a ~a: ~a;\n" (tsym->str (cadr fld)) (car fld) (caddr fld))
        (format #f "  ~a ~a;\n" (tsym->str (cadr fld)) (car fld))))
  (define (mk-sparam fld)
    (format #f ", ~a ~a" (tsym->str (list-ref fld 1)) (list-ref fld 0)))
  (define (mk-gparam fld)
    (format #f ", ~a *~a" (tsym->str (list-ref fld 1)) (list-ref fld 0)))
  (define (mk-setter fld)
    (format #f " t->~a = ~a;" (list-ref fld 0) (list-ref fld 0)))
  (define (mk-getter fld)
    (format #f " *~a = t->~a;" (list-ref fld 0) (list-ref fld 0)))
  (string-append
   "struct " sn " {\n"
   (apply string-append (map mk-field fields)) "};\n\n"
   "unsigned long " sn "_set(struct " sn " *t"
   (apply string-append (map mk-sparam fields)) ") {\n"
   (apply string-append (map mk-setter fields))
   "\n return sizeof(*t);\n}\n\n"
   "unsigned long " sn "_get(struct " sn " *t"
   (apply string-append (map mk-gparam fields)) ") {\n"
   (apply string-append (map mk-getter fields))
   "\n return sizeof(*t);\n}\n\n\n"))

(define (gen-code cases)
  (with-output-to-file (string-append c99-basename ".c")
    (lambda ()
      (for-each (lambda (code) (display code)) (map gen-c99-test-code cases))
      (display "#include <stdio.h>\n")
      (display "int Zmain() { printf(\"%ld\\n\", sizeof(struct test0)); }\n\n")
      ))
  (system (simple-format #f "gcc -g -o ~a.so -shared -fPIC ~a.c"
                         c99-basename c99-basename))
  c99-basename)


(define (exec-test case-num fields)
  (define (field->rand-val fld)
    (rand-mtype-val (mtypeof-basetype (cadr fld))
                    (and (pair? (cddr fld)) (caddr fld))))
  (define (type->ffi t) (mtype->ffi-type (mtypeof-basetype t)))
  (define testlib #f)

  (dynamic-wind
    (lambda () (set! testlib (load-foreign-library c99-basename)) )

    (lambda ()
      (let* ((sname (string-append "test" (number->string case-num) "_set"))
             (gname (string-append "test" (number->string case-num) "_get"))
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
             (set-ftn (pointer->procedure
                       unsigned-long
                       (foreign-library-pointer testlib sname)
                       (cons '* (map (lambda (t) (type->ffi t)) types))))
             (get-ftn (pointer->procedure
                       unsigned-long
                       (foreign-library-pointer testlib gname)
                       (cons '* (map (lambda (t) '*) types))))
             (sptr (cdata-ref (cdata& t-cd)))
             (vars (map (lambda (t) (make-cdata (cbase t))) types))
             (refs (map (lambda (v) (bytevector->pointer (cdata-bv v))) vars)))
        (fold
         (lambda (n seed)
           (and seed
                (let* ((vals (map field->rand-val fields))
                       (res (apply set-ftn sptr vals)))
                  (unless (eqv? res size)
                    (format #t "size mismatch: c99=~s vs scm=~s\n" res size)
                    (format #t "               ~s\n" fields)
                    (quit))
                  (fold
                   (lambda (name type value seed)
                     (let ((myval (cdata-ref (cdata-sel t-cd name))))
                       (unless (eqv? myval value)
                         (format #t "set-miss: ~s ~s got ~s\n" name value myval))
                       (and (eqv? myval value) seed)))
                   (eqv? res size) names types vals))
                (let* ((vals (map field->rand-val fields)))
                  (for-each
                   (lambda (name value) (cdata-set! (cdata-sel t-cd name) value))
                   names vals)
                  (apply get-ftn sptr refs)
                  (fold
                   (lambda (name var value seed)
                     (let* ((myval (cdata-ref var)))
                       (unless (eqv? myval value)
                         (format #t "get-miss: ~s ~s got ~s\n" name value myval))
                       (and seed (eqv? myval value))))
                   #t names vars vals))))
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

(define tS1 (cstruct `((a ,(cbase 'int)) (b ,(cbase 'double)))))
(define dS1 (make-cdata tS1))
(define tU1 (cunion `((a ,(cbase 'int)) (b ,(cbase 'double)))))
(define dU1 (make-cdata tU1))

;; --- last line ---
