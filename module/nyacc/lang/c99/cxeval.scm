;;; nyacc/lang/c99/c99eval.scm - evaluate constant expressions

;; Copyright (C) 2018-2024 Matthew Wette
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

(define-module (nyacc lang c99 cxeval)
  #:export (parse-c99-cx
            eval-c99-cx
            size-and-align-of-type
            eval-sizeof-type
            eval-alignof-type
            eval-sizeof-expr
            eval-typeof-expr
            eval-offsetof
            find-offsets
            find-sizes
            find-types
            sizeof-mtail
            cx-incr-size
            cx-maxi-size)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((nyacc lang util) #:select (make-tl tl-append tl->list))
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang arch-info)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 munge-base)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (system foreign)
  #:use-module (ice-9 match))

(use-modules (ice-9 pretty-print))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))

;; (string "abc" "dev")
(define (sizeof-string-const value)
  #f)

(include-from-path "nyacc/lang/c99/mach.d/c99cx-act.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99cx-tab.scm")

(define c99cx-raw-parser
  (make-lalr-parser
   (acons 'act-v c99cx-act-v c99cx-tables)))

(define gen-c99cx-lexer
  (let* ((reader (make-comm-reader '(("/*" . "*/"))))
         (comm-skipper (lambda (ch) (reader ch #f))))
    (make-lexer-generator c99cx-mtab
                          #:comm-skipper comm-skipper
                          #:chlit-reader read-c-chlit
                          #:num-reader read-c-num)))

(define (parse-c99-cx text)
  (with-throw-handler
      'c99-error
    (lambda ()
      (with-input-from-string text
        (lambda () (c99cx-raw-parser (gen-c99cx-lexer)))))
    (lambda (key fmt . args)
      (apply throw 'cpp-error fmt args))))

(define* (expand-typename typename udict #:key (namer def-namer))
  (let* ((decl `(udecl (decl-spec-list
                        (type-spec (typename ,typename)))
                       (declr (ident (namer)))))
         (xdecl (expand-typerefs decl udict))
         (xname (and xdecl (sx-ref* xdecl 1 1 1 1))))
    xname))

;; Update struct running-size (rs) given new item size (s) and align't (a).
(define (cx-incr-size s a rs)
  (+ s (* a (quotient (+ rs (1- a)) a))))

;; Update struct running-size (rs) given new bit-field size
;; bs: bit count, a: alignment, rs: running size
(define (cx-incr-bit-size bs a rs)
  (error "not done")
  (let ((ba (* 8 a)) (brs (* 8 rs)))
    (cond
     ((zero? bs) (/ (* ba (quotient (+ brs (1- ba)) ba)) 8))
     ((= (quotient (+ brs bs) ba) (quotient brs ba)) (/ (+ bs ba bs) 8))
     (else (/ (+ bs (* ba (quotient (+ brs (1- ba)) ba))) 8)))))

;; Update running union size (rs) given new item s and a.
(define (cx-maxi-size s a rs)
  (max s rs))

(define incr-size cx-incr-size)
(define incr-bit-size cx-maxi-size)
(define maxi-size cx-maxi-size)

(define (mkcdl specl declrs)
          (map (lambda (declr) `(comp-declr-list ,specl ,declr)) declrs))

(define (sizeof-mtail mtail udict) ;; => (values size align)

  (define (bfud exp mtail size align pwbf) ; bit-field update
    (call-with-values
        (lambda () (sizeof-mtail mtail udict))
      (lambda (elt-sz elt-al)
        (let* ((sz (if pwbf size (incr-size 0 align size)))
               (bz (eval-c99-cx exp udict))
               (sz (incr-bit-size bz elt-al sz)))
          (values sz (max elt-al align))))))

  (define (exec/decl decl size base-align pwbf update)
    ;; pwbf: prev was bit-field
    (let ((mtail (sx-tail (sx-find 'type-spec (sx-ref decl 1))))
          (declrs (or (and=> (sx-ref decl 2) sx-tail) '((ident "_")))))
      (let loop ((size size) (align base-align) (pwbf pwbf) (dlrs declrs))
        (if (null? dlrs) (values size align pwbf)
            (match (car dlrs)
              (`(comp-declr (bit-field ,nam ,exp))
               (call-with-values (lambda () (bfud exp size align pwbf mtail))
                 (lambda (elt-sz elt-al) (loop elt-sz elt-al #t (cdr dlrs)))))
              (`(comp-declr (bit-field ,exp))
               (call-with-values (lambda () (bfud exp size align pwbf mtail))
                 (lambda (elt-sz elt-al) (loop elt-sz elt-al #t (cdr dlrs)))))
              (_
               (call-with-values
                   (lambda ()
                     (let* ((mt (cdr (m-unwrap-declr (car dlrs) mtail))))
                       (sizeof-mtail mt udict)))
                 (lambda (elt-sz elt-al)
                   (let ((sz (if pwbf (incr-size 0 align size) size)))
                     (loop (update elt-sz elt-al sz) (max elt-al align)
                           #f (cdr dlrs)))))))))))

  (match mtail
    (`((pointer-to) . ,rest)
     (values (sizeof-basetype '*) (alignof-basetype '*)))
    (`((fixed-type ,name))
     (values (sizeof-basetype name) (alignof-basetype name)))
    (`((float-type ,name))
     (values (sizeof-basetype name) (alignof-basetype name)))
    (`((array-of ,dim) . ,rest)
     (let ((mult (eval-c99-cx dim udict)))
       (call-with-values
           (lambda () (sizeof-mtail rest udict))
         (lambda (size align) (values (* mult size) align)))))
    (`((struct-def (field-list . ,fields)))
     (let loop ((size 0) (align 0) (pwbf #f) (flds fields))
       (match flds
         ('() (values (incr-size 0 align size) align))
         (`((comment ,text) . ,rest) (loop size align pwbf rest))
         (`((comp-decl . ,_) . ,rest)
          (call-with-values
              (lambda () (exec/decl (car flds) size align pwbf incr-size))
            (lambda (size align pwbf) (loop size align pwbf rest)))))))
    (`((struct-def (ident ,name) (field-list . ,fields)))
     (sizeof-mtail `((struct-def (field-list . ,fields))) udict))
    (`((union-def (field-list . ,fields)))
     (let loop ((size 0) (align 0) (flds fields))
       (match flds
         ('() (values (incr-size 0 align size) align))
         (`((comment ,text) . ,rest) (loop size align rest))
         (`((comp-decl . ,_) . ,rest)
          (call-with-values
              (lambda () (exec/decl (car flds) size align #f maxi-size))
            (lambda (size align pwbf) (loop size align rest)))))))
    (`((union-def (ident ,name) (field-list . ,fields)))
     (sizeof-mtail `((union-def (field-list . ,fields))) udict))
    (`((enum-ref . ,rest))
     (values (sizeof-basetype "int") (alignof-basetype "int")))
    (`((enum-def . ,rest))
     (values (sizeof-basetype "int") (alignof-basetype "int")))
    (_ (sferr "c99/eval-sizeof-mtail: missed\n") (pperr mtail)
       (throw 'c99-error "coding error"))))

;; => (values size align)
(define* (sizeof-specl/declr specl declr #:optional (udict '()))
  (let* ((udecl `(udecl ,specl ,declr))
         (xdecl (expand-typerefs udecl udict))
         (mdecl (udecl->mdecl xdecl)))
    (sizeof-mtail (cdr mdecl) udict)))

(define (trim-mtail mtail)
  (case (caar mtail)
    ((extern) (trim-mtail (cdr mtail)))
    ((comment) (trim-mtail (cdr mtail)))
    ((initzer) (trim-mtail (cdr mtail)))
    (else mtail)))

;; @deffn {Procedure} size-and-align-of-type type-name [udict]
;; @deffx {Procedure} eval-sizeof-type sizeof-form [udict]
;; @deffx {Procedure} eval-alignof-type alignof-form [udict]
;; @example
;; (size-and-align-of-type '(sizeof-type (ident "foo_t"))) => (values 4 2)
;; (eval-sizeof-type '(sizeof-type (ident "foo_t"))) => 4
;; (eval-alignof-type '(alignof-type (ident "foo_t"))) => 2
;; @end example
;; @end deffn
(define* (size-and-align-of-type type-name #:optional (udict '()))
  (let* ((specl (sx-ref type-name 1))
         (declr (or (sx-ref type-name 2) '(param-declr))))
    (sizeof-specl/declr specl declr udict)))

(define* (eval-sizeof-type form #:optional (udict '()))
  (call-with-values
      (lambda () (size-and-align-of-type (sx-ref form 1) udict))
    (lambda (size align) size)))

(define* (eval-alignof-type form #:optional (udict '()))
  (call-with-values
      (lambda () (size-and-align-of-type (sx-ref form 1) udict))
    (lambda (size align) align)))

;; @deffn {Procedure} size-and-align-of-expr tree [udict]
;; => (values sizeof-val align-of)
;; @end deffn
(define* (size-and-align-of-expr tree #:optional (udict '()))

  (define (sizeof-literal tree)
    (sx-match tree
      ((sizeof-expr ,expr) (sizeof-literal expr))
      ((p-expr ,expr) (sizeof-literal expr))
      ((string . ,string-list)
       (cons
        (let loop ((sl string-list))
          (if (null? sl) 1
              (+ (string-length (car sl)) (loop (cdr sl)))))
        1))
      (,_ #f)))

  (define (gen-mtail tree)
    (sx-match tree
      ((sizeof-expr ,expr) (gen-mtail expr))
      ((p-expr ,expr) (gen-mtail expr))
      ((ident ,name)
       (let* ((udecl (assoc-ref udict name))
              (xdecl (and udecl (expand-typerefs udecl udict)))
              (mdecl (and xdecl (udecl->mdecl xdecl))))
         (if (not mdecl) (throw 'c99-error "not found: ~S" name))
         (trim-mtail (cdr mdecl))))
      ((array-ref ,elt ,expr)
       (let ((mtail (gen-mtail expr)))
         (match mtail
           (`((array-of ,dim) . ,rest) rest)
           (_ (throw 'c99-error "cxeval: can't ref array")))))
      ((de-ref ,expr)
       (let ((mtail (gen-mtail expr)))
         (match mtail
           (`((pointer-to) . ,rest) rest)
           (_ (throw 'c99-error "cxeval: can't de-ref")))))
      (,_ (throw 'c99-error "cxeval: can't sizeof ~S" (list tree)))))

  (let ((res (sizeof-literal tree)))
    (if res
        (values (car res) (cdr res))
        (sizeof-mtail (gen-mtail tree) udict))))

(define* (eval-sizeof-expr tree #:optional (udict '()))
  (call-with-values
      (lambda () (size-and-align-of-expr tree udict))
    (lambda (size align) size)))


;; =============================================================================

;; @deffn {Procedure} offsetof-mtail mtail desig base udict => offset
;; @end deffn
(define (offsetof-mtail mtail desig base udict)

  (define (do-aggr base fields update)
    (unless (equal? 'ident (caar desig))
      (throw 'c99-error "cxeval: desig ~S not for struct/union" (car desig)))
    (let loop ((offs base) (aln 0) (dsg (cadar desig))
               (decls '()) (flds fields))
      (cond
       ((pair? decls)
        (let* ((mdecl (udecl->mdecl (car decls)))
               (name (car mdecl))
               (mtail (cdr mdecl)))
          (call-with-values
              (lambda () (sizeof-mtail mtail udict))
            (lambda (elt-sz elt-al)
              (cond
               ((string=? name dsg)
                (let* ((offs (update 0 elt-al offs)))
                  (if (null? (cdr desig)) offs
                      (offsetof-mtail mtail (cdr desig) offs udict))))
               (else
                (loop (update elt-sz elt-al offs) (max elt-al aln)
                      dsg (cdr decls) flds)))))))
       ((pair? flds)
        (sx-match (car flds)
          ((comp-decl ,specl (comp-declr-list . ,declrs))
           (loop offs aln dsg (mkcdl specl declrs) (cdr flds)))
          ((comp-udecl ,specl ,declr)
           (loop offs aln dsg (mkcdl specl (list declr)) (cdr flds)))
          (,_ (loop offs aln dsg decls (cdr flds)))))
       (else #f))))  ;; not found

  (match mtail
    (`((array-of ,dim) . ,rest)
     (unless (number? (cadar desig)) (throw 'c99-error "bad designator"))
     (call-with-values
         (lambda ()(sizeof-mtail rest udict))
       (lambda (elt-sz elt-al)
         (let* ((offs (incr-size 0 elt-al base)) ; adjust for alignment
                (offs (+ offs (* (cadar desig) elt-sz))))
           (offsetof-mtail rest (cdr desig) offs udict)))))
    (`((struct-def (field-list . ,fields)))
     (do-aggr base fields incr-size))
    (`((struct-def (ident ,name) (field-list . ,fields)))
     (do-aggr base fields incr-size))
    (`((union-def (field-list . ,fields)))
     (do-aggr base fields maxi-size))
    (`((union-def (ident ,name) (field-list . ,fields)))
     (do-aggr base fields maxi-size))
    (_ (sferr "c99/eval-sizeof-mtail: missed\n") (pperr mtail)
       (throw 'c99-error "coding error"))))

(define* (unwrap-designator expr udict #:optional (seed '()))
  (sx-match expr
    ((p-expr (ident ,name)) (cons `(ident ,name) seed))
    ((d-sel (ident ,elt) ,expr)
     (unwrap-designator expr udict (cons `(ident ,elt) seed)))
    ((array-ref ,ix ,expr)
     (let ((ixval (eval-c99-cx ix udict)))
       (unwrap-designator expr udict (cons `(ary-ref ,ixval) seed))))
    (,_ (throw 'c99-error "cxeval: missed ~S\n" (list expr)))))
(export unwrap-designator)

;; @deffn {Procedure} eval-offsetof tree [udict]
;; where tree has the form
;; @example
;; (offsetof (type-name ...) designator-expr)
;; @end example
;; @example
;;   offsetof(foo_t, designator)
;; where
;;   designator: ident | designator "." ident | designator "[" expr "]"
;; @end example
;; @end deffn
(define* (eval-offsetof tree #:optional (udict '()))
  (sx-match tree
    ((offsetof-type (type-name ,spec-list ,declr) ,expr)
     (let* ((udecl `(udecl ,spec-list ,declr))
            (xdecl (expand-typerefs udecl udict))
            (mdecl (udecl->mdecl xdecl))
            (desig (unwrap-designator expr udict)))
       (offsetof-mtail (cdr mdecl) desig 0 udict)))
    ((offsetof-type (type-name ,spec-list) ,expr)
     (eval-offsetof
      `(offsetof-type (type-name ,spec-list (param-declr (ident "_"))) ,expr)
      udict))
    (,_ #f)))


;; =============================================================================

;; TODO:
;;   (define (eval-typeof-type type-name desig) ...)

(define (find-field fields name)
  (let loop ((specl #f) (declrs '()) (fields fields))
    (cond
     ((pair? declrs)
      (if (equal? (declr-name (car declrs)) name)
          `(type-name ,specl ,(car declrs))
          (loop specl (cdr declrs) fields)))
     ((pair? fields)
      (sx-match (car fields)
        ((comp-decl ,specl (comp-declr-list . ,declrs))
         (loop specl declrs (cdr fields)))
        ((comp-decl ,specl ,declr)
         (loop specl (list declr) (cdr fields)))
        (,_ (sferr "missed in cxeval(find-field)") #f)))
     (else (throw 'c99-error "field not found: ~A\n" (list name))))))

(define (lookup-type-field udecl name)
  (sx-match udecl
    ((type-name (decl-spec-list (type-spec (struct-def ,field-list))) . ,_)
     (let* ((field-list (clean-field-list field-list)))
       (find-field (sx-tail field-list) name)))
    ((type-name (decl-spec-list (type-spec (union-def ,field-list))) . ,_)
     (let* ((field-list (clean-field-list field-list)))
       (find-field (sx-tail field-list) name)))
    (,_ #f)))

;; is aggr type decl (not couting typedef
(define (aggr-decl? adecl)
  (let* ((specl (sx-ref adecl 1))
         (tspec (sx-find 'type-spec specl))
         (tag (sx-ref* tspec 1 1)))
    (and (memq tag '(struct-def union-def)))))

;; if struct or union ref, expand to struct or union def
(define (expose-aggr adecl udict)
  (define (key tag) (case tag ((struct-ref) 'struct) ((union-ref) 'union)))
  (let* ((specl (sx-ref adecl 1))
         (declr (sx-ref adecl 2))
         (tspec (sx-find 'type-spec specl))
         (tag (sx-tag (sx-ref* tspec 1))))
    (case tag
      ((typename)
       (call-with-values
           (lambda () (splice-typename specl declr (sx-ref* tspec 1 1) udict))
         (lambda (specl declr) (sx-list 'type-name #f specl declr))))
      ((struct-ref union-ref)
       (let* ((name (sx-ref* tspec 1 1 1))
              (form (cons (key tag) name))
              (specl (replace-aggr-ref specl name form udict)))
         (sx-list 'type-name #f specl declr)))
      (else adecl))))

;; @deffn {Scheme} eval-typeof-expr expr udict => `(type-name specl declr)
;; @var{expr} should be @code{(typeof-expr expr)} but is not.
;; @end deffn
(define (eval-typeof-expr expr udict)
  ;; a->b.c => a .b ->c ; a in udict, b in a, c in b
  ;; what about vectors?  DO LATER
  ;; a->b[1].c => a .b ->c ; a in udict, b in a, c in b
  (define (typeof-next next)
    (sx-match next
      ((ident ,name)
       (let* ((udecl (assoc-ref udict name))
              (specl (sx-ref udecl 1))
              (declr (sx-ref udecl 2)))
         `(type-name ,specl ,declr)))
      ((cast ,tname ,expr)
       tname)
      ((scope ,expr)
       (typeof-next expr))
      ((p-expr ,expr)
       (typeof-next expr))
      ((i-sel ,ident ,expr)
       (and expr (typeof-next `(d-sel ,ident (de-ref ,expr)))))
      ((d-sel (ident ,name) ,expr)
       (let* ((tname (typeof-next expr))
              (xdecl (and tname (expose-aggr tname udict))))
         (and xdecl
              (not (pointer-declr? (sx-ref xdecl 2)))
              (lookup-type-field xdecl name))))
      ((de-ref ,expr)
       (let ((tname (typeof-next expr)))
         (and tname
              (sx-match tname
                ((,decl ,specl (,declr (ptr-declr (pointer) ,expr)))
                 `(type-name ,specl (,declr ,expr)))
                ((,decl ,specl (,declr (ptr-declr (pointer ,rest) ,expr)))
                 `(type-name ,specl (,declr (ptr-declr ,rest) ,expr)))
                ((,decl ,specl (abs-ptr-declr (pointer)))
                 `(type-name ,specl)) ; maybe FIXME
                (,_ #f)))))
       ((array-ref ,indx ,expr)
        (let ((tname (typeof-next expr)))
          (and tname
               (sx-match tname
                 ((,decl ,specl (,declr (array-ref ,val ,expr)))
                  `(type-name ,specl (,declr ,expr)))
                 (,_  #f)))))))

  (typeof-next expr))

;; =============================================================================

;; @deffn {Procedure} eval-c99-cx tree [udict] [#:fail-proc fail-proc]
;; Evaluate the constant expression or return #f (for unimplemented or
;; non-expressions). If @code{fail-proc} is provided it is called with
;; the tree that could not be parsed.  If provided, it should return
;; @code{#f} or throw an exception.
;; @end deffn
(define* (eval-c99-cx tree #:optional udict ddict #:key fail-proc)

  (define (fail fmt . args)
    (and fail-proc (apply fail-proc fmt args)))

  (define (ddict-lookup name)
    (let ((repl (assoc-ref ddict name)))
      (cond
       ((not repl) #f)
       ((pair? repl) #f)
       ((string=? name repl) #f)
       (else repl))))

  (define (uop op ex)
    (and op ex (op ex)))

  (define (bop op lt rt)
    (and op lt rt (op lt rt)))

  (letrec
      ((ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))    ; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))    ; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))    ; eval expr in arg 3

       (eval-expr
        (lambda (tree)
          (case (sx-tag tree)
            ((fixed) (string->number (cnumstr->scm (sx-ref tree 1))))
            ((float) (string->number (cnumstr->scm (sx-ref tree 1))))
            ((char) (char->integer (string-ref (sx-ref tree 1) 0)))
            ((string) (string-join (sx-tail tree 1) ""))
            ((pre-inc post-inc) (uop 1+ (ev1 tree)))
            ((pre-dec post-dec) (uop 1- (ev1 tree)))
            ((pos) (and tree (ev1 tree)))
            ((neg) (uop - (ev1 tree)))
            ((not) (and tree (if (equal? 0 (ev1 tree)) 1 0)))

            ((mul) (bop * (ev1 tree) (ev2 tree)))
            ((div) (bop / (ev1 tree) (ev2 tree)))
            ((mod) (bop modulo (ev1 tree) (ev2 tree)))
            ((add) (bop + (ev1 tree) (ev2 tree)))
            ((sub) (bop - (ev1 tree) (ev2 tree)))
            ((lshift) (bop bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
            ((rshift) (bop bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
            ((lt) (if (bop < (ev1 tree) (ev2 tree)) 1 0))
            ((le) (if (bop <= (ev1 tree) (ev2 tree)) 1 0))
            ((gt) (if (bop > (ev1 tree) (ev2 tree)) 1 0))
            ((ge) (if (bop >= (ev1 tree) (ev2 tree)) 1 0))
            ((eq) (if (bop = (ev1 tree) (ev2 tree)) 1 0))
            ((ne) (if (bop = (ev1 tree) (ev2 tree)) 0 1))
            ((bitwise-not) (uop lognot (ev1 tree)))
            ((bitwise-or) (bop logior (ev1 tree) (ev2 tree)))
            ((bitwise-xor) (bop logxor (ev1 tree) (ev2 tree)))
            ((bitwise-and) (bop logand (ev1 tree) (ev2 tree)))
            ;;
            ((or)
             (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
               (if (and e1 e2) (if (and (zero? e1) (zero? e2)) 0 1) #f)))
            ((and)
             (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
               (if (and e1 e2) (if (or (zero? e1) (zero? e2)) 0 1) #f)))
            ((cond-expr)
             (let ((e1 (ev1 tree)) (e2 (ev2 tree)) (e3 (ev3 tree)))
               (if (and e1 e2 e3) (if (zero? e1) e3 e2) #f)))
            ;;
            ((sizeof-type)
             (catch 'c99-error
               (lambda () (eval-sizeof-type tree udict))
               (lambda (key fmt . args) (apply fail fmt args))))
            ((sizeof-expr)
             (catch 'c99-error
               (lambda () (eval-sizeof-expr tree udict))
               (lambda (key fmt . args) (apply fail fmt args))))
            ((alignof)
             (catch 'c99-error
               (lambda () (eval-alignof-type tree udict))
               (lambda (key fmt . args) (apply fail fmt args))))
            ((offsetof)
             (catch 'c99-error
               (lambda () (eval-offsetof tree udict))
               (lambda (key fmt . args) (apply fail fmt args))))
            ((ident)
             (let ((name (cadr tree)))
               (cond
                ((assoc-ref udict name) => eval-expr)
                ((ddict-lookup name) => string->number)
                (else (fail "cannot resolve identifier ~S" (sx-ref tree 1))))))
            ((p-expr) (ev1 tree))
            ((cast) (ev2 tree))
            ((fctn-call) #f)            ; assume not constant
            ((ref-to) #f)               ; assume address not useful
            ;;
            ;; TODO
            ((comp-lit) (fail "cxeval: comp-lit not implemented"))
            ((comma-expr) (fail "cxeval: comma-expr not implemented"))
            ((i-sel) (fail "cxeval: i-sel not implemented"))
            ((d-sel) (fail "cxeval: d-sel not implemented"))
            ((array-ref) (fail "cxeval: array-ref not implemented"))
            ;;
            ((c99x-deprecated)
             (sferr "eval-c99-cx:") (pperr tree)
             (throw 'c99-error "eval-c99-cx: coding error"))
            (else (fail "cxeval: non-expression"))))))

    (eval-expr tree)))

;; =============================================================================

(define (gen-offsets mtail base udict)

  (define (mt-al mtail)
    (call-with-values (lambda () (sizeof-mtail mtail udict))
      (lambda (sz al) al)))

  (define (do-aggr flds update)
    (let* ((aln (mt-al mtail)) (base (incr-size 0 aln base)))
      (let loop ((siz 0) (aln aln) (offs '()) (decls '()) (flds flds))
        (cond
         ((pair? decls)
          (let* ((mdecl (udecl->mdecl (car decls)))
                 (name (car mdecl)) (mtail (cdr mdecl)))
            (call-with-values (lambda () (gen-offsets mtail (+ base siz) udict))
              (lambda (el-sz el-al el-os)
                (let ((oval (if (pair? el-os) el-os
                                (+ base (incr-size 0 el-al siz)))))
                  (loop (update el-sz el-al siz) (max aln el-al)
                        (acons name oval offs) (cdr decls) flds))))))
         ((pair? flds)
          (sx-match (car flds)
            ((comp-decl ,specl (comp-declr-list . ,declrs))
             (loop siz aln offs (mkcdl specl declrs) (cdr flds)))
            #|
            ((comp-udecl ,specl (comp-declr (bit-field ,expr)))
            ;; zero means next field
             TODO)
            ((comp-udecl ,specl (comp-declr (bit-field ,ident ,expr)))
             TODO)
            |#
            ((comp-udecl ,specl ,declr)
             (loop siz aln offs (list declr) (cdr flds)))
            (,_
             (loop siz aln offs decls (cdr flds)))))
         (else (values (incr-size 0 aln siz) aln (reverse offs)))))))

  (match mtail
    (`((pointer-to) . ,rest)
     (let ((sz (sizeof-basetype '*)) (al (alignof-basetype '*)))
       (values sz al (incr-size 0 al base))))
    (`((fixed-type ,name))
     (let ((sz (sizeof-basetype name)) (al (alignof-basetype name)))
       (values sz al (incr-size 0 al base))))
    (`((float-type ,name))
     (let ((sz (sizeof-basetype name)) (al (alignof-basetype name)))
       (values sz al (incr-size 0 al base))))
    (`((array-of ,dim) . ,rest)
     (call-with-values (lambda () (gen-offsets rest base udict))
       (lambda (el-sz el-al el-of)
         (let ((base (incr-size 0 el-al base)))
           (let ((dim (eval-c99-cx dim udict)))
             (values (* dim el-sz) el-al (cons (cons dim el-sz) el-of)))))))
    (`((struct-def (field-list . ,flds)) . ,rest)
     (do-aggr flds incr-size))
    (`((struct-def (ident ,name) (field-list . ,flds)))
     (do-aggr flds incr-size))
    (`((union-def (field-list . ,flds)) . ,rest)
     (do-aggr flds maxi-size))
    (`((union-def (ident ,name) (field-list . ,flds)))
     (do-aggr flds maxi-size))
    (`((,(or 'enum-ref 'enum-def) . ,rest))
     (values (sizeof-basetype "int") (alignof-basetype "int") base))
    (_ (sferr "c99/gen-offsets: missed\n") (pperr mtail)
       (throw 'c99-error "coding error"))))

(define (find-offsets type-name udict)
  (sx-match type-name
    ((type-name ,spec-list ,declr)
     (let* ((udecl `(udecl ,spec-list ,declr))
            (xdecl (expand-typerefs udecl udict))
            (mdecl (udecl->mdecl xdecl)))
       (call-with-values
           (lambda () (gen-offsets (cdr mdecl) 0 udict))
         (lambda (size align offsets) offsets))))
    ((type-name ,spec-list)
     (find-offsets `(type-name ,spec-list (param-declr (ident "_"))) udict))
    (,_ #f)))

;; for array, provides (dim dim dim . elt-size)
(define (gen-sizes mtail udict)

  (define (mkcdl specl declrs)
    (map (lambda (declr) `(comp-declr-list ,specl ,declr)) declrs))

  (define (do-aggr flds)
    (let loop ((sizes '()) (decls '()) (flds flds))
        (cond
         ((pair? decls)
          (let* ((mdecl (udecl->mdecl (car decls)))
                 (name (car mdecl)) (mtail (cdr mdecl)))
            (loop (acons name (gen-sizes mtail udict) sizes)
                  (cdr decls) flds)))
         ((pair? flds)
          (sx-match (car flds)
            ((comp-decl ,specl (comp-declr-list . ,declrs))
             (loop sizes (mkcdl specl declrs) (cdr flds)))
            ((comp-udecl ,specl ,declr)
             (loop sizes (list declr) (cdr flds)))
            (,_
             (loop sizes decls (cdr flds)))))
         (else (reverse sizes)))))

  (match mtail
    (`((pointer-to) . ,rest) (sizeof-basetype '*))
    (`((fixed-type ,name)) (sizeof-basetype name))
    (`((float-type ,name)) (sizeof-basetype name))
    (`((array-of ,dim) . ,rest)
     (let ((el-sz (gen-sizes rest udict)) (dim (eval-c99-cx dim udict)))
       (cons dim (gen-sizes rest udict))))
    (`((struct-def (field-list . ,flds)) . ,rest) (do-aggr flds))
    (`((struct-def (ident ,name) (field-list . ,flds))) (do-aggr flds))
    (`((union-def (field-list . ,flds)) . ,rest) (do-aggr flds))
    (`((union-def (ident ,name) (field-list . ,flds))) (do-aggr flds))
    (`((,(or 'enum-ref 'enum-def) . ,rest)) (sizeof-basetype "int"))
    (_ (sferr "gen-sizes: missed\n") (pperr mtail)
       (throw 'c99-error "coding error"))))

(define (find-sizes type-name udict)
  (sx-match type-name
    ((type-name ,spec-list ,declr)
     (let* ((udecl `(udecl ,spec-list ,declr))
            (xdecl (expand-typerefs udecl udict))
            (mdecl (udecl->mdecl xdecl)))
       (gen-sizes (cdr mdecl) udict)))
    ((type-name ,spec-list)
     (find-sizes `(type-name ,spec-list (param-declr (ident "_"))) udict))
    (,_ #f)))

(define (gen-types mtail udict)

  (define (mkcdl specl declrs)
    (map (lambda (declr) `(comp-declr-list ,specl ,declr)) declrs))

  (define (do-aggr flds)
    (let loop ((types '()) (decls '()) (flds flds))
        (cond
         ((pair? decls)
          (let* ((mdecl (udecl->mdecl (car decls)))
                 (name (car mdecl)) (mtail (cdr mdecl)))
            (loop (acons name (gen-types mtail udict) types)
                  (cdr decls) flds)))
         ((pair? flds)
          (sx-match (car flds)
            ((comp-decl ,specl (comp-declr-list . ,declrs))
             (loop types (mkcdl specl declrs) (cdr flds)))
            ((comp-udecl ,specl ,declr)
             (loop types (list declr) (cdr flds)))
            (,_
             (loop types decls (cdr flds)))))
         (else (reverse types)))))

  (match mtail
    (`((typename ,name) . ,rest) name)
    (`((pointer-to) . ,rest) "void*")
    (`((fixed-type ,name)) name)
    (`((float-type ,name)) name)
    (`((array-of ,dim) . ,rest)
     (let ((el-ty (gen-types rest udict)) (dim (eval-c99-cx dim udict)))
       (cons dim (gen-types rest udict))))
    (`((struct-def (field-list . ,flds)) . ,rest) (do-aggr flds))
    (`((struct-def (ident ,name) (field-list . ,flds))) (do-aggr flds))
    (`((union-def (field-list . ,flds)) . ,rest) (do-aggr flds))
    (`((union-def (ident ,name) (field-list . ,flds))) (do-aggr flds))
    (`((,(or 'enum-ref 'enum-def) . ,rest)) "int")
    (_ (sferr "gen-types: missed\n") (pperr mtail)
       (throw 'c99-error "coding error"))))

(define* (find-types type-name udict #:optional (keep '()))
  (sx-match type-name
    ((type-name ,spec-list ,declr)
     (let* ((udecl `(udecl ,spec-list ,declr))
            (xdecl (expand-typerefs udecl udict keep))
            (mdecl (udecl->mdecl xdecl)))
       (gen-types (cdr mdecl) udict)))
    ((type-name ,spec-list)
     (find-types `(type-name ,spec-list (param-declr (ident "_"))) udict keep))
    (,_ #f)))

;; --- last line ---
