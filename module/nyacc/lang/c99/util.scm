;;; nyacc/lang/c99/util.scm - C parser utilities

;; Copyright (C) 2015-2018 Matthew R. Wette
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

(define-module (nyacc lang c99 util)
  #:export (c99-std-help
	    get-gcc-cpp-defs get-gcc-inc-dirs
	    remove-inc-trees
	    merge-inc-trees!
	    move-attributes
	    elifify)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((srfi srfi-1) #:select (append-reverse fold-right))
  #:use-module (srfi srfi-2) ;; and-let*
  #:use-module (sxml fold)
  #:use-module (ice-9 popen)		; gen-gcc-cpp-defs
  #:use-module (ice-9 rdelim)		; gen-gcc-cpp-defs
)

;; include-helper for C99 std
(define c99-std-help
  '(("alloca.h")
    ("complex.h" "complex" "imaginary" "_Imaginary_I=C99_ANY" "I=C99_ANY")
    ("ctype.h")
    ("fenv.h" "fenv_t" "fexcept_t")
    ("float.h" "float_t" "FLT_MAX=C99_ANY" "DBL_MAX=C99_ANY")
    ("inttypes.h"
     "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
     "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
     "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
     "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t"
     "imaxdiv_t")
    ("limits.h"
     "INT_MIN=C99_ANY" "INT_MAX=C99_ANY" "LONG_MIN=C99_ANY" "LONG_MAX=C99_ANY")
    ("math.h" "float_t" "double_t")
    ("regex.h" "regex_t" "regmatch_t")
    ("setjmp.h" "jmp_buf")
    ("signal.h" "sig_atomic_t")
    ("stdarg.h" "va_list")
    ("stddef.h" "ptrdiff_t" "size_t" "wchar_t")
    ("stdint.h"
     "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
     "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
     "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
     "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t")
    ("stdio.h" "FILE" "size_t")
    ("stdlib.h" "div_t" "ldiv_t" "lldiv_t" "wchar_t")
    ("string.h" "size_t")
    ("strings.h" "size_t")
    ("time.h" "time_t" "clock_t" "size_t")
    ("unistd.h" "size_t" "ssize_t" "div_t" "ldiv_t")
    ("wchar.h" "wchar_t" "wint_t" "mbstate_t" "size_t")
    ("wctype.h" "wctrans_t" "wctype_t" "wint_t")
    ))

(define (resolve-CC CC)
  (cond
   (CC CC)
   ((getenv "CC") => identity)
   (else "gcc")))

;; @deffn {Procedure} convert-def line
;; Convert string in gcc cpp defs to pair of strings for term and replacement.
;; @end deffn
(define (convert-line line)
  (with-input-from-string line
    (lambda ()
      (let loop ((term '()) (acc '()) (st 0) (ch (read-char)))
	(case st
	  ((0) ;; skip #define
	   (if (char=? ch #\space)
	       (loop term acc 1 (read-char))
	       (loop term acc 0 (read-char))))
	  ((1) ;; read term
	   (if (char=? ch #\space)
	       (loop (reverse-list->string acc) '() 2 (read-char))
	       (loop term (cons ch acc) st (read-char))))
	  ((2) ;; read rest
	   (if (or (eof-object? ch) (char=? ch #\newline))
	       (string-append term "=" (reverse-list->string acc))
	       (loop term (cons ch acc) st (read-char)))))))))

;; @deffn {Procedure} get-gcc-cpp-defs [args] [#:CC "gcc"] => '("ABC=123" ...)
;; Generate a list of default defines produced by gcc (or other comiler).
;; If keyword arg @arg{CC} is not provided this procedure looks for environment
;; variable @code{"CC"}, else it defaults to @code{"gcc"}.
;; @end deffn
(define* (get-gcc-cpp-defs #:optional (args '()) #:key CC)
  ;; @code{"gcc -dM -E"} will generate lines like @code{"#define ABC 123"}.
  ;; We generate and return a list like @code{'(("ABC" . "123") ...)}.
  (let* ((cmd (string-append (resolve-CC CC) " -dM -E - </dev/null"))
	 (ip (open-input-pipe cmd)))
    (let loop ((line (read-line ip 'trim)))
      (if (eof-object? line) '()
	  (cons (convert-line line) (loop (read-line ip 'trim)))))))

;; @deffn {Procedure} get-gcc-inc-dirs [args] [#:CC "gcc"] =>
;; Generate a list of compiler-internal include directories (for gcc).  If
;; keyword arg @arg{CC} is not provided this procedure looks for environment
;; variable @code{"CC"}, else it defaults to @code{"gcc"}.
;; @end deffn
(define* (get-gcc-inc-dirs #:optional (args '()) #:key CC)
  (let ((ip (open-input-pipe (string-append
			      (resolve-CC CC) " -E -Wp,-v - </dev/null 2>&1"))))
    (let iter ((dirs '()) (grab #f) (line (read-line ip 'trim)))
      (cond
       ((eof-object? line) dirs)
       ((string=? line "#include <...> search starts here:")
	(iter dirs #t (read-line ip 'trim)))
       ((string=? line "End of search list.") dirs)
       (grab
	(iter (cons (string-trim-both line) dirs)
	      grab (read-line ip 'trim)))
       (else
	(iter dirs grab (read-line ip 'trim)))))))

;; @deffn {Procedure} remove-inc-trees tree
;; Remove the trees included with cpp-include statements.
;; @example
;; '(... (cpp-stmt (include "<foo.h>" (trans-unit ...))) ...)
;; => '(... (cpp-stmt (include "<foo.h>")) ...)
;; @end example
;; @end deffn
(define (remove-inc-trees tree)
  (if (not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
  (let iter ((rslt (make-tl 'trans-unit))
	     ;;(head '(trans-unit)) (tail (cdr tree))
	     (tree (cdr tree)))
    (cond
     ((null? tree) (tl->list rslt))
     ((and (eqv? 'cpp-stmt (car (car tree)))
	   (eqv? 'include (caadr (car tree))))
      (iter (tl-append rslt `(cpp-stmt (include ,(cadadr (car tree)))))
	    (cdr tree)))
     (else (iter (tl-append rslt (car tree)) (cdr tree))))))

;; @deffn {Procedure} merge-inc-trees tree
;; Remove the trees included with cpp-include statements.
;; @example
;; '(... (cpp-stmt (include "<foo.h>" (trans-unit (stmt ...))) ...)
;; => '(... (stmt...) ...)
;; @end example
;; @end deffn
#;(define (Xmerge-inc-trees tree)
  (if (not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
  (let iter ((rslt (make-tl 'trans-unit))
	     (tree (cdr tree)))
    (cond
     ((null? tree) (tl->list rslt))
     ((and (eqv? 'cpp-stmt (caar tree)) (eqv? 'include (cadar tree)))
      (iter (tl-extend rslt (cdr (merge-inc-trees (cdddar tree)))) (cdr tree)))
     (else (iter (tl-append rslt (car tree)) (cdr tree))))))


;; @deffn {Procedure} merge-inc-trees! tree => tree
;; This will (recursively) merge code from cpp-includes into the tree.
;; @example
;; (trans-unit
;;  (decl (a))
;;  (cpp-stmt (include "<hello.h>" (trans-unit (decl (b)))))
;;  (decl (c)))
;; =>
;; (trans-unit (decl (a)) (decl (b)) (decl (c)))
;; @end example
;; @end deffn
(define (merge-inc-trees! tree)

  ;; @item find-span (trans-unit a b c) => ((a . +->) . (c . '())
  (define (find-span tree)
    (cond
     ((not (pair? tree)) '())		; maybe parse failed
     ((not (eqv? 'trans-unit (car tree))) (error "expecting c-tree"))
     ((null? (cdr tree)) (error "null c99-tree"))
     (else
      (let ((fp tree))			; first pair
	(let iter ((lp tree)		; last pair
		   (np (cdr tree)))	; next pair
	  (cond
	   ((null? np) (cons (cdr fp) lp))
	   ;; The following is an ugly hack to find cpp-include
	   ;; with trans-unit attached.
	   ((and-let* ((expr (car np))
		       ((eqv? 'cpp-stmt (car expr)))
		       ((eqv? 'include (caadr expr)))
		       (rest (cddadr expr))
		       ((pair? rest))
		       (span (find-span (car rest))))
		      (set-cdr! lp (car span))
		      (iter (cdr span) (cdr np))))
	   (else
	    (set-cdr! lp np)
	    (iter np (cdr np)))))))))

  ;; Use cons to generate a new reference:
  ;; (cons (car tree) (car (find-span tree)))
  ;; or not:
  (find-span tree)
  tree)


;; --- attributes ----------------------

(define (join-string-literal str-lit)
  (sx-list 'string (sx-attr str-lit) (string-join (sx-tail str-lit) "")))

;; used in c99-spec actions for attribute-specifiers
(define (attr-expr-list->string attr-expr-list)
  (string-append "(" (string-join (cdr attr-expr-list) ",") ")"))

;; (attribute-list (ident "packed") (attribute (ident "aligned" ...)))
;; => (attributes "packed;aligned(8);...")
(define (attr-spec->attr attr-spec)
  (define (spec->str spec)
    (sx-match spec
      ((ident ,name) name)
      ((attribute ,name) (spec->str name))
      ((attribute ,name ,args)
       (string-append (spec->str name) "(" (spec->str args) ")"))
      ((attr-expr-list . ,expr-list)
       (string-join (map spec->str expr-list) ","))
      ((fixed ,val) val)
      ((string ,val) (string-append "\"" val "\""))
      (,_ (simple-format #t "c99/body: missed ~S\n" spec) "MISSED")))
  `(attributes ,(string-join (map spec->str (sx-tail attr-spec)) ";")))
(export attr-spec->attr)

;; move @code{(attributes ...)} from under @code{decl-spec-list} to
;; under @code{@}
;; @example
;; (decl (decl-spec-list
;;         (attributes "__packed__" "__aligned__")
;;         (attributes "__alignof__(8)"))
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;;  =>
;; (decl (@ (attributes "__packed__;__aligned__;__alignof__(8)"))
;;       (decl-spec-list
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;; @end example
(define (XXX-move-attributes decl)
  (define (comb-attr attl attr)
    (cons `(attributes
	    ,(string-join
	      (let loop ((rz '()) (al attl))
		(if (null? al) rz (loop (cons (cadar al) rz) (cdr al))))
	      ";")) attr))
  (define (make-specl spl)
    (cons 'decl-spec-list (reverse spl)))
  (let ((tag (sx-tag decl))
	(attr (sx-attr decl))
	(spec-l (sx-ref decl 1))
	(declr-l (sx-ref decl 2)))
    (let loop ((attl '()) (spl1 '()) (spl0 (sx-tail spec-l)))
      (cond
       ((null? spl0)
	(if (null? attl) decl
	    (sx-list tag (comb-attr attl attr) (make-specl spl1) declr-l)))
       ((eq? 'attribute-list (sx-tag (car spl0)))
	(loop (cons (attr-spec->attr (car spl0)) attl) spl1 (cdr spl0)))
       (else
	(loop attl (cons (car spl0) spl1) (cdr spl0)))))))

;; ((attribute-list ...) (type-spec ...) (attribute-list ...)) =>
;;   (values (attribute-list ...)  ((type-spec ...) ...))
;; @deffn extract-attr spec-tail => (values attr-tree spec-tail)
;; Extract attributes from a decl-spec-list.
;; @end deffn
(define (extract-attr decl-spec-tail) ;; => (values attr-tree spec-tail)
  (let loop ((atl '()) (dst1 '()) (dst0 decl-spec-tail))
    (cond
     ((null? dst0)
      (if (null? atl)
	  (values '() decl-spec-tail)
	  (values `(attribute-list . ,atl) (reverse dst1))))
     ((eq? 'attribute-list (sx-tag (car dst0)))
      (loop (append (sx-tail (car dst0)) atl) dst1 (cdr dst0)))
     (else
      (loop atl (cons (car dst0) dst1) (cdr dst0))))))
(export extract-attr)

;; (attribute-list (attribute (ident "__packed__")) ...)
;;  =>
;; (attributes "__packed__;...")
;; OR
;; () => ()
(define (attrl->attrs attr-list)
  (define (spec->str spec)
    (sx-match spec
      ((ident ,name) name)
      ((attribute ,name) (spec->str name))
      ((attribute ,name ,args)
       (string-append (spec->str name) "(" (spec->str args) ")"))
      ((attr-expr-list . ,expr-list)
       (string-join (map spec->str expr-list) ","))
      ((fixed ,val) val)
      ((string ,val) (string-append "\"" val "\""))
      (,_ (simple-format #t "c99/body: missed ~S\n" spec) "MISSED")))
  (if (null? attr-list) '()
      `(attributes ,(string-join (map spec->str (sx-tail attr-list)) ";"))))
(export attrl->attrs)

;; rethink this -- assumes no attributes transferred to decl-spec-list
(define (move-specl-attr decl-spec-list)
  (let ((tag (sx-tag decl-spec-list))
	(attr (sx-attr decl-spec-list))
	(tail (sx-tail decl-spec-list)))
    (sx-cons*
     tag
     attr
     (call-with-values (lambda () (extract-attr tail))
       (lambda (attr-list spec-tail)
	 (fold-right
	  (lambda (form tail)
	    (cons
	     (sx-match form
	       ((type-spec ,spec)
		(sx-match spec
		  (((struct-ref struct-def) . ,rest)
		   (let ((attrs (attrl->attrs attr-list)))
		     `(type-spec
		       ,(sx-cons* (sx-tag spec)
				  (if (pair? attrs) (list attrs) '())
				  rest))))
		  (,_ form)))
	       (,_ form))
	     tail))
	  '() spec-tail))))))
(export move-specl-attr)

(define (move-attributes sx)
  (sferr "move-attributes <= sx:\n") (pperr sx)
  (sx-match sx
    ((decl (@ . ,attr) ,specl0 ,dclrl0)
     (let ((specl1 (move-specl-attr specl0))
	   ;;(dclr1 (move-dclrl-attr dclrl))
	   (dclrl1 dclrl0))
       (cond
	((and (eq? specl1 specl0) (eq? dclrl1 dclrl0)) sx)
	((eq? dclrl1 dclrl0) (sx-list attr specl1 dclrl0))
	((eq? specl1 specl0) (sx-list attr specl0 dclrl1))
	(else (sx-list 'decl attr specl1 dclrl1)))))
    ((decl (@ . ,attr) ,specl0)
     (let ((specl1 (move-specl-attr specl0)))
       (cond
	((eq? specl1 specl0) sx)
	(else (sx-list 'decl attr specl1)))))
    ((decl-spec-list . ,_) (move-specl-attr sx))
    (else
     (sferr "move-attributes: missed:\n") (pperr sx)
     (error "move-attributes"))))
(export move-attributes)

;; "__packed__;__aligned__;__alignof__(8)"
;;   =>
;; (attribute-list (attribute "__packed
(define (parse-attributes attr-str)
  (let ((attl (string-split attr-str ";"))
	)
    `(attribute-list)))

;; --- random stuff 

;; @deffn {Procedure} elifify tree => tree
;; This procedure will find patterns of
;; @example
;; (if cond-1 then-part-1
;;            (if cond-2 then-part-2
;;                       else-part-2
;; @end example
;; @noindent
;; and convert to
;; @example
;; (if cond-1 then-part-1
;;            (elif cond-2 then-part-2)
;;            else-part-2
;; @end example
;; @end deffn
(define (elifify tree)
  (define (fU tree)
    (sx-match tree
      ((if ,x1 ,t1 (if ,x2 ,t2 (else-if ,x3 ,t3) . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) (else-if ,x3 ,t3) . ,rest))
      ((if ,x1 ,t1 (if ,x2 ,t2 . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) . ,rest))
      (else
       tree))
    )
  (foldt fU identity tree))

;; --- last line ---
