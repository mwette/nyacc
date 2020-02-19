;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(add-to-load-path (getcwd))

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppsx (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))

(define cpp-defs
  (cond
   ((string-contains %host-type "darwin") '("__GNUC__=6" "__signed=signed"))
   (else (get-gcc-cpp-defs))))
(define inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/usr/include" "c99-exam"
     "/usr/include/glib-2.0" "/usr/lib/x86_64-linux-gnu/glib-2.0/include"
     ;;"/usr/include/dbus-1.0" "/usr/lib/x86_64-linux-gnu/dbus-1.0/include"
     ;;
     "/usr/include/cairo" "/usr/include/glib-2.0"
     "/usr/lib/x86_64-linux-gnu/glib-2.0/include"
     "/usr/include/pixman-1" "/usr/include/freetype2" "/usr/include/libpng12"
     )
   (get-gcc-inc-dirs)))
(define inc-help c99-def-help)

(define mode 'code)
(define debug #f)
(define xdef? (lambda (name mode) (memq mode '(code decl))))

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      ;;(pp cpp-defs) (pp inc-help) (pp inc-dirs)
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:show-incs #t
		 ;;#:xdef? xdef?
		 ))))

(define (parse-string str)
  ;;(simple-format #t "~S => \n" str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:show-incs #f
 		 #:mode mode #:debug debug
		 #:xdef? xdef?))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

;; The standard says:
;;  "For two qualified types to be compatible, both shall have the identically
;;   qualified version of a compatible type; the order of type qualifiers within
;;   a list of specifiers or qualifiers does not affect the specified type."

;;(and=> (parse-file "c99-exam/ex14.c") ppsx)
;;(and=> (parse-c99x "(a*b)+c") ppsx)

(use-modules (arch-info))
(use-modules (system foreign))

;;(define (def-namer) (symbol->string (gensym "@")))
(define (def-namer) "@")

;; => (values sizeof-val align-of)
(define* (eval-sizeof-type tree #:optional (udict '()) (ddict '()))
  (sx-match (sx-ref tree 1)
    ((type-name (decl-spec-list (type-spec (fixed-type ,name))))
     (values (sizeof-basetype name) (alignof-basetype name)))
    ((type-name (decl-spec-list (type-spec (float-type ,name))))
     (values (sizeof-basetype name) (alignof-basetype name)))
    ((type-name (decl-spec-list (type-spec (typename ,name))))
     (if (sizeof-basetype name)
	 (values (sizeof-basetype name) (alignof-basetype name))
	 (error "fixme")))
    (else
     (throw 'c99-error "failed to expand sizeof type ~S" (sx-ref tree 1)))))


;; === from cxeval.scm:
(define (expand-typename typename udict)
  (let* ((decl `(udecl (decl-spec-list
			(type-spec (typename ,typename)))
		       (declr (ident "_"))))
	 (xdecl (expand-typerefs decl udict))
	 (xname (and xdecl (sx-ref* xdecl 1 1 1 1))))
    xname))
;; ===


#;(define* (udecl->ify-decl-tail decl-tail #:optional (udict '()) (ddict '()))
    (sx-match decl-tail
    (((pointer-to) . ,rest) 'ffi-void*)
    (((array-of) . ,rest) 'ffi-void*)
    (((array-of ,size) . ,rest) 'ffi-void*)
    (((fixed-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI type for ~A" name)))
    (((float-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI type for ~S" name)))
    (((typename ,name) . ,rest)
     (or (assoc-ref ffi-typemap name)
	 (fherr "no FFI type for ~S" name)))
    (((void)) 'ffi:void)
    (((enum-def . ,rest2) . ,rest1) 'ffi:int)
    (((enum-ref . ,rest2) . ,rest1) 'ffi:int)

    (((struct-def (field-list . ,fields)))
     `(list ,@(map (lambda (fld)
		     (let* ((udict (unitize-comp-decl fld))
			    (name (caar udict))
			    (udecl (cdar udict))
			    (udecl (udecl-rem-type-qual udecl))
			    (mdecl (udecl->mdecl udecl)))
		       (mtail->ffi-desc (cdr mdecl))))
		   fields)))
    (((struct-def (ident ,name) ,field-list))
     (mtail->ffi-desc `((struct-def ,field-list))))
    
    (((union-def (field-list . ,fields)))
     ;; TODO check libffi on how unions are passed and returned.
     ;; I assume here never passed as a floating point.
     ;; This should use bounding-struct-descriptor from bytestructures
     (let loop ((type #f) (size 0) (flds fields))
       (if (null? flds)
	   (case type ((double) 'ffi:uint64) ((float) 'ffi:uint32) (else type))
	   (let* ((udict (unitize-comp-decl (car flds)))
		  (udecl (cdar udict))
		  (udecl (udecl-rem-type-qual udecl))
		  (mdecl (udecl->mdecl udecl))
		  (ftype (mtail->ffi-desc (cdr mdecl)))
		  (ftval (assq-ref ffi-symmap ftype))
		  (fsize (sizeof ftval)))
	     (if (> fsize size)
		 (loop ftype fsize (cdr flds))
		 (loop type size (cdr flds)))))))
    (((union-def (ident ,name) ,field-list))
     (mtail->ffi-desc `((union-def ,field-list))))
    
    (,otherwise
     (sferr "mtail->ffi-desc missed:\n") (pperr mdecl-tail) ;;(quit)
     (error "") (fherr "mtail->ffi-desc missed: ~S" mdecl-tail))))


;; @deffn {Procedure} reify-decl-tail decl-tail [udict ddict #:namer proc]
;; This procedure turns tails of abstract declarations into init-declr's.
;; This is useful for sending through @code{udecl->mdecl} for the purpose
;; of processing with munge tools.
;; @end deffn
(define* (reify-decl-tail decl-tail  ;; to udecl-tail
			  #:optional (udict '()) (ddict '())
			  #:key (namer def-namer))

  (define (reify-ad declr)
    (sx-match declr
      ((abs-ptr-declr ,ptr) `(abs-ptr-declr ,ptr (ident ,(namer))))
      ((abs-ptr-declr ,ptr ,dcl) `(abs-ptr-declr ,ptr ,(reify-ad dcl)))
      ((abs-ary-declr) `(ary-declr (ident ,(namer))))
      ((abs-ary-declr ,arg) `(ary-declr (ident ,(namer)) ,arg))
      ((abs-ary-declr ,dad ,arg) `(ary-declr ,(reify-ad dad) ,arg))
      ((abs-ftn-declr ,pms) `(ftn-declr (ident ,(namer)) ,pms))
      ((abs-ftn-declr ,dad ,pms) `(ftn-declr ,(reify-ad dad) ,pms))
      ((scope ,dad) `(scope ,(reify-ad dad)))
      (,_ (pp declr) (error "coding error: reify-ad"))))
  
  (sx-match-tail decl-tail
    (((decl-spec-list . ,stail))
     `((decl-spec-list . ,stail) (init-declr (ident ,(namer)))))
    (((decl-spec-list . ,specl-tail) ,declr)
     `((decl-spec-list . ,specl-tail) (init-declr ,(reify-ad declr))))
    ((,_) (pp decl-tail) (error "coding error: reify-decl-tail"))))

;; int, int*, int*[], int **, int(), int(float), int*(float),
;; int*(float)[3]

(when #t
  (let* ((code (string-append
		;;"typedef int bar_t[2];\n"
		"typedef int *bar_t;\n"
		;;"bar_t foo(bar_t (*)(bar_t));\n" ;; <= param-list broken
		"int foo(bar_t);\n" ;; <= param-list broken
		))
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 ;;(xdecl (expand-typerefs udecl udict))
	 )
    (pp udecl)
    (pp (expand-typerefs udecl udict))
    ;;(pp xdecl)
    ;;(pp99 udecl)
    ;;(pp99 xdecl)
    #t))

(when #f
  (let* ((code (string-append
		"typedef struct { int x; double z; void *p; } foo_t;\n"
		"int x = sizeof(int*(*)(int));\n"
		"int *(*y)(int);\n"
		))
	 (tree (parse-string code))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "x"))
	 (sot-x (sx-ref* udecl 2 2 1))
	 (tail (sx-tail (sx-ref sot-x 1)))
	 (ctail (sx-tail (assoc-ref udict "y")))
	 )
    (newline)
    (sf "abstract tail:\n")
    (ppsx tail)
    (pp99 udecl)
    (sf "vs concrete tail:\n")
    (ppsx ctail)
    (pp99 `(udecl . ,ctail))
    (sf "=> concrete tail:\n")
    (let ((rtail (reify-decl-tail tail udict)))
      (ppsx rtail)
      (pp99 `(udecl . ,rtail)))

    #t))

;; --- last line ---
