;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(add-to-load-path (getcwd))

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 cxeval))
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
(define pp99 pretty-print-c99)

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

(define mode 'file)
(define mode 'decl)
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

(define adecl #f)
;;(ppsx cpp-defs)
;;(ppsx inc-dirs)
;;(ppsx inc-help)

;;(use-modules ((system foreign) #:prefix ffi:))
(use-modules (system foreign))
;;(use-modules (system base pmatch))
(use-modules (ice-9 match))

(use-modules (arch-info))

(define ffi-type-map
  `(("void" . ,void) ("float" . ,float) ("double" . ,double) ("short" . ,short)
    ("short int" . ,short) ("signed short" . ,short)
    ("signed short int" . ,short) ("int" . ,int) ("signed" . ,int)
    ("signed int" . ,int) ("long" . ,long) ("long int" . ,long)
    ("signed long" . ,long) ("signed long int" . ,long)
    ("unsigned short int" . ,unsigned-short)
    ("unsigned short" . ,unsigned-short)
    ("unsigned int" . ,unsigned-int) ("unsigned" . ,unsigned-int)
    ("unsigned long int" . ,unsigned-long) ("unsigned long" . ,unsigned-long)
    ("char" . ,int8) ("signed char" . ,int8) ("unsigned char" . ,uint8)
    ("wchar_t" . ,int) ("char16_t" . ,int16) ("char32_t" . ,int32)
    ("long long" . ,long) ("long long int" . ,long)
    ("signed long long" . ,long) ("signed long long int" . ,long)
    ("unsigned long long" . ,unsigned-long)
    ("unsigned long long int" . ,unsigned-long) ("_Bool" . ,int8)
    ("size_t" . ,size_t) ("ssize_t" . ,ssize_t) ("ptrdiff_t" . ,ptrdiff_t)
    ;;
    ("int8_t" . int8) ("uint8_t" . uint8) 
    ("int16_t" . int16) ("uint16_t" . uint16) 
    ("int32_t" . int32) ("uint32_t" . uint32) 
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("intptr_t" . long) ("uintptr_t" . unsigned-long)
    ("char" . int8) ("signed char" . int8) ("unsigned char" . uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ;; FIXME:
    ("long long" . long) ("long long int" . long)
    ("signed long long" . long) ("signed long long int" . long)
    ("unsigned long long" . unsigned-long)
    ("unsigned long long int" . unsigned-long)
    ("_Bool" . int8)))

;; return (values sizeof alignof)
(define soao-pointer (values (sizeof '*) (alignof '*)))

;; sizeof alignof type mdecl tail
(define* (soaotmt expr udict #:optional (ddict '()))
  ;;(if (and (pair? mdecl-tail) (string? (car mdecl-tail))) (error "xxx"))
  (match expr
    (`((pointer-to) . ,rest)
     (values (sizeof '*) (alignof '*)))
    (`((array-of) . ,rest) 'ffi-void*)
    (`((array-of ,size) . ,rest) 'ffi-void*)
    (`((fixed ,name))
     (cond
      ((assoc-ref ffi-type-map name)
       => (lambda (t) (values (sizeof t) (alignof t))))
      (else (sf "no FFI type for ~A" name))))
    (`((float-type ,name))
     (cond
      ((assoc-ref ffi-type-map name)
       => (lambda (t) (values (sizeof t) (alignof t))))
      (else (sf "no FFI type for ~A" name))))
    (`((typename ,name))
     (cond
      ((assoc-ref ffi-type-map name)
       => (lambda (t) (values (sizeof t) (alignof t))))
      (else (sf "no FFI type for ~A" name))))
    ;;(((void)) 'void)
    (`((enum-def . ,rest2))
     (values (sizeof int) (alignof int)))
    (`((enum-ref . ,rest2) . ,rest1)
     (values (sizeof int) (alignof int)))
    (`((struct-def (field-list . ,fields)))
     (let loop ((sol '()) (aol '()) (ma 0) (flds fields))
       (cond
	((null? flds)
	 ;; compute layout
	 #f)
	(else
	 (let* ((udict (unitize-comp-decl (car flds)))
		(name (caar udict))
		(udecl (cdar udict))
		(udecl (udecl-rem-type-qual udecl))
		(mdecl (udecl->mdecl udecl)))
	   (call-with-values
	       (lambda () (soaotmt (cdr mdecl) udict ddict))
	     (lambda (so ao)
	       (loop (cons so sol) (cons ao aol) (max ao ma) (cdr flds)))))))))

    (`((struct-def (ident ,name) ,field-list))
     (soaotmt `((struct-def ,field-list)) udict ddict))
    
    #;(((union-def (field-list . ,fields)))
     ;; TODO check libffi on how unions are passed and returned.
     ;; I assume here never passed as a floating point.
     ;; This should use bounding-struct-descriptor from bytestructures
     (let loop ((type #f) (size 0) (flds fields))
       (if (null? flds)
	   (case type ((double) 'uint64) ((float) 'uint32) (else type))
	   (let* ((udict (unitize-comp-decl (car flds)))
		  (udecl (cdar udict))
		  (udecl (udecl-rem-type-qual udecl))
		  (mdecl (udecl->mdecl udecl))
		  (ftype (mtail->ffi-desc (cdr mdecl)))
		  ;;(ftval (assq-ref ffi-symmap ftype))
		  (fsize (sizeof ftype)))
	     (if (> fsize size)
		 (loop ftype fsize (cdr flds))
		 (loop type size (cdr flds)))))))
    #;(`((union-def (ident ,name) ,field-list))
     (soaotmt `((union-def ,field-list))))

    (_
     (sf "soaotmt:\n") (pp expr) (quit)
     (error "") (sf "soaotmt missed: ~S" expr))))

(define* (eval-sizeof-type tree udict #:optional (ddict '()))
  (sx-match (sx-ref tree 1)
    #;((type-name (decl-spec-list (type-spec (typename ,name))))
     (let* ((xname (expand-typename name udict))
	    (ffi-type (assoc-ref ffi-type-map xname)))
       (unless ffi-type ;; work to go
	 (throw 'c99-error "cxeval: failed to expand \"sizeof(~A)\"" name))
       (sizeof ffi-type)))
    ((type-name (decl-spec-list (type-spec (fixed-type ,name))))
     (values (sizeof-basename name) (alignof-basename name)))
    ((type-name (decl-spec-list (type-spec (float-type ,name))))
     (values (sizeof-basename name) (alignof-basename name)))
    ((type-name (decl-spec-list (type-spec . ,_1)) (abs-declr (pointer)))
     (values (sizeof-basename '*) (alignof-basename '*)))
    (else
     (pp tree)
     (throw 'c99-error "failed to expand sizeof type ~S" (sx-ref tree 1)))))

(let* (
       (code "int len = sizeof(\"abc\" \"def\");\n")
       (code "#include <sys/epoll.h>\n")
       (code "int foo[10];")
       (code (string-append
	      "typedef struct { int x; double z; void *p; } foo_t;\n"
	      "int x = sizeof(float);"))
       (tree (parse-string code))
       (udict (c99-trans-unit->udict tree))
       (udecl (assoc-ref udict "x"))
       (sot-x (sx-ref* udecl 2 2 1))
       )
  (pp udecl)
  (pp sot-x)
  (call-with-values
      (lambda () (eval-sizeof-type sot-x udict))
    (lambda (so ao) (sf "so=~S  ao=~S\n" so ao)))
  #t)

;; --- last line ---
