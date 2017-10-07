;; examples/nyacc/lang/c99/tryit.scm
;;(debug-set! stack 750)

(use-modules (srfi srfi-1))
(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 xparser))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 util1))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (ice-9 pretty-print))
(use-modules (sxml xpath))

(define cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    '("__GNUC__=6")
    (remove (lambda (s)
	      (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-gcc-cpp-defs)))
   (else
    '())))
(define inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir)
     "/opt/local/include/glib-2.0"
     "/opt/local/lib/glib-2.0/include"
     "/usr/include")
   (get-gcc-inc-dirs)))
(define inc-help
  (cond
   ((string-contains %host-type "darwin")
    '(("__builtin"
       "__builtin_va_list=void*"
       "__attribute__(X)="
       "__extension__="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__has_include(X)=__has_include__(X)"
       )
      ))
   (else
    '(("__builtin"
       "__builtin_va_list=void*" "__attribute__(X)="
       "__inline=" "__inline__="
       "__asm(X)=" "__asm__(X)="
       "__extension__="
       )
      ))))

(define mode 'file)
(define mode 'decl)
(define mode 'code)
(define debug #f)

(define (parse-file file)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs 
		 #:inc-dirs inc-dirs
		 #:inc-help inc-help
		 #:mode mode #:debug debug
		 #:xdef? #t))))

(define (parse-string str)
  ;;(simple-format #t "~S => \n" str)
  (with-input-from-string str
    (lambda ()
      (parse-c99 #:cpp-defs cpp-defs
		 #:inc-dirs inc-dirs 
		 #:inc-help inc-help
		 #:mode mode #:debug debug 
		 #:xdef? #t))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define ppsx (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 pretty-print-c99)

;; The standard says:
;;   For two qualified types to be compatible, both shall have the identically
;;   qualified version of a compatible type; the order of type qualifiers within
;;   a list of specifiers or qualifiers does not affect the specified type.

;;(and=> (parse-file "exam.d/ex14.c") ppsx)
;;(and=> (parse-c99x "(a*b)+c") ppsx)

(define adecl #f)
#;(let* ((code "struct foo { int x, y; } *a, b;\n") (indx 1)
       (code (string-append
	      "typedef int *foo_t;\n"
	      "typedef double hmm_t[3];\n"
	      "int baz(foo_t (*baz)(hmm_t y));\n"
	      ))
       (xcode (string-append
	      "typedef struct { int x; } foo_t;\n"
	      "foo_t *y;\n"
	      ;; expand as
	      ;; 1) struct struct__foo_t *y;
	      ;; 2) void *y;
	      ))
       (indx 2)
       (tree (parse-string code))
       ;;(tree (parse-c99x code))
       ;;(tree (parse-file "zz4.c"))
       
       (udict (c99-trans-unit->udict tree))
       ;;(ddict (udict-enums->ddict udict))
       (udecl (udict-ref udict "baz"))
       ;;(decl (and=> ((sxpath `((decl ,indx))) tree) car))
       (xdecl (expand-typerefs udecl udict))
       )
  ;;(display code)
  (pp99 udecl)
  (display "==\n")
  (pp99 xdecl)
  ;;(ppsx mspec)
  ;;(ppsx (expand-typerefs udecl udict '("foo_t")))
  ;;(ppsx ddict)
  ;;(set! adecl decl)
  #t)

;; ex12.c: illustrates removal of comment prefix, offset-8 => offset-2
;;(let ((tree (parse-file "exam.d/ex12.c"))) (pp99 tree))
      
;;(ugly-print (quote `(abc ,@def)))

(define n1570a ;; sec 6.7.8
  (string-append
   "typedef signed int t;\n"
   "typedef int plain;\n"
   "struct tag {\n"
   "  unsigned t:4;\n"
   "  const t:5;\n"
   "  plain r:5;\n"
   "};\n"))
(define n1570b
  (string-append
   n1570a
   "int foo() {\n"
   "  t f(t (t));\n"
   "  long t;\n"
   "  return 1;\n"
   "};\n"))

;; --- last line ---
(define ffi-cairo-types
  '((struct . "_cairo")
    "cairo_t"
    (struct . "_cairo_surface")
    "cairo_surface_t"
    (struct . "_cairo_device")
    "cairo_device_t"
    (struct . "_cairo_matrix")
    "cairo_matrix_t"
    (struct . "_cairo_pattern")
    "cairo_pattern_t"
    (pointer . "cairo_destroy_func_t")
    (struct . "_cairo_user_data_key")
    "cairo_user_data_key_t"
    (pointer . "cairo_write_func_t")
    (pointer . "cairo_read_func_t")
    (struct . "_cairo_rectangle_int")
    "cairo_rectangle_int_t"
    (struct . "_cairo_rectangle")
    "cairo_rectangle_t"
    (struct . "_cairo_rectangle_list")
    "cairo_rectangle_list_t"
    (struct . "_cairo_scaled_font")
    "cairo_scaled_font_t"
    (struct . "_cairo_font_face")
    "cairo_font_face_t"
    "cairo_glyph_t"
    "cairo_text_cluster_t"
    "cairo_text_extents_t"
    "cairo_font_extents_t"
    (struct . "_cairo_font_options")
    "cairo_font_options_t"
    (pointer . "cairo_user_scaled_font_init_func_t")
    (pointer
      .
      "cairo_user_scaled_font_render_glyph_func_t")
    (pointer
      .
      "cairo_user_scaled_font_text_to_glyphs_func_t")
    (pointer
      .
      "cairo_user_scaled_font_unicode_to_glyph_func_t")
    (union . "_cairo_path_data_t")
    "cairo_path_data_t"
    (struct . "cairo_path")
    "cairo_path_t"
    (pointer . "cairo_surface_observer_callback_t")
    (pointer . "cairo_raster_source_release_func_t")
    (pointer . "cairo_raster_source_snapshot_func_t")
    (pointer . "cairo_raster_source_copy_func_t")
    (pointer . "cairo_raster_source_finish_func_t")
    (struct . "_cairo_region")
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    11 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
    "cairo_region_t")
)


(begin
  (display "1234567890123456789012345678901234567890")
  (display "1234567890123456789012345678901234567890")
  (newline)
  (display "(define types\n")
  (display "  '")
  (ugly-print ffi-cairo-types #:per-line-prefix "   " #:trim-ends #t)
  (display ")\n")
  )
