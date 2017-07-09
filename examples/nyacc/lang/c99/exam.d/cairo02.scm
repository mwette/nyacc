;; exam.d/cairo02.scm

(define (sf fmt . args)
  (apply simple-format #t fmt args))

(use-modules (cairo cairo))
(use-modules (ffi-help-rt))
(use-modules (system foreign)) ;; string->pointer

(define srf (cairo_svg_surface_create (string->pointer "abc.svg") 200.0 200.0))
(define cr (cairo_create srf))


;; --- use of user data ----------------

;; typedef struct _cairo_user_data_key {
;;   int unused;
;; } cairo_user_data_key_t;
;;
;; typedef void (*cairo_destroy_func_t)(void *data);
;;
;; cairo_status_t cairo_set_user_data(cairo_t *cr, const cairo_user_data_key_t
;;      *key, void *user_data, cairo_destroy_func_t destroy);

;; make a key
(define k1 (make-cairo_user_data_key_t))

;; make some data
(define v1 '((abc . 123) (def . 456)))

;; since k1,v1 are global they don't need guardian, actually
;; (define g (make-guardian))

(define (d1 data)
  (sf "d1 called with ~S\n" (pointer->scm data))
  #f)
	 
(cairo_set_user_data cr (pointer-to k1) (scm->pointer v1) d1)

;; -----------------------------------

(cairo_move_to cr 10.0 10.0)
(cairo_line_to cr 190.0 10.0)
(cairo_line_to cr 190.0 190.0)
(cairo_line_to cr 10.0 190.0)
(cairo_line_to cr 10.0 10.0)
(cairo_stroke cr)


(cairo_destroy cr)
(cairo_surface_destroy srf)

;; --- last line ---


