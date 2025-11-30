;; gobj-01.scm

;; Copyright (C) 2025 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) any
;; later version.
;;
;; This library is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
;; details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;; This demo shows that one can create new GObject based classes in Guile.

(use-modules (srfi srfi-9))
(use-modules (rnrs bytevectors))
(use-modules (nyacc foreign cdata))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (ffi gobject))

(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)


(define-record-type <demo-stuff>
  (make-demo-stuff x y z)
  demo-stuff?
  (x demo-x set-demo-x!)
  (y demo-y set-demo-y!)
  (z demo-z set-demo-z!))


(define show_ftn_t
  (cfunction
   (lambda (proc) (ffi:procedure->pointer ffi:void proc (list '*)))
   (lambda (fptr) (ffi:pointer->procedure ffi:void fptr (list '*)))))

(define DemoClass
  (cstruct (list `(parent_class ,GObjectClass)
                 `(show ,(cpointer show_ftn_t)))))
(define DemoClass*
  (cpointer DemoClass))

(define DemoObject
  (cstruct (list `(object ,GObject)
                 `(color ,(cbase 'int))
                 `(proxy ,(cpointer 'void)))))

(define DemoObject*
  (name-ctype 'DemoObject* (cpointer DemoObject)))


(define demo-set-property
  (lambda (object prop_id value pspec)
    (values)))

(define demo-get-property
  (lambda (object prop_id value pspec)
    (values)))

(define demo-dispose
  (lambda (object)
    (values)))

(define demo-finalize
  (lambda (object)
    (values)))

(define demo-show
  (lambda (object) ;; expect type DemoObject*
    (let* ((proxy (cdata-ref object 'proxy))
           (stuff (ffi:pointer->scm proxy)))
      (display stuff) (newline)
      (values))))


(define demo-class-init
  (make-cdata
   GClassInitFunc
   (lambda (class data)
     (sf "class-init: class=~s data=~s\n" class data)
     (let ((object_class (ccast GObjectClass* class))
           (demo_class (ccast DemoClass* class)))
       (cdata-set! object_class demo-get-property '* 'get_property)
       (cdata-set! object_class demo-set-property '* 'set_property)
       (cdata-set! object_class demo-dispose '* 'dispose)
       (cdata-set! object_class demo-finalize '* 'finalize)
       (cdata-set! demo_class demo-show '* 'show)
       (values)))))

(define demo-init
  (make-cdata
   GInstanceInitFunc
   (lambda (instance g_class)
     (sf "init: instance=~s g_class~s\n" instance g_class)
     (let* ((object (ccast GObject* instance))
            (demo (ccast DemoObject* instance))
            (stuff (make-demo-stuff 1 2 3))
            (proxy (ffi:scm->pointer stuff)))
       (cdata-set! demo 0 '* 'color)
       (sf "proxy=~s\n" (cdata-ref demo '* 'proxy))
       (cdata-set! demo proxy '* 'proxy)
       ))))

(define demo-get-type
  (let ((type-id 0))
    (lambda ()
      (if (zero? type-id)
          (let* ((p-type (g_type_from_name "GObject"))
                 (c-size (ctype-size DemoClass))
                 (i-size (ctype-size DemoObject))
                 (flags 0))
            (set! type-id (g_type_register_static_simple
                           p-type "Demo"
                           c-size demo-class-init
                           i-size demo-init
                           flags))))
      type-id)))

(define (runit)
  (g_type_init)
  (let* ((gobj (g_object_new (demo-get-type) NULL))
         (demo (ccast DemoObject* gobj))
         (proxy (cdata*-ref demo 'proxy))
         (stuff (ffi:pointer->scm proxy))
         )
    (sf "gobj : ~s\n" gobj)
    (sf "demo : ~s\n" demo)
    (sf "proxy: ~s\n" proxy)
    (sf "stuff: ~s\n" stuff)
    (values)))

(runit)

;; --- last line ---
