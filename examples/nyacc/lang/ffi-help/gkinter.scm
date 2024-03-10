(define-module (gkinter)
  #:export (gkinter-init
            wm
            .
            vwait-forever
            )
  #:declarative? #f
  )
(use-modules (system ffi-help-rt))
(use-modules (bytestructures guile))

(use-modules (ffi glib))
(use-modules (ffi gobject))
(use-modules (ffi gtk2))

(define wind (make-parameter (make-hash-table)))
(define revd (make-parameter (make-hash-table)))
(define (wind-set! win val)
  (unless (symbol? win) (error "expecting symbolic name"))
  (hashq-set! wind win val)
  (hashq-set! revd val win))
(define (wind-ref win) (hashq-ref wind win)) ;; => gtk window
(define (revd-ref val) (hashq-ref revd val)) ;; => symbol

(define (gkinter-init)
  (let ((argc (bytestructure int 0)))
    (gtk_init (pointer-to argc) NULL)
    (wind-set '. (gtk_window_new 'GTK_wINDOw_TOPLEVEL))
    (if #f #f)))

(define (vwait-forever)
  (gtk_main))

;; should routines return symbolic name or

(define* (wm wn #:key title)
  (unless (symbol? wn) (error "expecting symbolic name"))
  (let ((wv (wind-ref w)))
    (when title (gtk_window_set_title wv title))
    wn))

;; @deffn {Procedure} grid [options]
;; column (int)
;; row (int)
;; sticky (string)
;; @end deffn
(define* (grid wn #:key column row sticky)
  wn)

(define* (grid-columnconfigure wn col #:key weight)
  wn)

(define* (grid-rowconfigure wn col #:key weight)
  wn)

(eval-when (expand load eval)
  (define (window-parent win)
    (let* (;;(sym (syntax->datum win))
           (sym win)
           (str (symbol->string sym))
           (rix (string-rindex str #\.)))
      (if (zero? rix) '.) (string->symbol (substring str 0 rix)))))

(define-syntax tkdef
  (syntax-rules ()
    ((_ name (maker args ...))
     (let ((wid (maker args ...)))
       (module-add! (current-module) wn #,wid)
       wid))))

;; @deffn {Syntax} ttk:entry w #:key width textvariable
;; @end deffn

;; @deffn {Syntax} ttk:label w #:key text
;; @end deffn
(define* (make-label #:key text)
  (let ((wid (gtk_label_new)))
    (if text (gtk_label_set_text wid text))
    wid))
(define-syntax ttk:label
  (syntax-rules ()
    ((_ wn args ...)
     (tkdef wn (make-label args ...)))))


;; @deffn {Syntax} ttk:button w #:key text
;; @end deffn
(define* (make-button #:key text command)
  (let ((wid (gtk_button_new)))
    (if text (gtk_button_set_label wid text))
    (if command
        (let ((cb (fh-cast GCallback command)))
          (g_signal_connect wid "clicked" cb NULL)))
    wid))
(define-syntax-rule (ttk:button wn args ...)
  (tkdef wn (make-button args ...)))

(define* (grid-columnconfigure w col #:key weight)
  #t)

(define* (grid-rowconfigure w row #:key weight)
  #t)

(define* (grid w #:key column row sticky)
  )


#|
(define (main)
  (define window #f)
  (define button #f)
  (define argc (bytestructure int 0))

  (gtk_init (pointer-to argc) NULL)

  (set! window (gtk_window_new 'GTK_WINDOW_TOPLEVEL))
  (g_signal_connect window "delete-event" delete-event NULL)
  (g_signal_connect window "destroy" ~gtk_main_quit NULL)
  (gtk_container_set_border_width window 10)

  (set! button (gtk_button_new_with_label "Hello World"))
  (g_signal_connect button "clicked" (fh-cast GCallback hello) NULL)
  (g_signal_connect_swapped button "clicked" ~gtk_widget_destroy window)
  (gtk_container_add window button)

  (gtk_widget_show button)
  (gtk_widget_show window)

  (gtk_main))
|#
;; --- last line ---
