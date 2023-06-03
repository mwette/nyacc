;;; examples/nyacc/lang/c99/ffi-help.scm

;; Copyright (C) 2016-2023 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;;; TODOs:

;;; Issues:

;;; Code:

(define-module (nyacc lang c99 ffi-next)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    load-include-file
	    fh-cnvt-udecl fh-cnvt-cdecl fh-cnvt-cdecl->str fh-scm-str->scm-exp
	    string-member-proc string-renamer
	    )
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 munge)
  #:use-module (nyacc lang c99 cxeval)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc version)
  #:use-module (nyacc lang sx-util)
  #:use-module ((nyacc util) #:select (ugly-print))
  #:use-module (system foreign)
  #:use-module (sxml fold)
  #:use-module (sxml match)
  #:use-module ((sxml xpath)
		#:renamer (lambda (s) (if (eq? s 'filter) 'node-filter s)))
  #:use-module ((srfi srfi-1) #:select (fold fold-right remove last))
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (system base pmatch)
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:re-export (*nyacc-version*)
  #:version (1 08 1))

(define fh-cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    (remove (lambda (s) (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-sys-cpp-defs)))
   (else (get-sys-cpp-defs))))
    
(define fh-inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir) "/usr/include")
   (get-sys-inc-dirs)))

(define fh-inc-help c99-def-help)

;; DEBUGGING
(set! fh-inc-dirs (cons "." fh-inc-dirs))

;; maybe change to a record-type
(define *BS-version* 1)
(define *options* (make-parameter '()))
(define *debug-parse* (make-parameter #f)) ; parse debug mode
(define *show-incs* (make-parameter #f))   ; show include directories
(define *echo-decls* (make-parameter #f)) ; add echo-decls code for debugging

(define *prefix* (make-parameter "")) ; name prefix (e.g., prefix-syms)
(define *renamer* (make-parameter identity)) ; renamer from ffi-module

(define *mport* (make-parameter #t))	   ; output module port
(define *udict* (make-parameter '()))	   ; udecl dict
(define *ddict* (make-parameter '()))	   ; cpp-def based dict
(define *wrapped* (make-parameter '()))	; wrappers for foo_t and foo_t*
(define *defined* (make-parameter '()))	; type defined

(define *errmsgs* (make-parameter '()))	; list of warnings

(define dev/null
  (let ((port #f))
    (lambda ()
      (unless port (set! port (open-output-file "/dev/null")))
      port)))

(define (sfscm fmt . args)
  (apply simple-format (*mport*) fmt args))
(define* (ppscm tree #:key (per-line-prefix ""))
  (pretty-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define* (upscm tree #:key (per-line-prefix ""))
  (ugly-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define (c99scm tree)
  (pretty-print-c99 tree
		    (*mport*)
		    #:per-line-prefix ";; "))
(define (nlscm) (newline (*mport*)))

(define (sfout fmt . args)
  (apply simple-format #t fmt args))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (nlout) (newline))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args)
  (force-output (current-error-port)))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))

(define (fherr/once fmt . args)
  (let ((errmsgs (*errmsgs*)))
    (cond
     ((member fmt errmsgs)
      (apply throw 'ffi-help-error #f '()))
     (else
      (*errmsgs* (cons fmt errmsgs))
      (apply throw 'ffi-help-error fmt args)))))

;; === utilities

(define (sfstr fmt . args)
  (apply simple-format #f fmt args))

(define (ppstr exp)
  (call-with-output-string
    (lambda (port)
      (pretty-print exp port #:per-line-prefix "  "))))

(define (make-arg-namer)
  (let ((ix 0))
    (lambda ()
      (let ((iv ix))
	(set! ix (1+ ix))
	(simple-format #f "arg~S" iv)))))

;; strings->symbol
(define (strings->symbol . string-list)
  (string->symbol (apply string-append string-list)))

;; '(abc def) => "abc-def"
(define (m-path->name path)
  (string-join (map symbol->string path) "-"))

;; '(abc def) => "abc/def"
(define (m-path->f-path path)
  (string-join (map symbol->string path) "/"))

(define (link-libs)
  (string->symbol (string-append (*prefix*) "-llibs")))

;; @deffn {Procedure} opts->attrs module-opts script-opts
;; The values in @var{script-opts} override @var{module-opts}.  That is,
;; if the value is a list then append, else replace.
;; @end deffn
(define (opts->attrs module-opts script-opts)
  ;; module-opts: inc-dirs pkg-config ...
  ;; script-opts: inc-dirs
  (fold-right
   (lambda (opt seed)
     (cond
      ((assq-ref seed (car opt)) =>
       (lambda (val)
	 (if (pair? val)
	     (acons (car opt) (append (cdr opt) val) seed)
	     (acons (car opt) val seed))))
      (else (cons opt seed))))
   (filter (lambda (pair) (symbol? (car pair))) module-opts)
   script-opts))

(define (opts->mopts opts) ;; module options to pass
  (filter (lambda (pair) (keyword? (car pair))) opts))

;; Run pkg-config
(define (pkg-config name . args)
  (if name
      (let* ((cmdstr (string-append "pkg-config" " "
				    (string-join args) " " name))
	     (port (open-input-pipe cmdstr))
	     (ostr (read-line port))
	     (status (close-pipe port))
	     (items (if (eof-object? ostr) '() (string-split ostr #\space))))
	(unless (zero? status) (fherr "failed: `~A'" cmdstr))
	items)
      '()))

;; use pkg-config to get a list of include dirs
;; (pkg-config-incs "cairo") => ("/opt/local/include/cairo" ...)
(define (pkg-config-incs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-I" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

(define (pkg-config-defs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-D" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

;; TODO:
;; 1) check if --libs provides -L that is not in ld.so dirs.
(define (pkg-config-libs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-lm" s) l) ;; workaround for ubuntu libm issue
      ((string=? "-l" (substring/shared s 0 2))
       (cons (string-append "lib" (substring/shared s 2)) l))
      (else l)))
   '()
   (pkg-config name "--libs")))

(define (resolve-attr-val val)
  (let* ((val (if (procedure? val) (val) val)))
    (cond
     ((eq? #f val) '())
     ((list? val) val)
     ((string? val) (list val))
     (else (throw 'ffi-help-error "value does not resolve to list")))))

(define (rename name)
  ((*renamer*) name))

;; === output scheme module header 

;; === type conversion ==============

(define bs-typemap
  '(("void" . 'void) ("float" . float) ("double" . double)
    ("short" . short) ("short int" . short) ("signed short" . short)
    ("signed short int" . short) ("int" . int) ("signed" . int)
    ("signed int" . int) ("long" . long) ("long int" . long)
    ("signed long" . long) ("signed long int" . long) ("long long" . long)
    ("long long int" . long-long) ("signed long long" . long-long)
    ("signed long long int" . long)
    ("unsigned short int" . unsigned-short) ("unsigned short" . unsigned-short)
    ("unsigned int" . unsigned-int) ("unsigned" . unsigned-int)
    ("unsigned long int" . unsigned-long) ("unsigned long" . unsigned-long)
    ("unsigned long long int" . unsigned-long-long)
    ("unsigned long long" . unsigned-long-long)
    ("intptr_t" . intptr_t) ("uintptr_t" . uintptr_t) ("size_t" . size_t)
    ("ssize_t" . ssize_t) ("ptrdiff_t" . ptrdiff_t)
    ("int8_t" . int8) ("uint8_t" . uint8) 
    ("int16_t" . int16) ("uint16_t" . uint16) 
    ("int32_t" . int32) ("uint32_t" . uint32)
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("float _Complex" . complex64) ("double _Complex" . complex128)
    ;; hacks:
    ("char" . int8) ("signed char" . int8) ("unsigned char" . uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ("_Bool" . int8)))

(define bs-defined (map car bs-typemap))
;; --- output routines ---------------

;; --- structs and unions

;; --- enums

;; === function declarations : signatures for pointer->procedure


;; === function calls : unwrap args, call, wrap return

;; given mdecl for an exec argument give the unwrapper
;;(define (mdecl->fh-unwrapper mdecl)

;;(define (mdecl->fh-wrapper mdecl)

;; given list of name-unwrap pairs generate function arg names
;;(define (gen-exec-arg-names params)

;; This generates the list of arguments to the actual call.
;;(define (gen-exec-call-args params)

;;(define (gen-exec-return-wrapper udecl)

;; data-structures in C
;; define:
;;   (make-type_t args) => bs
;;   (define type_t-desc ...)

;; bs-ref bs
(define ffi-defined '())
(define (mtail->ffi-desc x) #f)

(define (gen-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl (*udict*) ffi-defined))
	 (udecl (udecl-rem-type-qual udecl1))
	 (mdecl (udecl->mdecl udecl1)))
    (mtail->ffi-desc (md-tail mdecl))))

(define (cnvt-fctn name rtail params)
  (let* ((varargs? (and (pair? params) (equal? (last params) '(ellipsis))))
	 (decl-return (gen-decl-return rdecl))
	 (decl-params (gen-decl-params params))
	 (exec-return (gen-exec-return-wrapper rdecl))
	 (exec-params (gen-exec-params params))
	 (sname (string->symbol name))
	 (~name (string->symbol (string-append "~" name)))
	 ;;(call `(,~name ,@(gen-exec-call-args exec-params)))
	 (va-call `(apply ,~name ,@(gen-exec-call-args exec-params)
			  (map cdr ~rest)))
	 (call `((force ,~name) ,@(gen-exec-call-args exec-params))))
    (cond
     (varargs?
      (sfscm ";; to be used with fh-cast\n")
      (ppscm
       `(define (,sname ,@(gen-exec-arg-names exec-params) . ~rest)
	  (let ((,~name (fh-link-proc
			 ,decl-return ,name
			 (append (list ,@decl-params) (map car ~rest))
			 (force ,(link-libs))))
		,@(gen-exec-unwrappers exec-params))
	    ,(if exec-return (list exec-return va-call) va-call)))))
     (else ;; combined ~name and name defines
      (ppscm
       `(define ,sname
	  (let ((,~name
		 (delay (fh-link-proc ,decl-return ,name (list ,@decl-params)
				      (force ,(link-libs))))))
	    (lambda ,(gen-exec-arg-names exec-params)
	      (let ,(gen-exec-unwrappers exec-params)
		,(if exec-return (list exec-return call) call))))))))
    (sfscm "(export ~A)\n" name)))
  
;;(define (cnvt-extern name ms-tail)
;;(define (node-not-typeof? crit)

(define (cnvt-udecl udecl udict wrapped defined)

  ;; but typedefs needs to be included
  ;; ("foo_t" (typedef-for (xxx)))
  (let* (;;(xdecl (expand-typerefs udecl udict defined))
         (mdecl (udecl->mdecl udecl))
         (ident (car mdecl))
         (mattr (md-attr mdecl))
         (mtail (md-tail mdecl)))

    (sferr "ffi-next(cnvt-udecl):\n")
    (pperr mdecl)
    (quit)
    
    (pmatch mtail
      (`((function-returning ,param-list) . ,rtail)
       (cnvt-fctn ident param-list rtail))
      (_
       (sferr "cnvt-decl: missed\n") (pperr mtail)))
    
    ;;(sferr "md-attr: ~S\n" (md-attr mdecl))
    (quit)
    
    ))

(display "ffi-next/TODO: add undef for c99; maybe FOO=! or !FOO vs FOO=1")

;; === enums and #defined => lookup

;; given keeper-def names and cpp defs (c-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc mod-def-names udict ddict ext-mods)

  (define typerefs (udict->typedef-names udict))

  (define (eval-code-string code)
    (let* ((tree (with-error-to-port (dev/null)
		   (lambda () (parse-c99x code typerefs))))
	   (value (and tree (eval-c99-cx tree udict ddict))))
      value))

  ;; @var{mod-dict} is list of CPP defs and enum key/val pairs. It is
  ;; possible for an enum symbol to be used as a macro function so we
  ;; need to first check for integer before trying expand-cpp-macro-ref.
  (sfscm "\n;; access to enum symbols and #define'd constants:\n")
  (let ((st-name (string->symbol (string-append (*prefix*) "-symbol-tab")))
	(sv-name (string->symbol (string-append (*prefix*) "-symbol-val")))
	(defs
	  (fold
	   (lambda (name seed)
	     (let* ((repl (expand-cpp-name name ddict '()))
		    (val (and (string? repl) (eval-code-string repl))))
	       (if val
		   (acons (string->symbol name) val seed)
		   ;;(acons (string->symbol name) #f seed))))
		   seed)))
	   '() mod-def-names))
	(ext-ftns			; lookup in use-ffi-modules
	 (map
	  (lambda (mod)
	    (list (string->symbol
		   (string-append (m-path->name mod) "-symbol-val")) 'k))
	  ext-mods)))
    (ppscm `(define ,st-name '(,@defs)))
    (sfscm "\n")
    (ppscm `(define ,sv-name (lambda (k) (or (assq-ref ,st-name k) ,@ext-ftns))))
    (sfscm "(export ~A)\n" sv-name)
    ;;
    (nlscm)
    (ppscm
     `(define (unwrap-enum obj)
	(cond
	 ((number? obj) obj)
	 ((symbol? obj) (,sv-name obj))
	 ((fh-object? obj) (struct-ref obj 0)) ;; ???
	 (else (error "type mismatch")))))))

;; === Parsing the C header(s)

;; @deffn parse-code code [attrs]
;; Parse @var{code}, a Scheme string, using cpp-defs and inc-dirs from
;; @var{attrs}.
;; This procedure is used by @code{parse-includes}.
;; @end deffn
(define* (parse-code code #:optional (attrs '()))
  (let* ((cpp-defs (resolve-attr-val (assq-ref attrs 'cpp-defs)))
	 (inc-dirs (resolve-attr-val (assq-ref attrs 'inc-dirs)))
	 (inc-help (resolve-attr-val (assq-ref attrs 'inc-help)))
	 ;;
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (cpp-defs (append (pkg-config-defs pkg-config) cpp-defs))
	 (inc-dirs (append (pkg-config-incs pkg-config) inc-dirs))
	 ;;
	 (cpp-defs (append cpp-defs fh-cpp-defs))
	 (inc-dirs (append inc-dirs fh-inc-dirs))
	 (inc-help (append inc-help fh-inc-help)))
    (or (with-input-from-string code
	  (lambda ()
	    (call-with-values
		(lambda ()
		  (parse-c99 #:cpp-defs cpp-defs
			     #:inc-dirs inc-dirs
			     #:inc-help inc-help
			     #:mode 'decl
			     #:xdef? #t	; expand CPP-defines (dev-1.02)
			     #:return-defs #t
			     #:show-incs (*show-incs*)
			     #:debug (*debug-parse*)))
	      (lambda (tree defs)
		(*ddict* defs) tree))))
	(fherr "parse failed"))))

;; @deffn parse-includes attrs
;; This routine generates a top-level source string-file with all the includes,
;; parses it, and then merges one level down of includes into the top level,
;; as if the bodies of the incudes had been combined into one file.
;; @end deffn
(define parse-includes
  (let* ((p (node-join
	     (select-kids (node-typeof? 'cpp-stmt))
	     (select-kids (node-typeof? 'include))
	     (select-kids (node-typeof? 'trans-unit))))
	 (merge-inc-bodies
	  (lambda (t) (cons 'trans-unit (apply append (map cdr (p t)))))))
    (lambda (attrs)
      (let* ((inc-files (resolve-attr-val (assq-ref attrs 'include)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files) "")))
	(and=> (parse-code prog attrs) merge-inc-bodies)))))

;; === main converter ==================

;; => (values wrapped defined)
(define* (process-decls decls udict
			#:optional (wrapped '()) (defined '())
			#:key (declf (lambda (k) #t)))
  (fold-values			  ; from (sxml fold)
   (lambda (name wrapped defined) ; name: "foo_t" or (enum . "foo")
     (catch 'ffi-help-error
       (lambda ()
	 (cond
	  ((and ;; Process the declaration if all conditions met:
	    (declf name)		  ; 1) user wants it
	    (not (member name defined))	  ; 2) not already defined
	    (not (if (pair? name)	  ; 3) not anonymous 
		     (string=? "*anon*" (cdr name))
		     (string=? "" name))))
	   (let ((udecl (udict-ref udict name)))
	     (nlscm) (c99scm udecl)
	     (if (*echo-decls*)
		 (sfscm "(if echo-decls (display \"~A\\n\"))\n" name))
	     (cnvt-udecl udecl udict wrapped defined)))
	  (else (values wrapped defined))))
       ;; exception handler:
       (lambda (key fmt . args)
	 (if fmt (apply simple-format (current-error-port)
			(string-append "ffi-help: " fmt "\n") args))
	 (sfscm ";; ... failed.\n")
	 (values wrapped defined))))
   decls wrapped defined))

;; "abc,def" => '(abc def)
(define (comma-split str)
  (map string->symbol (string-split str #\,)))

;; process define-ffi-module expression
;; was intro-ffi
(define (expand-ffi-module-spec path module-options)
  (let* ((script-options (*options*))
	 (dbugl (or (and=> (assq-ref script-options 'debug) comma-split) '()))
	 (attrs (opts->attrs module-options script-options))
	 (incf (or (assq-ref attrs 'inc-filter) #f))
	 (declf (or (assq-ref attrs 'decl-filter) identity))
	 (renamer (or (assq-ref attrs 'renamer) identity))
	 ;;
	 (tree (begin
		 (if (memq 'parse dbugl) (*debug-parse* #t))
		 (and (assq-ref script-options 'show-incs) (*show-incs* #t))
		 (cond
		  ((assq-ref attrs 'include) (parse-includes attrs))
		  ((assq-ref attrs 'api-code) =>
		   (lambda (code) (parse-code code attrs)))
		  (else (fherr "expecing #:include or #:api-code")))))
	 (udict (c99-trans-unit->udict/deep tree))
	 (udecls (c99-trans-unit->udict tree #:inc-filter incf))
	 (ffi-decls (map car udecls))	; just the names, get decls from udict

	 ;; the list of typedefs we will generate (later):
	 (ffimod-defined #f)
	 ;; ext modules [from #:use-modules (ffi cairo)]
	 (ext-mods
	  (fold-right
	   (lambda (opt seed)
	     (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	   '() module-options))
	 (ext-defd			
	  (fold
	   (lambda (upath seed)
	     (unless (resolve-module upath)
	       (fherr "module not defined: ~S" upath))
	     (let* ((modul (resolve-module upath #:ensure #f))
		    (pname (m-path->name upath))
		    (vname (string->symbol (string-append pname "-types")))
		    (var (module-ref modul vname)))
	       (append var seed)))
	   '() ext-mods)))

    ;; set globals
    (*prefix* (m-path->name path))
    (*udict* udict)
    (*ddict* (udict-enums->ddict udict (*ddict*)))
    (*renamer* renamer)
    (if (memq 'echo-decls dbugl) (*echo-decls* #t))

    ;; file and module header
    ;;(ffimod-header path module-options)

    ;; Convert and output foreign declarations.
    (call-with-values
	(lambda ()
	  ;; We need to have externs in wrapped because function param
	  ;; type have wrapped types preserved (e.g., enums).
	  ;; swap of udecls with udict failed on glib g???
	  (process-decls ffi-decls udecls ;; udict <= swap failed 01 Dec 2018
			 ;; wrapped and defined:
			 ext-defd (append bs-defined ext-defd)
			 ;; declaration filter
			 #:declf declf))
      (lambda (wrapped defined)
	;; Set ffimod-defined for including, but removed built-in types.
	(let* ((bity (car bs-defined))	; first built-in type
	       (defd (let loop ((res '()) (defs defined))
		       (if (eq? (car defs) bity) res
			   (loop (cons (car defs) res) (cdr defs))))))
	  (set! ffimod-defined defd))))
    
    ;; output global constants (from enum and #define)
    (let* ((modd (c99-trans-unit->ddict tree #:inc-filter incf #:skip-fdefs #t))
	   (modd (udict-enums->ddict udecls modd))
	   (xtra (or (assq-ref module-options 'def-keepers) '()))
	   (mod-def-names (fold (lambda (p s) (cons (car p) s)) xtra modd)))
      (gen-lookup-proc mod-def-names udict (*ddict*) ext-mods))

    ;; output list of defined types
    (sfscm "\n(define ~A-types\n  '" (m-path->name path))
    (ugly-print ffimod-defined (*mport*) #:per-line-prefix "   " #:trim-ends #t)
    (sfscm ")\n(export ~A-types)\n" (m-path->name path))

    ;; Return the output port so the compiler can output remaining code.
    #t))


;; === test compiler ================

(define fh-llibs (delay '()))

;; @deffn {Procedure} C-decl->scm code-string => sexp
;; Generate a symbolic expression that evals to a Guile procedure.
;; @example
;; (define fmod-exp (C-fun-decl->proc "double dmod(double, double);"))
;; (define fmod (eval fmod-exp (current-module)))
;; (fmod 2.3 0.5)
;; @end example
;; @end deffn
(define* (C-decl->scm code #:key expand)
  (let ((tree (with-input-from-string code parse-c99)))
    (and tree
	 (let* ((udict (c99-trans-unit->udict tree))
		(udecl (cdr (last udict)))
		(udecl (if expand (expand-typerefs udecl udict) udecl))
		(str-decl (fh-cnvt-udecl udecl udict)) 
		(scm-decl (with-input-from-string str-decl read))
		(scm-value (sx-ref scm-decl 2)))
	   scm-value))))
(define C-fun-decl->scm C-decl->scm)

(define-syntax-rule (C-decl c-code-string)
  (eval (C-decl->scm c-code-string) (current-module)))

(define* (fh-cnvt-udecl udecl udict #:key (prefix "fh"))
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* prefix)
		 (*mport* (open-output-string)) (*udict* udict))
    (cnvt-udecl udecl udict '() '())
    (let ((res (get-output-string (*mport*))))
      (close (*mport*))
      res)))

;; convert string-body of Scheme code to a Scheme expression
;; @example
;; (fh-scm-str->scm-exp "(define a 1)") => '(begin (define a 1))
;; @end example
(define (fh-scm-str->scm-exp str)
  (call-with-input-string str
    (lambda (iport)
      (cons 'begin
	    (let loop ((exp (read iport)))
	      (if (eof-object? exp) '()
		  (cons exp (loop (read iport)))))))))

;; Convert declaration with @var{name} in string-body of C @var{code}
;; to string-body of Scheme code.
;; @example
;; (fh-cnvt-cdecl "sqrt" "double sqrt(double x);") =>
;;   "(define ~sqrt ...)\n (define (sqrt x) ...)"
;; @end example
(define* (fh-cnvt-cdecl->str name code #:key (prefix "fh"))
  (let* ((tree (with-input-from-string code parse-c99))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict name)))
    (fh-cnvt-udecl udecl udict)))

;; like above but then convert to Scheme expression
(define* (fh-cnvt-cdecl name code #:key (prefix "fh"))
  (and=> (fh-cnvt-cdecl->str name code #:prefix prefix) fh-scm-str->scm-exp))
			     
;; === repl compiler ================

;; @deffn {Procedure} load-include-file filename [#pkg-config pkg]
;; This is the functionality that Ludo was asking for: to be at guile
;; prompt and be able to issue
;; @example
;; (use-modules (nyacc lang c99 ffi-help))
;; (load-include-file "cairo.h" #:pkg-config "cairo")
;; @end example
;; @end deffn
;; + Right now the only way would be to generate a file and eval it, because
;;   our code generates strings and not lists.
;; + and=> with-output-to-string  eval
;; + first need to cut up intro-ffi
;; options:
;;   api-code cpp-defs decl-filter
;;   inc-dirs inc-filter inc-help include
;;   library pkg-config renamer
(define* (load-include-file filename
			    #:key pkg-config)
  "- Procedure: load-include-file filename [#:pkg-config pkg]
     This is the functionality that Ludo was asking for: to be at guile
     prompt and be able to issue
          (use-modules (nyacc lang c99 ffi-help))
          (load-include-file \"cairo.h\" #:pkg-config \"cairo\")"
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '())
		 (*prefix* "") (*mport* #t) (*udict* '()))
    (let* ((attrs (acons 'include (list filename) '()))
	   (attrs (if pkg-config (acons 'pkg-config pkg-config attrs) attrs))
	   (tree (parse-includes attrs))
	   (udict (c99-trans-unit->udict/deep tree))
	   (udecls (c99-trans-unit->udict tree))
	   (decls (map car udecls))
	   (scmport (mkstemp! (string-copy ",_FH_XXXXXX")))
	   (scmfile (port-filename scmport))
	   (compile-file (@@ (system base compile) compile-file)))
      (*prefix* (symbol->string (gensym "fh-")))
      (*mport* scmport)
      (*udict* udict)
      (sfscm "(use-modules (ffi ffi-help-rt))\n")
      (sfscm "(use-modules ((system foreign) #:prefix ffi:))\n")
      (sfscm "(use-modules (bytestructures guile))\n")
      (ppscm `(define ,(link-libs)
		(list ,@(map
			 (lambda (l) `(dynamic-link ,l))
			 (pkg-config-libs pkg-config)))))
      (process-decls decls udict '() bs-defined)
      (close (*mport*))
      (simple-format #t "wrote ~S; compile and load: ...\n" scmfile)
      (load-compiled (compile-file scmfile #:opts '()))
      (if #f #f))))


;; === file compiler ================

(use-modules (system base language))
(use-modules (ice-9 pretty-print))

;; Return #t when ffi-file has an mtime greater than that of scm-file
(define (more-recent? ffi-file scm-file)
  ;; copied from ice-9/boot-9.scm
  (let ((stat1 (stat ffi-file)) (stat2 (stat scm-file)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
	(and (= (stat:mtime stat1) (stat:mtime stat2))
	     (>= (stat:mtimensec stat1)
		 (stat:mtimensec stat2))))))

;; given modules spec, determine if any ffi-module dependencies are
;; outdated
(define (check-deps module-options)
  (let ((ext-modz ;; use filter?
	 (fold-right
	  (lambda (opt seed)
	    (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	  '() module-options))
	(ext-mods (map cdr (filter
			    (lambda (opt) (eq? (car opt) 'use-ffi-module))
			    module-options))))
    (for-each
     (lambda (fmod)
       (let* ((base (string-join (map symbol->string fmod) "/"))
	      (xffi (string-append base ".ffi"))
	      (xscm (string-append base ".scm")))
	 (when (not (access? xscm R_OK))
	   (fherr "compiled dependent ~S not found" fmod))
	 (when (more-recent? xffi xscm)
	   (fherr "dependent ~S needs recompile" xffi)
	   (sleep 2))))
     ext-mods)))

(define-syntax parse-module-options
  (lambda (x)
    (define (key->sym stx)
      (datum->syntax stx (keyword->symbol (syntax->datum stx))))
    (define (ffimod-option? key)
      (and (keyword? key)
	   (member key '(#:api-code
			 #:cpp-defs #:decl-filter #:inc-dirs #:inc-filter
			 #:inc-help #:include #:library #:pkg-config #:renamer
			 #:use-ffi-module #:def-keepers))))
    (define (module-option? key) (keyword? key))

    (syntax-case x ()
      ((_ key val option ...)
       (eq? (syntax->datum #'key) #:use-ffi-module)
       #`(cons
	  (cons (quote #,(key->sym #'key)) (quote val))
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (ffimod-option? (syntax->datum #'key))
       #`(cons
	  (cons (quote #,(key->sym #'key)) val)
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (module-option? (syntax->datum #'key))
       #`(cons
	  (cons key (quote val))
	  (parse-module-options option ...)))
      
      ((_ key val option ...)
       #'(syntax-error "compile-ffi: expecting keyword"))
      
      ((_) #''()))))

(define-syntax-rule (define-ffi-module path-list attr ...)
  (let ((module-options (parse-module-options attr ...)))
    (check-deps module-options)
    (expand-ffi-module-spec (quote path-list) module-options)))

(define (string-member-proc . args)
  (lambda (s) (member s args)))

;; to convert symbol-based #:renamer to string-based
(define (string-renamer proc)
  (lambda (s) (string->symbol (proc (symbol->string s)))))

(define call-with-output-file/atomic
  (@@ (system base compile) call-with-output-file/atomic))

;; @deffn {Procedure} compile-ffi-file file [options]
;; This procedure will 
;; @end deffn
(define* (compile-ffi-file filename #:optional (options '()))
  (parameterize ((*options* options) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* "")
		 (*mport* #t) (*udict* '()) (*ddict* '()))
    (if (not (access? filename R_OK))
	(fherr "ERROR: not found: ~S" filename))
    (call-with-input-file filename
      (lambda (iport)
	(let ((output (or (assq-ref 'output options)
			  (string-append (dirname filename) "/"
					 (basename filename ".ffi") ".scm"))))
	  (call-with-output-file/atomic output
	    (lambda (oport)
	      (*mport* oport)
	      (let ((env (make-fresh-user-module)))
		;;(eval '(use-modules (nyacc lang c99 ffi-help)) env)
		(eval '(use-modules (nyacc lang c99 ffi-next)) env)
		(let loop ((exp (read iport)))
		  (cond
		   ((eof-object? exp)
		    (display "\n;; --- last line ---\n" oport)
		    output)
		   ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
		    (eval exp env)
		    (loop (read iport)))
		   (else
		    (newline oport)
		    (pretty-print exp oport)
		    (loop (read iport)))))))))))))

;; --- last line ---
