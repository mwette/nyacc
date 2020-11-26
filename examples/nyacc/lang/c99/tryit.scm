;; examples/nyacc/lang/c99/tryit.scm

;; Copyright (C) 2020 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(add-to-load-path (getcwd))

(use-modules (nyacc lang c99 parser))
(use-modules (nyacc lang c99 cxeval))
(use-modules (nyacc lang c99 pprint))
(use-modules (nyacc lang c99 munge))
(use-modules (nyacc lang c99 munge-base))
(use-modules (nyacc lang c99 cpp))
(use-modules (nyacc lang c99 util))
(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules (nyacc lex))
(use-modules (nyacc util))
(use-modules (sxml fold))
(use-modules (sxml xpath))
(use-modules (ice-9 pretty-print))

(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)
(define ppin (lambda (sx) (pretty-print sx #:per-line-prefix "  ")))
(define pp99 (lambda (sx) (pretty-print-c99 sx #:per-line-prefix "  ")))
(define cep current-error-port)
(define (sferr fmt . args) (apply simple-format (cep) fmt args))
(define (pperr sx) (pretty-print sx (cep) #:per-line-prefix "  "))
(define (ppe99 sx) (pretty-print-c99 sx (cep) #:per-line-prefix "  "))

(define *cpp-defs* (get-gcc-cpp-defs))
(define *inc-dirs* (get-gcc-inc-dirs))
(define *inc-help* c99-def-help)

(define *mode* (make-parameter 'code))
(define *debug* (make-parameter #f))
(define *xdef?* (lambda (name mode) (memq mode '(code decl))))

(define* (parse-file file #:key cpp-defs inc-dirs mode debug)
  (with-input-from-file file
    (lambda ()
      (parse-c99 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define* (parse-string str
		       #:optional (tyns '())
		       #:key cpp-defs inc-dirs mode debug)
  (with-input-from-string str
    (lambda ()
      (parse-c99 tyns
		 #:cpp-defs (or cpp-defs *cpp-defs*)
		 #:inc-dirs (or inc-dirs *inc-dirs*)
		 #:inc-help *inc-help*
 		 #:mode (or mode (*mode*))
		 #:debug (or debug (*debug*))
		 #:show-incs #f
		 #:xdef? *xdef?*))))

(define (parse-string-list . str-l)
  (parse-string (apply string-append str-l)))

(use-modules (nyacc lang arch-info))
(use-modules ((system foreign) #:prefix ffi:))
(use-modules (ice-9 match))

(define (fold p s l)
  (let loop ((s s) (l l))
    (if (null? l) s (loop (p (car l) s) (cdr l)))))

(define-syntax-rule (pass-if mesg expr)
  (begin
    (display mesg)
    (newline)
    (pp expr)))

(define (remove-comments tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (sx-match tree
      ((comment ,text) seed)
      (,_ (if (pair? seed) (cons (reverse kseed) seed) (reverse kseed)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

(define (fix-tree tree)
  (define (fD seed tree) '())
  (define (fU seed kseed tree)
    (case (car tree)
      ((include)
       (let ((t (reverse (cdr kseed))))
	 (if (pair? seed) (cons t seed) t)))
      ((comment) seed)
      (else
       (let ((t (reverse kseed)))
	 (if (pair? seed) (cons t seed) t)))))
  (define (fH seed node) (cons node seed))
  (foldts fD fU fH '() tree))

(when #f
  (let* ((code "int foo = sizeof(int(*)());")
	 (tree (or (parse-string code) (error "parse failed")))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict "foo"))
	 (expr (sx-ref* udecl 2 2 1))
	 )
    (sf "declaration::\n")
    (pp udecl)
    (sf "extract initializer expression:\n")
    (pp expr)
    (sf "evaluate:\n")
    (sf "x = ~S\n" (eval-c99-cx expr))))

(when #f
  (let* ((code "int intx;\n")
	 (tree (or (parse-string code) (error "parse failed")))
	 )
    (sf "~A\n" code)
    (ppin tree)
    (pp99 tree)
    ))

(when #f
  (let* ((code "*(x->y->z)")
	 (tree (parse-c99x code))
 	 )
    (pp code)
    (pp tree)
    (pp99 tree)
    (newline)
    ))

(define (sxpath->proc path)
  (match path
    (()
     '())
    (`(// . ,rest)
     `(node-join (node-or
		  (node-self (node-typeof? '*any*))
		  (node-closure (node-typeof? '*any*)))
		 ,(sxpath->proc rest)))
    (`((equal? ,x) . ,rest)
     `(node-join (select-kids (node-equal? ,x))
		 ,(sxpath->proc rest)))
    (`((eq? ,x) . ,rest)
     `(node-join (select-kids (node-eq? ,x))
		 ,(sxpath->proc rest)))
    (((? symbol? symb) . rest)
     `(node-join (select-kids (node-typeof? (quote ,symb)))
		 ,(sxpath->proc rest)))
    #;(((? procedure? proc) . rest)
    `(node-join ,proc
    ,(sxpath->proc rest)))
    (((? number? numb) . rest)
     `(node-join (node-pos ,numb)
		 ,(sxpath->proc rest)))
    (_
     (error "don't grok" path))))

(when #f
  (let* (;;(p1 (sxpath->proc '(// struct-def)))
	 #;(p1 '(node-join
	       (node-or
		(node-self (node-typeof? '*any*))
		(node-closure (node-typeof? '*any*)))
	       (node-join
		(select-kids (node-typeof? 'struct-def))
		node-join)))
	 ;;(f1 (eval p1 (current-module)))
	 (p1 '(node-join
		(node-or
		 (node-self (node-typeof? '*any*))
		 (node-closure (node-typeof? '*any*)))
		(node-join
		 (select-kids (node-typeof? 'struct-def))
		 node-join)))
	 (f1 (node-join
		(node-or
		 (node-self (node-typeof? '*any*))
		 (node-closure (node-typeof? '*any*)))
		(node-join
		 (select-kids (node-typeof? 'struct-def))
		 )))
	 (f2 (node-join
		(node-or
		 (node-self (node-typeof? '*any*))
		 (node-closure (node-typeof? '*any*)))
		(node-join
		 (select-kids (node-typeof? 'struct-def))
		 (node-or
		  (node-join
		   (select-kids (node-typeof? 'ident))
		   (select-kids (node-typeof? 'field-list)))
		  (node-join
		   (select-kids (node-typeof? 'field-list))))
		 )
		))
	 (t1 `(udecl (decl-spec-list
		      (stor-spec (typedef))
		      (type-spec
		       (struct-def
			(ident "foo")
			(field-list
			 (comp-decl
			  (decl-spec-list (type-spec (fixed-type "int")))
			  (comp-declr-list (comp-declr (ident "comp")))))))
		      (init-declr (ident "foo_t")))))
	 )
    ;;(pp p1)
    ;;(pp f1)
    ;;(pp t1)
    ;;(pp (f1 t1))
    (pp (f2 t1))
    #f))

(define sel-struct
  (let ((sel (node-join
	      (node-or
	       (node-self (node-typeof? '*any*))
	       (node-closure (node-typeof? '*any*)))
	      (node-join
	       (select-kids (node-typeof? 'struct-def))))))
    (lambda (node)
      (let ((res (sel node)))
	(and (pair? res) (car res))))))
  
(define sel-fields
  (let ((sel (node-or
	      (node-join
	       (select-kids (node-typeof? 'ident))
	       (select-kids (node-typeof? 'field-list)))
	      (node-join
	       (select-kids (node-typeof? 'field-list))))))
  (lambda (node)
    (let ((res (sel (list node))))
      (and (pair? res) (car res))))))

(define sel-type-specs
  (let ((sel (node-join
	      (select-kids (node-typeof? 'comp-decl))
	      (select-kids (node-typeof? 'decl-spec-list))
	      (select-kids (node-typeof? 'type-spec)))))
    (lambda (nodeset)
      (sel nodeset))))

(define sel-declrs
  (let ((sel (node-join
	      (select-kids (node-typeof? 'comp-decl))
	      (select-kids (node-typeof? 'comp-declr-list))
	      (select-kids (node-typeof? 'comp-declr)))))
    (lambda (nodeset)
      (sel nodeset))))

;; given struct-def and elt, return elt's type-spec
(define (probe1 struct path)
  (let* ((field-list (sel-fields struct))
	 )
    (pp field-list)
    #f))

(when #f
  (let* ((code
	  (string-append
	   "void foo() { __asm__ goto (\"mov r0,r1\" : "
	   ": [mcu] \"I\" (123), [ssr] \"X\" (456) "
	   " : \"foo\", \"bar\" : error ); }"))
	 (tree (parse-string code #:mode 'decl))
 	 )
    (pp tree)
    #t))

(when #f
  (let* ((code
	  (string-append
	   "#define sei() __asm__ __volatile__ (\"sei\" ::: \"memory\")\n"
	   "int foo() { sei(); }\n"
	   ))
	 (tree (parse-string code #:mode 'code))
 	 )
    (pp tree)
    #t))

(when #t
  (let* ((code
	  (string-append
	   "#define ISR(vector, ...) void vector (__VA_ARGS__) \n"
	   "ISR(__vector__12__) { int x; }\n"))
	 (tree (parse-string code #:mode 'code)))
    (pp tree)
    (pp99 tree)
    #t))

(if #f (pp (parse-file "/tmp/exam.d/ex08.c" #:mode 'code)))

(when #f
  (let* ((code 
	  "if (((Rd & 0x08) && (Rr & 0x08)) ||
              ((Rr & 0x08) && (~Ru & 0x08)) ||
              ((~Ru & 0x08) && (Rd & 0x08))) {
            sreg = SREG_SET_H(sreg);
          } else {
            sreg = SREG_CLR_H(sreg);
          }
        
          if (((Rd & 0x80) && (Rr & 0x80) && (~Ru & 0x80)) ||
              ((~Rd & 0x80) && (~Rr & 0x80) && (Ru & 0x80))) {
            sreg = SREG_SET_V(sreg);
          } else {
            sreg = SREG_CLR_V(sreg);
          }
        
          sreg = set_N(sreg, Ru);
          sreg = set_S(sreg);
          sreg = set_Z(sreg, Ru);
        
          if (((Rd & 0x80) && (Rr & 0x80)) ||
              ((Rr & 0x80) && (~Ru & 0x80)) ||
              ((~Ru & 0x80) && (Rd & 0x80))) {
            sreg = SREG_SET_C(sreg);
          } else {
            sreg = SREG_CLR_C(sreg);
          }")
	 (code (string-append "void foo() {\n" code "}\n"))
	 (tree (or (parse-string code) (error "parse failed")))
	 (tree (sx-ref* tree 1 3 1))
	 )
    (sf "~A\n" code)
    (ppin tree)
    ;;(pp99 tree)
    ))


;; ffi-help patterns:
;; Figure out how to have ffi-help print message when new pattern shows up.
;;
;; typedef struct foo *bar_t;
;; struct foo; typedef struct foo *bar_t; stuct foo { int a; };
;; typedef struct foo bar_t; struct foo { int a; }; typedef bar_t *baz_t;
;; struct foo { int a; }; typedef struct foo bar_t;
;; struct foo { int a; }; typedef struct foo *bar_t;

;; struct foo; int baz(struct foo*); 

;; case 1
;; typedef struct foo *bar_t; struct foo { int a; }; =>
;; (define struct-foo-desc 'void)
;; (define bar_t (fh:pointer (delay struct-foo-desc)))
;; (define-ffi-pointer-type bar_t struct-foo-desc bar_t? make-bar_t)
;;
;; (set! struct-foo-desc (bs:struct (list `(a ,int))))
;; (define-fh-compound-type struct-foo struct-foo-desc
;;                          struct-foo? make-struct-foo)
;; (fh-ref-deref! bar_t* make-bar_t* struct-foo make-struct-foo)

;; --- last line ---
