;; nyacc/lang/c99/utilZ.scm - C processing code
;; 
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by 
;;; the Free Software Foundation, either version 3 of the License, or 
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; code not used
;; v151112a - 

(define-module (nyacc lang c99 util2)
  #:export (tree->udict
	    match-decl match-comp-decl
	    stripdown
	    udecl->mspec
	    declr->ident
	    
	    find-tf-decls
	    tf-decl-ref
	    canize-decl
	    canize-enum-def-list gen-enum-utils
	    find-used-typenames
	    resolve-typename
	    expand-decl-typerefs
	    filter-typedefs
	    repl-typename
	    fix-fields
	    )
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)		; let*-values
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (nyacc lang util)
  )

;; === possibly old stuff ==========

;; @item match-tf-decl sx
;; If a typedef struct or function declaration return that, or @code{#f}.
;; This is a helper for find-tf-decls.
(define (match-tf-decl sx)
  (sxml-match sx

    ;; struct type decl
    ((decl (decl-spec-list
	    (stor-spec (typedef))
	    (type-spec
	     ;;(struct-def (field-list (comp-decl ,flds ...) ...))))
	     (struct-def (field-list ,flds ...))))
	   (init-declr-list (init-declr (ident ,name))))
     (list '(type . sdecl) (cons 'name name) (list 'fields flds ...)))

    ;; enum type decl
    ((decl (decl-spec-list
	    (stor-spec (typedef))
	    (type-spec
	     (enum-def ,rest ...)))
	   (init-declr-list (init-declr (ident ,name))))
     (list '(type . edecl) (cons 'name name) rest ...))

    ;; function decl
    ((decl
      (decl-spec-list ,ret-type)
      (init-declr-list
       (init-declr (ftn-declr (ident ,name) (param-list ,params ...)))))
     (list '(type . fdecl) (cons 'name name) (list 'param-list params ...)
	   (cons 'ret-ty ret-type)))

    (,otherwise #f)))

;; @item find-tf-decls sx
;; Filter struct typedef and function declarations.
(define (find-tf-decls tree)
  (let iter ((res '()) (tree tree))
    (if (null? tree) (reverse res)
	(iter
	 (or (and=> (match-tf-decl (car tree)) (lambda (m) (cons m res))) res)
	 (cdr tree)))))


;; @item fix-fields flds => flds
;; This will take a list of fields from a struct and remove lone comments.
;; If a field following a lone comment has no code-comment, the lone comment
;; will be used.  That is,
;; @example
;;   /* foo */
;;   int x;
;; @end example
;; @noindent
;; is the same as
;; @example
;;   int x; /* foo */
;; @end example
;; @noindent
(define (fix-fields flds)
  (let iter ((rz '()) (cl '()) (fl flds))
    ;;(pretty-print fl)
    (cond
     ((null? fl) (reverse rz))
     ((eqv? 'comment (caar fl))
      (iter rz (cons (cadar fl) cl) (cdr fl)))
     ((eqv? 'comp-decl (caar fl))
      (if (eq? 4 (length (car fl)))
	  (iter (cons (car fl) rz) '() (cdr fl)) ; has comment
	  (let* ((cs (apply string-append (reverse cl))) ; add comment
		 (fd (append (car fl) (list (list 'comment cs)))))
	    (iter (cons fd rz) '() (cdr fl)))))
     (else
      (error "bad field")))))

;; @item tf-decl-ref tf-decl-l name
;; Select struct-typedef or function declartion from list.
;; @example
;; (tf-decl-ref '((tdecl (name . "foo") (fields ...)) ...) "foo")
;; =>   (tdecl (name . "foo") (fields ...))
;; @end example
(define (tf-decl-ref tf-decl-l name)
  (let iter ((tf-l tf-decl-l))
    (if (null? tf-l) #f
	(if (string=? (assoc-ref (car tf-l) 'name) name) (car tf-l)
	    (iter (cdr tf-l))))))

;; @item canize-decl decln-spec declr-list
;; Given declaration specifier and declator list
;; canize-decl (type-spec ...) (comp-declr-list ...) =>
;;  ((float . "double") (name . "quat") (size . 4)
;; TODO: I think this should be a fold
(define (canize-decl t-spec d-list)
  ;;(simple-format #t "f5: ~S ~S\n" t-spec d-list)
  (let* ((ty (sxml-match t-spec
	       ((type-spec (fixed-type ,name))
		(cons 'fixed name))
	       ((type-spec (float-type ,name))
		(cons 'float name))
	       ((type-spec (typename ,name))
		(cons 'tname name))
	       ((type-spec (struct-def ,guts ...))
		(list 'struc guts ...))
	       (,otherwise
		(error "t-spec off:" t-spec))))
	 (dl (sxml-match (cadr d-list)
	       ((array-of (ident ,name) (p-expr (fixed ,size)))
		(cons name (string->number size)))
	       ((array-of (ident ,name) (p-expr (ident ,size)))
		(cons name size))
	       ((ident ,name)
		(cons name 1))
	       (,otherwise
		(pretty-print d-list)
		(error "d-list off")))))
    (list (cons 'type ty) (cons 'name (car dl)) (cons 'size (cdr dl)))))

;; @resolve-typename name tn-dict
;; OBSOLETE, I THINK
;; given a name which appears in @code{(typename . name)} generate a resolved
;; type.
(define (resolve-typename name tn-dict)
  
  (define (tdef-specs specs decls)
    (if (null? decls) specs
	(tdef-specs
	 (sxml-match (car decls)
	   ((decl (decl-spec-list (stor-spec (typedef))
				  (type-spec . ,spec )) ...)
	    ;;(simple-format #t "~S\n" (caar spec))
	    (cons (cons 'type-spec (car spec)) specs))
	   (,otherwise specs))
	 (cdr decls))))

  (define (resolve-spec spec)
    ;;(simple-format #t "decl=~S\n" spec)
    ;;(newline)
    ;;(pretty-print spec)
    (sxml-match spec
      ((type-spec (enum-def . ,guts))
       (simple-format #t "enum\n"))
      ((type-spec (struct-def . ,guts))
       (simple-format #t "struct\n"))
      ((type-spec (union-def . ,guts))
       (simple-format #t "union\n"))
      ((type-spec (fixed-type ,name))
       (simple-format #t "fixed-type\n"))
      ((type-spec (float-type ,name))
       (simple-format #t "float-type\n"))
      ((type-spec (typename ,name))
       (simple-format #t "typename\n"))
      (,otherwise
       (simple-format #t "todo: ~S\n" spec))
      ))

  ;;(define (resolve-enum spec)
  ;; This should remove comments and enumerate the symbols

  (let ()
    #f))
      
;; @item find-used-typenames td-list
;; for structure etc types find referenced typenames
(define (find-used-typenames td-list)
  (let iter ((rez '()) (td-l td-list))
    (if (null? td-l) rez
	(iter
	 (case (assq-ref (car td-l) 'type)
	   ((sdecl)
	    (fold
	     (lambda (elt rez)
	       (sxml-match elt
		 ((comp-decl (type-spec (typename ,tname)) ,rest ...)
		  (cons tname rez))
		 (,otherwise
		  rez)))
	     rez
	     (assq-ref (car td-l) 'fields)))
	   (else
	    rez))
	 (cdr td-l)))))

;; ==== c code generation =============

;; @item gen-enum-utils name enum-def-list port
;; Generate C code to convert enum names to/from values.
;; This routine expects all enums to have defined constants.
;; Run through canize-enum-def-list if not sure.
(define (gen-enum-utils name enum-def-list port)
  (let* ((bl "                  ") (mx 2)
	 (f (lambda (in fmt . args)
	      (apply simple-format port
		     (string-append (substring bl 0 (* mx in)) fmt)
		     args)))
	 )
    (f 0 "static const char *~A_names = \"~A\";\n\n"
       name
       (string-join
	(map (lambda (defn) (car (assq-ref defn 'ident))) (cdr enum-def-list))
	" ")
       )
    (f 0 "static int ~A_n2v(const char *name) {\n" name)
    (for-each
     (lambda (defn)
       ;;(simple-format #t "defn=~S\n" defn)
       (let ((name (car (assq-ref defn 'ident)))
	     (ival (cadar (assq-ref defn 'p-expr)))
	     )
	 (f 1 "if (strcmp(name, ~S) == 0) return ~A;\n" name ival)
	 ))
     (cdr enum-def-list))
    (f 1 "return -1;\n")
    (f 0 "}\n\n")
    (f 0 "static const char * ~A_v2n(int ival) {\n" name)
    (f 1 "switch (ival) {\n")
    (for-each
     (lambda (defn)
       (let ((name (car (assq-ref defn 'ident)))
	     (ival (cadar (assq-ref defn 'p-expr)))
	     )
	 (f 1 "case ~A: return ~S; break;\n" ival name)
	 ))
     (cdr enum-def-list))
    (f 1 "default: return \"\"; break;\n")
    (f 1 "}\n")
    (f 0 "}\n")
    ))

;; A variable specification, or v-spec, is a triple:
;; name: string name
;; type: string type
;; size: array length

;; "short" "short int" "signed short" "signed short int"
;; "int" "signed" "signed int"
;; "long" "long int" "signed long" "signed long int"
;; "long long" "long long int" "signed long long" "signed long long int"
;; "unsigned short int" "unsinged short"
;; "unsigned int" "unsigned"
;; "unsigned long "unsigned long"
;; "unsigned long long int" "unsigned long long"
;; "char" "signed char" "unsigned char"
;; "_Bool"
;; "float" "double" "long double"
;; "_Complex" "float _Complex" "double _Complex" "long double _Complex"

;; types -> builtin over
;; time_t -> signed long
;; size_t -> unsigned long
(define type-map
  '(("time_t" . "long")
    ("clock_t" . "long")
    ("size_t" . "long")
    ("FILE" . "void*") ;; ???
    ("ptrdiff_t" . "long")
    ("wchar_t" . "short")
    ("int8_t" . "char") ("uint8_t" . "unsigned char")
    ("int16_t" . "short") ("uint16_t" . "unsigned short")
    ("int32_t" . "int") ("uint32_t" . "unsigned int")
    ("int64_t" . "long long") ("uint64_t" . "unsigned long long")
    ("va_list" . "long")
    ))
  
(define (type-conv name) (assoc-ref type-map name))

;; --- last line ---
