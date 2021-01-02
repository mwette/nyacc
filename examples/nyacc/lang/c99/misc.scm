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

