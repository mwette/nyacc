;; yaml.scm - yaml

;; Copyright (C) 2020 Matthew R. Wette
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

;;; Code:

(define-module (yaml)
  #:export (read-yaml-file cnvt-tree)
  #:use-module (yaml libyaml)
  #:use-module (yaml ffi-help-rt)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:prefix ffi:))

(use-modules (ice-9 format))
(define (ff fmt . args) (apply format #t fmt args))
(define (sf fmt . args) (apply simple-format #t fmt args))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define-syntax-rule (bs-ref obj ...)
  (bytestructure-ref obj ...))

(define-syntax-rule (fh-ref obj ...)
  (fh-object-ref obj ...))

;; works w/ bytestructures
;; tag-property created via (make-object-property) used to track tags
(define* (cnvt-tree root stack #:optional tag-property)

  ;; not implemented yet
  (define (add-node-tag node tag) node)
  (define add-node-tagx
    (let ((dict (make-hash-table 31)))
      (lambda (node tag)
	(if (and tag-property
		 (not (equal? tag "tag:yaml.org,2002:str")))
	    ;;(unless (hash-ref dict tag) (hash-set! dict tag tag))
	    (set! (tag-property node) (hash-ref dict tag)))
	node)))

  (define (cnvt-scalar-node node)
    (let* ((style (wrap-yaml_scalar_style_t
		   (bs-ref node 'data 'scalar 'style)))
	   (raw (bytestructure-ref node 'data 'scalar 'value))
	   (val (ffi:pointer->string (ffi:make-pointer raw))))
      val))

  (define (cnvt-sequence-node node)
    (let* ((style (wrap-yaml_sequence_style_t
		   (bs-ref node 'data 'sequence 'style)))
	   (start (bs-ref node 'data 'sequence 'items 'start))
	   (end (bs-ref node 'data 'sequence 'items 'end))
	   (top (bs-ref node 'data 'sequence 'items 'top))
	   (item-size (bytestructure-descriptor-size yaml_node_item_t-desc)))
      (let loop ((sequence '()) (addr (- top item-size)))
	(if (>= addr start)
	    (let* ((item (bytestructure int*-desc addr))
		   (indx (1- (bs-ref item '*)))
		   (nd (bs-ref stack indx)))
	      (loop (cons (cnvt-node nd) sequence) (- addr item-size)))
	    (list->vector sequence)))))

  (define (cnvt-mapping-node node)
    (let* ((style (wrap-yaml_mapping_style_t
		   (bs-ref node 'data 'mapping 'style)))
	   (start (bs-ref node 'data 'mapping 'pairs 'start))
	   (end (bs-ref node 'data 'mapping 'pairs 'end))
	   (top (bs-ref node 'data 'mapping 'pairs 'top))
	   (pair-size (bytestructure-descriptor-size yaml_node_pair_t-desc)))
      (let loop ((mapping '()) (addr (- top pair-size)))
	(if (>= addr start)
	    (let* ((pair (bytestructure yaml_node_pair_t*-desc addr))
		   (key-ix (1- (bs-ref pair '* 'key)))
		   (key-nd (bs-ref stack key-ix))
		   (val-ix (1- (bs-ref pair '* 'value)))
		   (val-nd (bs-ref stack val-ix)))
	      (loop (acons (cnvt-node key-nd) (cnvt-node val-nd) mapping)
		    (- addr pair-size)))
	    mapping))))

  (define (cnvt-node node)
    (let ((type (wrap-yaml_node_type_t (bs-ref node 'type))))
      (add-node-tag
       (case type
	 ((YAML_SCALAR_NODE) (cnvt-scalar-node node))
	 ((YAML_SEQUENCE_NODE) (cnvt-sequence-node node))
	 ((YAML_MAPPING_NODE) (cnvt-mapping-node node))
	 (else (error "missed type" type)))
       (ffi:pointer->string (ffi:make-pointer (bs-ref node 'tag))))))

  (cnvt-node root))

(define (read-yaml-file filename)
  (let* ((parser (make-yaml_parser_t))
	 (&parser (pointer-to parser))
	 (document (make-yaml_document_t))
	 (&document (pointer-to document))
	 (file (fopen filename "r")))

    (yaml_parser_initialize &parser)
    (yaml_parser_set_input_file &parser file)
    (yaml_parser_load &parser &document)
    
    (let* ((start (fh-object-ref document 'nodes 'start))
	   (stack (bytestructure yaml_node_t*-desc start))
	   (root (fh-object-val (yaml_document_get_root_node &document)))
	   (yaml (cnvt-tree root stack)))
      (yaml_document_delete &document)
      (yaml_parser_delete &parser)
      (fclose file)
      yaml)))

;; --- last line ---
