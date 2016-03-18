;;; lang/dxl/body.scm
;;;
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

;; dxl parser body, with tables makes a parser

;; ------------------------------------------------------------------------

;; no semicolon insertion
;;(define (NSI) (fluid-set! *insert-semi* #f))

(define (read-cpp-line ch) #f)
(define (fmtout fmt . args) (apply simple-format #t fmt args))

(define ignore-newline-bfore
  '("else" ";" ")" "}"))

(define ignore-newline-after		; see intro of dxl manual
  '(";"  ","  "?"  ":"  "="  "("  "+"  "*"  "["  "&"  "-"  "!"  "~"  "/"  "%"
    "<<"  ">>"  "<>"  "<"  ">"  "<="  ">="  "=="  "!=" 	"^"  "|"  "&&"  "and"
    "||"  "or"  "^^"  "+="  "-="  "*="  "/="  "%="  "<<="  ">>="  "&="  "|="
    "^="  "<-"  ":="  "=>"  ".."  "."  "->"  "::"  "\\"
    ;; extras:
    "{"
    ))

(define dxl-built-in-types
  (append
   '("AccessRec" "AttrDef" "AttrType" "Baseline" "Buffer" "ClipboardLock"
     "Column" "Comment" "DB" "DBE" "Date" "DiscussionFilter"
     "DiscussionStatus" "EmbeddedOleObject" "HistoryType" "Icon" "Filter"
     "Justification" "Link" "LockedList" "Module" "ModuleProperties"
     "Object" "PageLayout" "PartitionDefinition" "PartitionLinkset"
     "ProblemItem" "Project" "Regexp" "RichText" "RifDefinition" "Sort"
     "Stream" "Template" "User" "ArchiveInclusionDescriptor")
   '("ADMABool_" "AttrDef_" "ADAString_" "ATAEnumValues_" "AllObject_"
     "DiscString_" "DiscString_" "DiscStatus_" "DOM_Document_"
     "DropEventBool_" "DropEventString_" "FilteredLdapUserListRef_"
     "HAString_" "HttpHeaderEntryString_" "InLinkRef__" "InPartString_"
     "OSLCServiceProviderDialog_" "OSLCServiceProviderDialogs_"
     "OSLCServiceProviderString_" "OutPartString_" "PartFileString_"
     "RifImportBool_" "RifModuleDefinitionString_" "triLevelMod_"
     "UserElement_")
   '("_d" "_y" "AllRoot__" "Attr__" "AttrDef__" "AttrDxlVal__" "AttrInhVal__"
     "Below__" "LdapUserRef__" "RTF_string__" "Scroll__"
     "SignatureInfoSpecifier__")
   ))

;; @item gen-dxl-lexer => thunk
;; Generate a context-sensitive lexer for the C language.
(define gen-dxl-lexer
  ;; This gets ugly in order to handle cpp.
  ;;.need to add support for num's w/ letters like @code{14L} and @code{1.3f}.
  ;; todo: I think there is a bug wrt the comment reader because // ... \n
  ;; will end up in same mode...  so after
  ;; int x; // comment
  ;; the lexer will think we are not at BOL.
  (let* ((match-table mtab)
	 (read-ident read-c-ident)
	 (read-comm read-c-comm)
	 (ident-like? like-c-ident?)
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt ident-like? strtab))  ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
	 (chrseq (remove-mt ident-like? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	  ; characters in grammar
	 (typtab (map
		  (lambda (str) (cons (string->symbol str)
				      (assq-ref match-table 'built-in-type)))
		  dxl-built-in-types))
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair))))
	 (tv-semi (assoc-ref match-table ";"))
	 (inb (map (lambda (k) (assoc-ref match-table k)) ignore-newline-bfore))
	 (ina (map (lambda (k) (assoc-ref match-table k)) ignore-newline-after))
	 )
    (lambda* (#:key (mode 'code))      ; modes are 'code or 'file
      (let ((bol #t)		       ; begin-of-line condition
	    (prev #f))		       ; prev token
	;; Return the first (tval lval) pair not excluded by the CPP.
	(lambda ()
	  (define (read-token)
	    (let iter ((ch (read-char)))
	      (cond
	       ((eof-object? ch) (assc-$ '($end . EOF)))
	       ((eq? ch #\newline) (set! bol #t) (cons tv-semi ch))
	       ((char-set-contains? c:ws ch) (iter (read-char)))
	       (bol
		(cond
		 #;((read-comm ch) =>
		 (lambda (c) (assc-$ (cons '$lone-comm (cdr c)))))
		 ((read-comm ch bol) (iter (read-char)))
		 ;;((read-cpp-line ch) => (lambda (s) (assc-$ (exec-cpp s))))
		 (else (set! bol #f) (iter ch))))
	       ((read-ident ch) =>
		(lambda (str)
		  (let ((sym (string->symbol str)))
		    (cond ((assq-ref keytab sym) => (lambda (t) (cons t str)))
			  ((assq-ref typtab sym) => (lambda (t) (cons t str)))
			  (else (cons (assq-ref symtab '$ident) str))))))
	       ((read-c-num ch) => assc-$)
	       ((read-c-string ch) => assc-$)
	       ((read-c-chlit ch) => assc-$)
	       #;((read-comm ch) =>
	       (lambda (c) (assc-$ (cons '$code-comm (cdr c)))))
	       ((read-comm ch bol) (iter (read-char)))
	       ;;((and (simple-format #t "chs=>~S\n" (read-chseq ch)) #f))
	       ((read-chseq ch) => identity)
	       ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	       (else (cons ch (string ch))))))

	  ;; need to add back cpp-parse (dxl only uses #include)

	  (define (cont next)
	    (cond
	     ((not prev)
	      (set! prev next)
	      (cont (read-token)))
	     ((and (eqv? (car prev) tv-semi)
		   (memq (car next) inb))
	      (set! prev next)
	      (cont (read-token)))
	     ((and (eqv? (car next) tv-semi)
		   (memq (car prev) ina))
	      (cont (read-token)))
	     (else
	      (let ((curr prev))
		(set! prev next)
		;;(simple-format #t "tok=~S\n" curr)
		curr))))
	  
	  (let ((tokp (read-token)))
	    (cont tokp))
	  
	  )))))

;; --- last line ---
