;;; nyacc/lang/mlang/pprint.scm

;; Copyright (C) 2016,2018,2025 Matthew Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

(define-module (language nx-mlang pprint)
  #:export (pretty-print-ml)
  #:use-module ((srfi srfi-1) #:select (pair-for-each fold-right))
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (ice-9 pretty-print)
  )

;; TODO;; this is C
(define op-sym 
  (let ((ot '(("=" . eq) ("+=" . pl-eq) ("-=" . mi-eq) ("*=" . ti-eq)
              ("/=" . di-eq) ("%=" . mo-eq) ("<<=" . ls-eq) (">>=" . rs-eq)
              ("&=" . ba-eq) ("^=" . bx-eq) ("|=" bo-eq))))
    (lambda (name)
      (assoc-ref ot name))))

;; TODO; this is C
(define op-prec
  ;; in order of decreasing precedence
  '((p-expr ident fixed float string)
    (comp-lit post-inc post-dec sel fctn-call array-ref)
    (de-ref ref-to neg pos not bitwise-not sizeof pre-inc pre-dec)
    (cast)
    (mul div ldiv mod)
    (add sub)
    (lshift rshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and ss-and)
    (or ss-or)
    (cond-expr)
    (assn-expr)
    (comma)))

;; TODO
(define op-assc ;; this is C
  '((left array-ref sel post-inc post-dec comp-lit mul div ldiv mod add sub
          lshift rshift lt gt le ge bitwise-and bitwise-xor bitwise-or and or)
    (right pre-inc pre-dec sizeof bitwise-not not pos neg handle ref-to de-ref
           cast cond assn-expr)
    (nonassoc)))

(define protect-expr? (make-protect-expr op-prec op-assc))

(define* (pretty-print-ml tree #:key (indent-level 4))

  (define fmtr (make-pp-formatter #:width 120 #:basic-offset indent-level))
  (define (push-il) (fmtr 'push))
  (define (pop-il) (fmtr 'pop))
  
  (define sf (lambda args (apply fmtr args)))
  
  (define (unary/l op rep rval)
    (sf rep)
    (if (protect-expr? 'rt op rval)
        (ppx/p rval #f)
        (ppx rval #f)))
  
  (define (unary/r op rep lval)
    (sf rep)
    (if (protect-expr? 'lt op lval)
        (ppx/p lval #f)
        (ppx lval #f)))
  
  (define (binary op rep lval rval)
    (if (protect-expr? 'lt op lval)
        (ppx/p lval #f)
        (ppx lval #f))
    (sf rep)
    (if (protect-expr? 'rt op rval)
        (ppx/p rval #f)
        (ppx rval #f)))

  (define (string->mlang st)
    (if (string-any #\' st)
        (reverse-list->string
         (string-fold
          (lambda (ch seed)
            (if (char=? #\' ch) (cons* #\' ch seed) (cons ch seed)))
          '() st))
        st))

  (define (ppx/p tree nosp) (sf "(") (ppx tree nosp) (sf ")"))
  
  (define (ppx tree nosp)
    (define ppxin (lambda (tree) (ppx tree nosp)))
    (sx-match tree

      ((script-file . ,rest)
       #f)

      ((classdef-file . ,items)
        (for-each ppxin items))

      ;;((class-defn (ident ,name) (attr-list . ,attrs) . ,rest)   )
      ((class-defn (ident ,name) (supers . ,supers) . ,rest)
       (sf "classdef ~a < ~a\n" name (string-join (map cadr supers) " & "))
       (push-il) (for-each ppxin rest) (pop-il) (sf "end\n"))

      ;;((properties (attr-list)
      ((properties . ,items)
       (sf "properties\n") (push-il)
       (for-each ppxin items) (pop-il) (sf "end\n"))
      ((property (ident ,name))
       (sf "~a\n" name))

      ((methods (attr-list . ,attrs) . ,items)
       (sf "methods (") (for-each ppxin attrs) (sf ")\n") (push-il)
       (for-each (lambda (item) (ppxin item) (sf "\n")) items)
       (pop-il) (sf "end\n"))
      ((methods . ,items)
       (sf "methods\n") (push-il)
       (for-each (lambda (item) (ppxin item) (sf "\n")) items)
       (pop-il) (sf "end\n"))

      ((function-sig (ident ,name) ,iputs ,oputs ,coml)
       (case (length (cdr oputs))
         ((0) #f)
         ((1) (sf "~a = " (cadar oputs)))
         (else (sf "[~a] = " (string-join (map cadr oputs) ", "))))
       (case (length (cdr iputs))
         ((0) (sf "~a\n" name))
         (else (sf "~a(" name) (ppx iputs nosp) (sf ")\n"))))

      ((attr (ident ,name) "=" ,expr)
       (sf "~a = " name) (ppxin expr))
      ((attr (ident ,name))
       (sf "~a" name))

      ((function-file . ,items)
       (for-each ppxin items))

      ((fctn-defn (fctn-decl (ident ,name) ,iputs ,oputs ,coml) ,stmt-list)
       (sf "function ")
       (case (length (cdr oputs))
         ((0) #f)
         ((1) (ppxin oputs) (sf " = "))
         ((else) (sf "[") (ppxin oputs) (sf "] = ")))
       (sf "~A(" name) (ppxin iputs) (sf ")\n")
       (for-each ppxin (sx-tail coml 1))
       (push-il) (ppxin stmt-list) (pop-il) (sf "end\n"))
      ((fctn-defn (fctn-decl (ident ,name) ,iputs ,oputs) ,stmt-list)
       (sf "function ")
       (case (length (cdr oputs))
         ((0) #f)
         ((1) (ppxin oputs) (sf " = "))
         ((else) (sf "[") (ppxin oputs) (sf "] = ")))
       (sf "~A(" name) (ppxin iputs) (sf ")\n")
       (push-il) (ppxin stmt-list) (pop-il) (sf "end\n"))

      ((comm ,text)
       (sf "%~A\n" text))

      ((ident-list . ,rest)
       (pair-for-each
        (lambda (pair)
          (ppxin (car pair))
          (if (pair? (cdr pair)) (sf ", ")))
        rest))

      ((ident ,ident)
       (sf "~A" ident))

      ((stmt-list . ,stmts)
       (for-each ppxin stmts))

      ((empty-stmt) (sf "\n"))

      ((call-stmt ,name . ,args)
       (ppxin name) (sf "(")
       (pair-for-each
        (lambda (pair) (ppxin (car pair)) (if (pair? (cdr pair)) (sf ", ")))
        args)
       (sf ");\n"))

      ((assn ,lhs ,rhs)
       (ppxin lhs) (sf " = ") (ppxin rhs) (sf ";\n"))

      ((assn-many (lval-list . ,lvals) ,expr)
       (sf "[")
       (pair-for-each
        (lambda (pair) (ppxin (car pair)) (if (pair? (cdr pair)) (sf ", ")))
        lvals)
       (sf "] = ") (ppxin expr) (sf "\n"))

      ((for (ident ,name) ,expr ,stmt-list)
       (sf "for ~a =" name) (ppxin expr) (sf "\n")
       (push-il) (ppxin stmt-list) (pop-il) (sf "end"))

      ((while ,expr ,stmt-list)
       (sf "while ") (ppxin expr) (sf "\n")
       (push-il) (ppxin stmt-list) (pop-il) (sf "end"))

      ((if ,expr ,stmt-list . ,forms)
       (sf "if ") (ppxin expr) (sf "\n") (push-il)
       (ppxin stmt-list)
       (for-each
        (lambda (form)
          (case (sx-tag form)
            ((elseif)
             (pop-il) (sf "elseif ") (ppxin (sx-ref form 1)) (push-il) (sf "\n")
             (ppxin (sx-ref form 2)))
            ((else)
             (pop-il) (sf "else\n") (push-il)
             (ppxin (sx-ref form 1)))))
        forms)
       (pop-il) (sf "end\n"))

      ((switch ,expr . ,cases)
       (sf "switch ") (ppxin expr) (sf "\n") (push-il)
       (for-each
        (lambda (case)
          (cond
           ((eq? 'otherwise (sx-tag case))
            (sf "otherwise\n") (push-il) (ppxin (sx-ref case 1)) (pop-il))
           (else
            (sf "case ") (ppxin (sx-ref case 1)) (sf "\n")
            (push-il) (ppxin (sx-ref case 2)) (pop-il))))
        cases)
       (pop-il) (sf "end"))

      ((expr-stmt ,expr)
       (ppxin expr)
       (sf ";\n"))

      ((return ,value)
       (sf "return ") (ppxin value) (sf ";\n"))
      ((return)
       (sf "return;\n"))

      ((command ,name . ,args)
       (sf "~A" name)
       (for-each (lambda (arg) (sf " ~A" (sx-ref arg 1))) args)
       (sf "\n"))

      ((aref-or-call ,name ,argx-list)
       (ppxin name) (sf "(")
       (pair-for-each
        (lambda (pair)
          (ppxin (car pair))
          (if (pair? (cdr pair)) (sf ", ")))
        (sx-tail argx-list))
       (sf ")"))

      ((cell-array . ,rows)
       (sf "{")
       (pair-for-each
        (lambda (pair)
          (ppxin (car pair))
          (if (pair? (cdr pair)) (sf "; ")))
        rows)
       (sf "}"))

      ((matrix . ,rows)
       (sf "[")
       (pair-for-each
        (lambda (pair)
          (ppxin (car pair))
          (if (pair? (cdr pair)) (sf "; ")))
        rows)
       (sf "]"))
      ((float-matrix . ,rows)
       (sf "[")
       (pair-for-each
        (lambda (pair) (ppxin (car pair)) (if (pair? (cdr pair)) (sf "; ")))
        rows)
       (sf "]"))
      ((fixed-vector . ,rows)
       (sf "[")
       (pair-for-each
        (lambda (pair) (ppxin (car pair)) (if (pair? (cdr pair)) (sf "; ")))
        rows)
       (sf "]"))

      ((row . ,expr-list)
       (pair-for-each
        (lambda (pair)
          (ppx (car pair) #t)
          (if (pair? (cdr pair)) (sf ", ")))
        expr-list))

      ((transpose ,expr) (ppxin expr) (sf "'"))
      
      ((array-ref ,ident ,expr-list)
       (ppxin ident) (sf "(") (ppxin expr-list) (sf ")"))

      ((cell-ref ,ident ,expr-list)
       (ppxin ident) (sf "{") (ppxin expr-list) (sf "}"))

      ((expr-list . ,items)
       (fold-right                      ; seed is "need comma"
        (lambda (item seed)
          (if seed (sf ", "))
          (cond
           ((eqv? 'colon-expr (sx-tag item)) (sf ":") #f)
           (else (ppxin item) #t)))
        #f
        items))

      ((colon-expr ,start ,incr ,end)
       (ppxin start) (sf ":") (ppxin incr) (sf ":") (ppxin end))
      ((colon-expr ,start ,end)
       (ppxin start) (sf ":") (ppxin end))
      ((colon-expr)
       (sf ":"))
      ((fixed-colon-expr ,start ,incr ,end)
       (ppxin start) (sf ":") (ppxin incr) (sf ":") (ppxin end))
      ((fixed-colon-expr ,start ,end)
       (ppxin start) (sf ":") (ppxin end))
      ((fixed-colon-expr)
       (sf ":"))

      ((or ,lex ,rex) (binary 'or (if nosp "|" " | ") lex rex))
      ((ss-or ,lex ,rex) (binary 'ss-or (if nosp "||" " || ") lex rex))
      ((and ,lex ,rex) (binary 'and (if nosp "&" " & ") lex rex))
      ((ss-and ,lex ,rex) (binary 'ss-and (if nosp "&&" " && ") lex rex))
      
      ((pos ,expr) (unary/l 'pos "+" expr))
      ((neg ,expr) (unary/l 'neg "-" expr))
      ((handle ,expr) (unary/l 'handle "@" expr))

      ((lt ,lval ,rval) (binary 'lt (if nosp "<" " < ") lval rval))
      ((gt ,lval ,rval) (binary 'gt (if nosp ">" " > ") lval rval))
      ((le ,lval ,rval) (binary 'le (if nosp "<=" " <= ") lval rval))
      ((ge ,lval ,rval) (binary 'ge (if nosp ">=" " >= ") lval rval))
      ((eq ,lval ,rval) (binary 'eq (if nosp "==" " == ") lval rval))
      ((neq ,lval ,rval) (binary 'neq (if nosp "~=" " ~= ") lval rval))

      ((add ,lval ,rval) (binary 'add (if nosp "+" " + ") lval rval))
      ((sub ,lval ,rval) (binary 'sub (if nosp "-" " - ") lval rval))
      ((mul ,lval ,rval) (binary 'mul "*" lval rval))
      ((div ,lval ,rval) (binary 'div "/" lval rval))
      ((ldiv ,lval ,rval) (binary 'ldiv "\\" lval rval))

      ((neg ,expr) (sf "-") (ppxin expr))
      ((pos ,expr) (sf "+") (ppxin expr))
      ((not ,expr) (sf "~") (ppxin expr))
      ((handle ,expr) (sf "@") (ppxin expr))

      ((sel ,id ,ex) (binary 'sel "." ex id))
      ((wrap ,ex) (ppx ex #f))

      ((fixed ,value) (sf "~A" value))
      ((float ,value) (sf "~A" value))
      ((string ,value) (sf "'~A'" (string->mlang value)))
      ((end) (sf "end"))
      
      (,_ (simple-format #t "\n*** NOT HANDLED: ~S\n" (car tree)))))

  (ppx tree #f))


#;(use-modules (nyacc lang mlang parser))
#;(let ((sx (with-input-from-file "exam.d/ex02.m" parse-ml))
      )
  (pretty-print sx)
  ;;(simple-format #t "==>\n")
  ;;(pretty-print-ml sx)
  )

;; --- last line ---
