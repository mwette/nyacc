;;; examples/nyacc/lang/cxx/mach.scm
;;;
;;; Copyright (C) 2016 Matthew R. Wette
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FIT"!="SS "for" A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#| based on CxxGrammar.y, distributed w/o copyright or license:
 * This is a yacc-able parser for the entire ANSI C++ grammar with no
 * unresolved conflicts. The parse is SYNTACTICALLY consistent and requires 
 * no template or type name assistance.  The grammar in the C++ standard notes
 * that its grammar is a superset of the true grammar requiring semantic
 * constraints to resolve ambiguities. This grammar is a really big superset
 * unifying expressions and declarations, eliminating the type/non-type
 * distinction, and iterating to find a consistent solution to the
 * template/arith,metoic < ambiguity.  As a result the grammar is much 
 * simpler, but requires the missing semantic constraints to be performed in
 * a subsequent semantic pass, which is of course where they belong. This
 * grammar will support conversion of C++ tokens into an Abstract Syntax 
 * Tree. A lot of further work is required to make that tree useful.
 *
 *  Author:         E.D.Willink             Ed.Willink@rrl.co.uk
 *  Date:           19-Nov-1999
 *
 * see also: http://www.edwillink.plus.com/projects/fog/
 *
|#

(define-module (nyacc lang cxx mach)
  #:export (cxx-spec gen-cxx-files)
  #:use-module (nyacc lalr)
  #:use-module (nyacc bison)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang util)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module ((sxml xpath) #:select (sxpath))
  )
  

(define cxx-notice "
This work is derived from CxxGrammar.y by E.D.Willink, which was
provided without copyright or license.  Adaptation to nyacc is

Copyright (C) 2016 Matthew R. Wette

This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
or any later version published by the Free Software Foundation.
See the file COPYING included with the this distribution.")

(define cxx-spec
  (lalr-spec
   (notice cxx-notice)
   (start translation-unit)
   (prec<
    (nonassoc 'shift-there)
    (nonassoc "::" "else" "++" "--" "+" "-" "*" "&" "[" "{" "<" ":" $string)
    (nonassoc 'reduce-here-mostly)
    (nonassoc "(")
    )
   (grammar
    #|
    * The %prec resolves the 14.2-3 ambiguity:
    * Identifier '<' is forced to go through the is-it-a-template-name test
    * All names absorb "template" with the name, so that no template_test is 
    * performed for them.  This requires all potential declarations within
    * an expression to perpetuate this policy and thereby guarantee the 
    * ultimate coverage of explicit_instantiation.
    *
    * The %prec also resolves a conflict in identifier : which is forced to
    * be a shift of a label for  a labeled-statement rather than a reduction
    * for the name of a bit-field or generalised constructor.  This is pretty
    * dubious syntactically but correct for all semantic possibilities.  The
    * shift is only activated when the ambiguity exists at the start of a 
    * statement. In this context a bit-field declaration or constructor
    * definition are not allowed.
    |#

    (identifier ($ident))

    (id
     (identifier ($prec 'shift-there))
     (identifier template-test "+" template-argument-list ">")
     (identifier template-test "-" ) ;; requeued < follows
     (template-id)
     )

    (template-test
     ;; Queue '+' or '-' < as follow on 
     ("<" ($$ (template-test)))
     )

    (global-scope
     ("::")
     ("template" global-scope)
     )

    (id-scope (id "::"))

    #|
    * A :: B :: C; is ambiguous How much is type and how much name ?
    * The %prec maximises the (type) length which is the 7.1-2 semantic 
    * constraint.
    |#

    (nested-id
     (id ($prec 'shift-there)) ;; Maximise length
     (id-scope nested-id)
     )

    (scoped-id
     (nested-id)
     (global-scope nested-id)
     )

    #|
    * destructor-id has to be held back to avoid a conflict with a one's 
    * complement as per 5.3.1-9. It gets put back only when scoped or in 
    * a declarator-id, which is only used as an explicit member name.
    * Declarations of an unscoped destructor are always parsed as a one's
    * complement.
    |#
    (destructor-id
     ("~" id)
     ("template" destructor-id)
     )
    
    (special-function-id
     (conversion-function-id)
     (operator-function-id)
     ("template" special-function-id)
     )

    (nested-special-function-id
     (special-function-id)
     (id-scope destructor-id)
     (id-scope nested-special-function-id)
     )

    (scoped-special-function-id
     (nested-special-function-id)
     (global-scope nested-special-function-id)
     )

    ;; declarator-id is all names in all scopes, except reserved words
    (declarator-id
     (scoped-id)
     (scoped-special-function-id)
     (destructor-id)
     )

    #|
    The standard defines pseudo-destructors in terms of type-name, which 
    is class/enum/typedef, of which class-name is covered by a normal 
    destructor. pseudo-destructors are supposed to support ~int() in
    templates, so the grammar here covers built-in names. Other names 
    are covered by the lack of   identifier/type discrimination.
    |#

    (built-in-type-id
     (built-in-type-specifier)
     (built-in-type-id built-in-type-specifier)
     )
    
    (pseudo-destructor-id
     (built-in-type-id "::" "~" built-in-type-id)
     ("~" built-in-type-id)
     ("template" pseudo-destructor-id)
     )
    
    (nested-pseudo-destructor-id
     (pseudo-destructor-id)
     (id-scope nested-pseudo-destructor-id)
     )

    (scoped-pseudo-destructor-id
     (nested-pseudo-destructor-id)
     (global-scope scoped-pseudo-destructor-id)
     )

    ;; A.2 Lexical conventions
    #|
    * String concatenation is a phase 6, not phase 7 activity so does not 
    * really belong in the grammar.  However it may be convenient to have 
    * it here to make this grammar fully functional.  Unfortunately it 
    * introduces a conflict with the generalised parsing of extern "C"
    * which is correctly resolved to maximise the string length as the 
    * token source should do anyway.
    |#

    ;; Perverse order avoids conflicts 
    (string
     (StringLiteral ($prec 'shift-there) ($$ (make-tl 'string $1)))
     ;; TODO: the following generates massive reduce-reduce conflicts
     ;;(StringLiteral string ($$ (tl-insert $2 $1)))
     )

    (literal
     (IntegerLiteral)
     (CharacterLiteral)
     (FloatingLiteral)
     (string)
     (boolean-literal)
     )

    (IntegerLiteral ($fixed))
    (CharacterLiteral ($chlit))
    (FloatingLiteral ($float))
    (StringLiteral 
     ($string ($$ (make-tl 'string $1)))
     (StringLiteral $string ($$ (tl-append $1 $2))))
    (boolean-literal
     ("false" ($$ `(boolean ,$1)))
     ("true" ($$ `(boolean ,$1))))

    ;; A.3 Basic concepts
    (translation-unit
     (opt-declaration-seq)
     )

    ;;A.4 Expressions
    #|
    * primary-expression covers an arbitrary sequence of all names with the 
    * exception of an unscoped destructor, which is parsed as its unary 
    * expression which is the correct disambiguation (when ambiguous). This
    * eliminates the traditional A(B) meaning A B ambiguity, since we never 
    * have to tack an A onto the front of something that might start with (.
    * The name length got maximised ab initio. The downside is that semantic 
    * interpretation must split the names up again.
    *
    * Unification of the declaration and expression syntax means that unary 
    * and binary pointer declarator operators:
    *     int * * name
    * are parsed as binary and unary arithmetic operators (int) * (*name). 
    * Since type information is not used ambiguities resulting from a cast
    *      (cast)*(value)
    * are resolved to favour the binary rather than the cast unary to ease
    * AST clean-up.  The cast-call ambiguity must be resolved to the cast 
    * to ensure that (a)(b)c can be parsed.
    *
    * The problem of the functional cast ambiguity
    *     name(arg)
    * as call or declaration is avoided by maximising the name within the 
    * parsing kernel. So primary-id-expression picks up 
    *     extern long int const var = 5;
    * as an assignment to the syntax parsed as "extern long int const var". 
    * The presence of two names is parsed so that "extern long into const" 
    * is distinguished from "var" considerably simplifying subsequent
    * semantic resolution.
    *
    * The generalised name is a concatenation of potential type-names 
    * (scoped identifiers or built-in sequences) plus optionally one of 
    * the special names such as an operator-function-id, 
    * conversion-function-id or destructor as the final name. 
    |#
    (primary-expression
     (literal)
     ("this")
     (suffix-decl-specified-ids)
     ;;("::" identifier) covered by suffix-decl-specified-ids
     ;;("::" operator-function-id) covered by suffix-decl-specified-ids
     ;;("::" qualified-id) covered by suffix-decl-specified-ids
     ;; Prefer binary to unary ops, cast to call:
     (abstract-expression ($prec 'reduce-here-mostly))
     ;;(id-expression) covered by suffix-decl-specified-ids
     )
    
    ;; Abstract-expression covers the () and [] of abstract-declarators.
    (abstract-expression
     (parenthesis-clause)
     ("[" opt-expression "]")
     ("template" abstract-expression)
     )

    #|
    * Type I function parameters are ambiguous with respect to the 
    * generalised name, so we have to do a lookahead following any 
    * function-like parentheses. This unfortunately hits normal 
    * code, so kill the -- lines and add the ++ lines for efficiency.
    * Supporting Type I code under the superset causes perhaps 25% of
    * lookahead parsing. Sometimes complete class definitions get 
    * traversed since they are valid generalised type I parameters!
    |#
    (type1-parameters 
     (parameter-declaration-list ";")  ;; ----
     (type1-parameters parameter-declaration-list ";")
     )
    
    (mark-type1
     ($empty ($$ (mark-type1) (yyclearin)))
     )
    
    (postfix-expression
     (primary-expression)
     ;; (postfix-expression parenthesis-clause)
     (postfix-expression parenthesis-clause mark-type1 "-") ;; ----
     ;;----
     (postfix-expression
      parenthesis-clause mark-type1 "+" type1-parameters mark "{" $error
      ($$ (yyerrok) (remark-type1) (unmark) (unmark)))
     (postfix-expression
      parenthesis-clause mark-type1 "+" type1-parameters mark $error
      ($$ (yyerrok) (remark-type1) (unmark) (unmark)))
     (postfix-expression
      parenthesis-clause mark-type1 "+" $error
      ($$ (yyerrok) (yyclearin) (remark-type1) (unmark)))
     (postfix-expression "[" opt-expression "]")
     ;;(destructor-id "[" opt-expression "]")
     ;; ^-- not semantically valid
     ;;(destructor-id parenthesis-clause)
     ;; ^-- omitted to resolve known ambiguity
     ;;(simple-type-specifier "(" opt-expression-list ")")
     ;; ^-- simple-type-specifier is a primary-expression
     (postfix-expression "." declarator-id)
     ;;(postfix-expression "." "template" declarator-id)
     ;; ^-- "template" absorbed into declarator-id.
     (postfix-expression "." scoped-pseudo-destructor-id)
     (postfix-expression "->" declarator-id)
     ;;(postfix-expression "->" "template" declarator-id)
     ;; ^-- "template" absorbed into declarator-id.
     (postfix-expression "->" scoped-pseudo-destructor-id)
     (postfix-expression "++")
     (postfix-expression "--")
     ("dynamic_cast" "<" type-id ">" "(" expression ")")
     ("static_cast" "<" type-id ">" "(" expression ")")
     ("reinterpret_cast" "<" type-id ">" "(" expression ")")
     ("const_cast" "<" type-id ">" "(" expression ")")
     ("typeid" parameters-clause)
     ;;("typeid" "(" expression ")")
     ;;^-- covered by parameters-clause
     ;;("typeid" "(" type-id ")" )
     ;;^-- covered by parameters-clause
     )
    
    (opt-expression-list
     ()
     (expression-list)
     )

    (expression-list
     (assignment-expression)
     (expression-list "," assignment-expression)
     )

    (unary-expression
     (postfix-expression)
     ("++" cast-expression)
     ("--" cast-expression)
     (ptr-operator cast-expression)
     ;;("*" cast-expression)
     ;; ^-- covered by ptr-operator
     ;;("&" cast-expression)
     ;; ^-- covered by ptr-operator
     ;;(decl-specifier-seq "*" cast-expression)
     ;; ^-- covered by binary operator
     ;;(decl-specifier-seq "&" cast-expression)
     ;; ^-- covered by binary operator
     (suffix-decl-specified-scope star-ptr-operator cast-expression)
     ;; ^-- covers e.g int ::type::* const t = 4
     ("+" cast-expression)
     ("-" cast-expression)
     ("!" cast-expression)
     ("~" cast-expression)
     ("sizeof" unary-expression)
     ;;("sizeof" "(" type-id ")")
     ;;^-- covered by unary-expression
     (new-expression)
     (global-scope new-expression)
     (delete-expression)
     (global-scope delete-expression)
     ;;("delete" "[" "]" cast-expression)
     ;;^-- covered by "delete" cast-expression since cast-expression covers ...
     ;;("::" "delete" "[" "]" cast-expression)
     ;;^-- ... abstract-expression cast-expression and so [] cast-expression
     )
    
    (delete-expression
     ("delete" cast-expression) ;; also covers delete[] cast-expression
     )
    
    (new-expression
     ("new" new-type-id opt-new-initializer)
     ("new" parameters-clause new-type-id opt-new-initializer)
     ("new" parameters-clause)
     ;;("new" "(" type-id ")")  covered by parameters-clause
     ("new" parameters-clause parameters-clause opt-new-initializer)
     ;;("new" "(" type-id ")" new-initializer)
     ;; ^^ covered by parameters-clause parameters-clause
     ;;("new" parameters-clause "(" type-id ")")
     ;; ^^ covered by parameters-clause parameters-clause
     )

    ;; opt-ptr-operator-seq production reused to save a %prec
    (new-type-id
     (type-specifier opt-ptr-operator-seq)
     (type-specifier new-declarator)
     (type-specifier new-type-id)
     )
    
    (new-declarator
     (ptr-operator new-declarator)
     (direct-new-declarator)
     )
    
    (direct-new-declarator
     ("[" expression "]")
     (direct-new-declarator "[" constant-expression "]")
     )
    
    (opt-new-initializer
     ()
     ("(" opt-expression-list ")")
     )

    #|
    * cast-expression is generalised to support a [] as well as a () prefix. 
    * This covers the omission of "delete"[] which when followed by a 
    * parenthesised expression was ambiguous. It also covers the gcc indexed
    * array initialisation for free.
    |#
    (cast-expression
     (unary-expression)
     (abstract-expression cast-expression)
     ;;("(" type-id ")" cast-expression) covered by abstract-expression
     )
    
    (pm-expression
     (cast-expression)
     (pm-expression ".*" cast-expression)
     (pm-expression "->*" cast-expression)
     )
    
    (multiplicative-expression
     (pm-expression)
     (multiplicative-expression star-ptr-operator pm-expression)
     (multiplicative-expression "/" pm-expression)
     (multiplicative-expression "%" pm-expression)
     )
    
    (additive-expression
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression)
     (additive-expression "-" multiplicative-expression)
     )
    
    (shift-expression
     (additive-expression)
     (shift-expression "<<" additive-expression)
     (shift-expression ">>" additive-expression)
     )
    
    (relational-expression
     (shift-expression)
     (relational-expression "<" shift-expression)
     (relational-expression ">" shift-expression)
     (relational-expression "<=" shift-expression)
     (relational-expression ">=" shift-expression)
     )
    
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression)
     (equality-expression "!=" relational-expression)
     )
    
    (and-expression
     (equality-expression)
     (and-expression "&" equality-expression)
     )
    
    (exclusive-or-expression
     (and-expression)
     (exclusive-or-expression "^" and-expression)
     )
    
    (inclusive-or-expression
     (exclusive-or-expression)
     (inclusive-or-expression "|" exclusive-or-expression)
     )
    
    (logical-and-expression
     (inclusive-or-expression)
     (logical-and-expression "&&" inclusive-or-expression)
     )
    
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression)
     )
    
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" expression ":" assignment-expression)
     )

    #|
    * assignment-expression is generalised to cover the simple assignment of 
    * a braced initializer in order to contribute to the coverage of 
    * parameter-declaration and init-declaration.
    |#
    (assignment-expression
     (conditional-expression)
     (logical-or-expression assignment-operator assignment-expression)
     (logical-or-expression "=" braced-initializer)
     (throw-expression)
     )
    
    (assignment-operator
     ("=") ("+=") ("&=") ("/=") ("%=") ("*=") ("|=")
     ("<<=") (">>=") ("-=") ("^=")
     )

    #|
    * expression is widely used and usually single-element, so the reductions
    * are arranged so that a single-element expression is returned as is. 
    * Multi-element expressions are parsed as a list that may then behave 
    * polymorphically as an element or be compacted to an element.
    |#
    (opt-expression
     ()
     (expression)
     )
    
    (expression
     (assignment-expression)
     (expression-list "," assignment-expression)
     )
    
    (constant-expression
     (conditional-expression)
     )

    ;; The grammar is repeated for when the parser stack knows that the next >
    ;; must end a template.
    (templated-relational-expression
     (shift-expression)
     (templated-relational-expression "<" shift-expression)
     (templated-relational-expression "<=" shift-expression)
     (templated-relational-expression ">=" shift-expression)
     )
    
    (templated-equality-expression
     (templated-relational-expression)
     (templated-equality-expression "==" templated-relational-expression)
     (templated-equality-expression "!=" templated-relational-expression)
     )
    
    (templated-and-expression
     (templated-equality-expression)
     (templated-and-expression "&" templated-equality-expression)
     )
    
    (templated-exclusive-or-expression
     (templated-and-expression)
     (templated-exclusive-or-expression "^" templated-and-expression)
     )

    (templated-inclusive-or-expression
     (templated-exclusive-or-expression)
     (templated-inclusive-or-expression "|" templated-exclusive-or-expression)
     )

    (templated-logical-and-expression
     (templated-inclusive-or-expression)
     (templated-logical-and-expression "&&" templated-inclusive-or-expression)
     )

    (templated-logical-or-expression
     (templated-logical-and-expression)
     (templated-logical-or-expression "||" templated-logical-and-expression)
     )

    (templated-conditional-expression
     (templated-logical-or-expression)
     (templated-logical-or-expression
      "?" templated-expression ":" templated-assignment-expression)
     )

    (templated-assignment-expression
     (templated-conditional-expression)
     (templated-logical-or-expression
      assignment-operator templated-assignment-expression)
     (templated-throw-expression)
     )

    (templated-expression
     (templated-assignment-expression)
     (templated-expression-list "," templated-assignment-expression)
     )

    (templated-expression-list
     (templated-assignment-expression)
     (templated-expression-list "," templated-assignment-expression)
     )
    
    ;; A.5 Statements
    ;; Parsing statements is easy once simple-declaration has been
    ;; generalised to cover expression-statement.
    (looping-statement
     (start-search looped-statement ($$ (end-search)))
     )
    
    (looped-statement
     (statement)
     (advance-search "+" looped-statement)
     (advance-search "-")
     )
    
    (statement
     (control-statement)
     ;;(expression-statement) covered by declaration-statement
     (compound-statement)
     (declaration-statement)
     (try-block)
     )
    
    (control-statement
     (labeled-statement)
     (selection-statement)
     (iteration-statement)
     (jump-statement)
     )
    
    (labeled-statement
     (identifier ":" looping-statement)
     ("case" constant-expression ":" looping-statement)
     ("default" ":" looping-statement)
     )

    ;; (expression-statement (opt-expression ";")) 
    ;; ^^ covered by declaration-statement
    
    (compound-statement
     ("{" opt-statement-seq "}")
     ("{" opt-statement-seq looping-statement "#" bang $error "}"
      ($$ (UNBANG "Bad statement-seq.")))
     )
    
    (opt-statement-seq
     ($empty) 
     (opt-statement-seq looping-statement)
     (opt-statement-seq looping-statement "#" bang $error ";"
			($$ (UNBANG "Bad statement.")))
     )
    

    ;;  The dangling else conflict is resolved to the innermost if.
    (selection-statement
     ("if" "(" condition ")" looping-statement ($prec 'shift-there))
     ("if" "(" condition ")" looping-statement "else" looping-statement)
     ("switch" "(" condition ")" looping-statement)
     )
    
    (opt-condition
     ($empty)
     (condition)
     )
  
    (condition
     (parameter-declaration-list)
     ;;(expression covered by parameter-declaration-list
     ;;(type-specifier-seq declarator "=" assignment-expression)
     ;;^^ covered by parameter-declaration-list
     )

    (iteration-statement
     ("while" "(" condition ")" looping-statement)
     ("do" looping-statement "while" "(" expression ")" ";")
     ("for" "(" for-init-statement opt-condition ";" opt-expression ")"
      looping-statement)
     )

    (for-init-statement
     (simple-declaration)
     ;;(expression-statement) covered by simple-declaration
     )
    
    (jump-statement
     ("break" ";")
     ("continue" ";")
     ("return" opt-expression ";")
     ("goto" identifier ";")
     )
    
    (declaration-statement
     (block-declaration)
     )

    ;; A.6 Declarations
    (compound-declaration
     ("{" nest opt-declaration-seq "}" ($$ (unnest)))
     ("{" nest opt-declaration-seq util looping-declaration "#" bang $error "}"
      ($$ (unnest) (UNBANG "Bad declaration-seq.")))
     )
    
    (opt-declaration-seq
     ($empty)
     (opt-declaration-seq util looping-declaration)
     (opt-declaration-seq util looping-declaration "#" bang $error ";"
			  ($$ (UNBANG "Bad declaration.")))
     )
    
    (looping-declaration
     (start-search1 looped-declaration ($$ (end-search)))
     )
    
    (looped-declaration
     (declaration)
     (advance-search "+" looped-declaration)
     (advance-search "-")
     )
    
    (declaration
     (block-declaration)
     (function-definition)
     (template-declaration)
     ;;(explicit-instantiation) covered by relevant declarations
     (explicit-specialization)
     (specialised-declaration)
     )
    
    (specialised-declaration
     (linkage-specification)
     (namespace-definition)
     ("template" specialised-declaration)
     )
    
    (block-declaration
     (simple-declaration)
     (specialised-block-declaration)
     )
    
    (specialised-block-declaration
     (asm-definition)
     (namespace-alias-definition)
     (using-declaration)
     (using-directive)
     ("template" specialised-block-declaration)
     )
    
    (simple-declaration
     (";")
     (init-declaration ";")
     (init-declarations ";")
     (decl-specifier-prefix simple-declaration)
     )

    #|
    * A decl-specifier following a ptr-operator provokes a shift-reduce
    * conflict for const name which is resolved in favour of the pointer,
    * and implemented by providing versions of decl-specifier guaranteed
    * not to start with a cv-qualifier.
    *
    * decl-specifiers are implemented type-centrically. That is the semantic 
    * constraint that there must be a type is exploited to impose structure,
    * but actually eliminate very little syntax. built-in types are multi-name
    * and so need a different policy.
    * 
    * non-type decl-specifiers are bound to the left-most type in a 
    * decl-specifier-seq, by parsing from the right and attaching suffixes to
    * the right-hand type. Finally residual prefixes attach to the left.
    |#
    (suffix-built-in-decl-specifier.raw
     (built-in-type-specifier)
     (suffix-built-in-decl-specifier.raw built-in-type-specifier)
     (suffix-built-in-decl-specifier.raw decl-specifier-suffix)
     )
    
    (suffix-built-in-decl-specifier
     (suffix-built-in-decl-specifier.raw)
     ("template" suffix-built-in-decl-specifier)
     )
    
    (suffix-named-decl-specifier
     (scoped-id)
     (elaborate-type-specifier)
     (suffix-named-decl-specifier decl-specifier-suffix)
     )
    
    (suffix-named-decl-specifier.bi
     (suffix-named-decl-specifier)
     (suffix-named-decl-specifier suffix-built-in-decl-specifier.raw)
     )
    
    (suffix-named-decl-specifiers
     (suffix-named-decl-specifier.bi)
     (suffix-named-decl-specifiers suffix-named-decl-specifier.bi)
     )
    
    (suffix-named-decl-specifiers.sf
     (scoped-special-function-id) ;; operators etc
     (suffix-named-decl-specifiers)
     (suffix-named-decl-specifiers scoped-special-function-id)
     )
    
    (suffix-decl-specified-ids
     (suffix-built-in-decl-specifier)
     (suffix-built-in-decl-specifier suffix-named-decl-specifiers.sf)
     (suffix-named-decl-specifiers.sf)
     )
    
    (suffix-decl-specified-scope
     (suffix-named-decl-specifiers "::")
     (suffix-built-in-decl-specifier suffix-named-decl-specifiers "::")
     (suffix-built-in-decl-specifier "::")
     )

    (decl-specifier-affix
     (storage-class-specifier)
     (function-specifier)
     ("friend")
     ("typedef")
     (cv-qualifier)
     )

    (decl-specifier-suffix
     (decl-specifier-affix)
     )

    (decl-specifier-prefix
     (decl-specifier-affix)
     ("template" decl-specifier-prefix)
     )

    (storage-class-specifier
     ("register") ("static") ("mutable")
     ("extern" ($prec 'shift-there)) ;; Prefer linkage specification
     ("auto")
     )

    (function-specifier
     ("explicit")
     ("inline")
     ("virtual")
     )

    (type-specifier
     (simple-type-specifier)
     (elaborate-type-specifier)
     (cv-qualifier)
     )

    (elaborate-type-specifier
     (class-specifier)
     (enum-specifier)
     (elaborated-type-specifier)
     ("template" elaborate-type-specifier)
     )
    
    (simple-type-specifier
     (scoped-id)
     (built-in-type-specifier)
     )
    
    (built-in-type-specifier
     ("char") ("wchar_t") ("bool") ("short") ("int") ("long") ("signed")
     ("unsigned") ("float") ("double") ("void")
     )

    #|
    The over-general use of declaration-expression to cover 
    opt-decl-specifier-seq declarator in a function-definition means that
    class X { };
    could be a function-definition or a class-specifier.
    enum X { };
    could be a function-definition or an enum-specifier.
    The function-definition is not syntactically valid so resolving the 
    false conflict in favour of the elaborated-type-specifier is correct.
    |#
    (elaborated-type-specifier
     (elaborated-class-specifier)
     (elaborated-enum-specifier)
     ("typename" scoped-id)
     )

    (elaborated-enum-specifier
     ("enum" scoped-id ($prec 'shift-there))
     )
    
    (enum-specifier
     ("enum" scoped-id enumerator-clause)
     ("enum" enumerator-clause)
     )
    
    (enumerator-clause
     ("{" enumerator-list-ecarb)
     ("{" enumerator-list enumerator-list-ecarb)
     ("{" enumerator-list "," enumerator-definition-ecarb)
     )
    
    (enumerator-list-ecarb
     ("}")
     (bang $error "}" ($$ (UNBANG "Bad enumerator-list.")))
     )
    
    (enumerator-definition-ecarb
     ("}")
     (bang $error "}" ($$ (UNBANG "Bad enumerator-definition.")))
     )
    
    (enumerator-definition-filler
     ($empty)
     (bang $error "," ($$ (UNBANG "Bad enumerator-definition.")))
     )
    
    (enumerator-list-head
     (enumerator-definition-filler)
     (enumerator-list "," enumerator-definition-filler)
     )
    
    (enumerator-list (enumerator-list-head enumerator-definition))

    (enumerator-definition
     (enumerator)
     (enumerator "=" constant-expression)
     )
    
    (enumerator (identifier))

    (namespace-definition
     ("namespace" scoped-id compound-declaration)
     ("namespace" compound-declaration)
     )
    
    (namespace-alias-definition
     ("namespace" scoped-id "=" scoped-id ";")
     )

    (using-declaration
     ("using" declarator-id ";")
     ("using" "typename" declarator-id ";")
     )

    (using-directive ("using" "namespace" scoped-id ";"))
    (asm-definition ("asm" "(" string ")" ";"))

    (linkage-specification
     ("extern" string looping-declaration)
     ("extern" string compound-declaration)
     )

    ;; A.7 Declarators
    ;; init-declarator is named init-declaration to reflect the embedded
    ;; opt-decl-specifier-seq
    (init-declarations
     (assignment-expression "," init-declaration)
     (init-declarations "," init-declaration)
     )
    (init-declaration
     (assignment-expression)
     ;;(assignment-expression "=" initializer-clause)
     ;;^-- covered by assignment-expression:
     ;;(assignment-expression "(" expression-list ")")
     ;;^-- covered by another set of call arguments:
     ;;(declarator ...
     ;;^-- covered by assignment-expression:
     ;;(direct-declarator ... 
     ;;^-- covered by postfix-expression:
     )

    (star-ptr-operator
     ("*")
     (star-ptr-operator cv-qualifier)
     )
    (nested-ptr-operator
     (star-ptr-operator)
     (id-scope nested-ptr-operator)
     )
    (ptr-operator
     ("&")
     (nested-ptr-operator)
     (global-scope nested-ptr-operator)
     )
    (ptr-operator-seq
     (ptr-operator)
     (ptr-operator ptr-operator-seq)
     )

    ;; Independently coded to localise the shift-reduce conflict:
    ;;    sharing just needs another %prec
    (opt-ptr-operator-seq
     ($empty ($prec 'shift-there)) ;; Maximise type length
     (ptr-operator opt-ptr-operator-seq)
     )

    (opt-cv-qualifier-seq
     ($empty)
     (opt-cv-qualifier-seq cv-qualifier)
     )
    
    (cv-qualifier ("const") ("volatile"))

    ;;type-id -- also covered by parameter declaration
    (type-id
     (type-specifier opt-abstract-declarator)
     (type-specifier type-id)
     )

    ;; abstract-declarator: -- also covered by parameter declaration
    (opt-abstract-declarator
     ($empty)
     (ptr-operator opt-abstract-declarator)
     (direct-abstract-declarator)
     )
    (opt-direct-abstract-declarator
     ($empty)
     (direct-abstract-declarator)
     )
    (direct-abstract-declarator
     (opt-direct-abstract-declarator parenthesis-clause)
     (opt-direct-abstract-declarator "[" "]")
     (opt-direct-abstract-declarator "[" constant-expression "]")
     ;; ("(" abstract-declarator ")") -- covered by parenthesis-clause
     )

    (parenthesis-clause
     (parameters-clause opt-cv-qualifier-seq)
     (parameters-clause opt-cv-qualifier-seq exception-specification)
     )

    (parameters-clause
     ("(" parameter-declaration-clause ")")
     )
    
    ;; parameter-declaration-clause also covers init-declaration, type-id,
    ;;    declarator and abstract-declarator.
    (parameter-declaration-clause
     ($empty)
     (parameter-declaration-list)
     (parameter-declaration-list "...")
     )
    
    (parameter-declaration-list
     (parameter-declaration)
     (parameter-declaration-list "," parameter-declaration)
     )

    ;; A typed abstract qualifier such as
    ;;     Class * ...
    ;; looks like a multiply, so pointers are parsed as their binary
    ;; operation equivalents that ultimately terminate with a degenerate
    ;; right hand term.
    (abstract-pointer-declaration
     (ptr-operator-seq)
     (multiplicative-expression star-ptr-operator opt-ptr-operator-seq)
     )
    (abstract-parameter-declaration
     (abstract-pointer-declaration)
     (and-expression "&")
     (and-expression "&" abstract-pointer-declaration)
     )
    (special-parameter-declaration
     (abstract-parameter-declaration)
     (abstract-parameter-declaration "=" assignment-expression)
     ("...")
     )
    (parameter-declaration
     (assignment-expression)
     (special-parameter-declaration)
     (decl-specifier-prefix parameter-declaration)
     )

    ;; The grammar is repeated for use within template <>
    (templated-parameter-declaration
     (templated-assignment-expression)
     (templated-abstract-declaration)
     (templated-abstract-declaration "=" templated-assignment-expression)
     (decl-specifier-prefix templated-parameter-declaration)
     )
    (templated-abstract-declaration
     (abstract-pointer-declaration)
     (templated-and-expression "&")
     (templated-and-expression "&" abstract-pointer-declaration)
     )

    ;; function-definition includes constructor, destructor, implicit int
    ;; definitions too.  A local destructor is successfully parsed as a
    ;; function-declaration but the ~ was treated as a unary operator.
    ;; constructor-head is the prefix ambiguity between a constructor
    ;; and a member-init-list starting with a bit-field.
    (function-definition
     (ctor-definition)
     (func-definition)
     )
    (func-definition
     (assignment-expression function-try-block)
     (assignment-expression function-body)
     (decl-specifier-prefix func-definition)
     )
    (ctor-definition
     (constructor-head function-try-block)
     (constructor-head function-body)
     (decl-specifier-prefix ctor-definition)
     )
    (constructor-head
     (bit-field-init-declaration)
     (constructor-head "," assignment-expression)
     )
    (function-try-block ("try" function-block handler-seq))
    (function-block (opt-ctor-initializer function-body))
    (function-body (compound-statement))

    ;; An = initializer looks like an extended assignment-expression.
    ;; An () initializer looks like a function call.
    ;; initializer is therefore flattened into its generalised customers.
    ;; (initializer ("=" initializer-clause) ;; -- flattened into caller
    ;; ("(" expression-list ")") ;; -- flattened into caller 
    (initializer-clause
     (assignment-expression)
     (braced-initializer)
     )
    (braced-initializer
     ("{" initializer-list "}")
     ("{" initializer-list "," "}")
     ("{" "}")
     ("{" looping-initializer-clause "#" bang $error "}"
      ($$ (UNBANG "Bad initializer-clause.")))
     ("{" initializer-list "," looping-initializer-clause "#" bang $error "}"
      ($$ (UNBANG "Bad initializer-clause.")))
     )
    (initializer-list
     (looping-initializer-clause)
     (initializer-list "," looping-initializer-clause)
     )
    (looping-initializer-clause
     (start-search looped-initializer-clause ($$ (end-search)))
     )
    (looped-initializer-clause
     (initializer-clause)
     (advance-search "+" looped-initializer-clause)
     (advance-search "-")
     )

    ;; A.8 Classes
    ;; An anonymous bit-field declaration may look very like inheritance:
    ;;     const int B = 3;
    ;;     class A : B ;
    ;; The two usages are too distant to try to create and enforce a common
    ;; prefix so we have to resort to a parser hack by backtracking.
    ;; Inheritance is much the most likely so we mark the input stream context
    ;; and try to parse a base-clause. If we successfully reach a { the base-
    ;; clause is ok and inheritance was the correct choice so we unmark
    ;; and continue. If we fail to find the { an error token causes
    ;; back-tracking to the alternative parse in elaborated-type-specifier
    ;; which regenerates the : and declares unconditional success.
    (colon-mark
     (":" ($$ (mark)))
     )
    (elaborated-class-specifier
     (class-key scoped-id ($prec 'shift-there))
     (class-key scoped-id colon-mark $error ($$ (rewind-colon)))
     )
    (class-specifier-head
     (class-key scoped-id colon-mark base-specifier-list "{" ($$ (unmark)))
     (class-key ":" base-specifier-list "{")
     (class-key scoped-id "{")
     (class-key "{")
     )
    (class-key ("class") ("struct") ("union"))
    (class-specifier
     (class-specifier-head opt-member-specification "}")
     (class-specifier-head opt-member-specification util
			   looping-member-declaration "#" bang $error "}"
			   ($$ (UNBANG "Bad opt-member-specification.")))
     )
    (opt-member-specification
     ($empty)
     (opt-member-specification util looping-member-declaration)
     (opt-member-specification util looping-member-declaration
			       "#" bang $error ";"
			       ($$ (UNBANG "Bad member-declaration.")))
     )
    (looping-member-declaration
     (start-search looped-member-declaration ($$ (end-search)))
     )
    (looped-member-declaration
     (member-declaration)
     (advance-search "+" looped-member-declaration)
     (advance-search "-")
     )
    (member-declaration
     (accessibility-specifier)
     (simple-member-declaration)
     (function-definition)
     ;;(function-definition ";") <- covered by null declaration
     ;;(qualified-id ";") <- covered by simple-member-declaration
     (using-declaration)
     (template-declaration)
     )

    ;; The generality of constructor names (there need be no parenthesised
    ;; argument list) means that that
    ;;          name : f(g), h(i)
    ;; could be the start of a constructor or the start of an anonymous
    ;; bit-field. An ambiguity is avoided by parsing the ctor-initializer
    ;; of a function-definition as a bit-field.
    (simple-member-declaration
     (";")
     (assignment-expression ";")
     (constructor-head ";")
     (member-init-declarations ";")
     (decl-specifier-prefix simple-member-declaration)
     )
    (member-init-declarations
     (assignment-expression "," member-init-declaration)
     (constructor-head "," bit-field-init-declaration)
     (member-init-declarations "," member-init-declaration)
     )
    (member-init-declaration
     (assignment-expression)
     ;;(assignment-expression "=" initializer-clause)
     ;; ^-- covered by assignment-expression 
     ;;(assignment-expression "(" expression-list ")")
     ;; ^-- covered by another set of call arguments 
     (bit-field-init-declaration)
     )
    (accessibility-specifier
     (access-specifier ":")
     )
    (bit-field-declaration
     (assignment-expression ":" bit-field-width)
     (":" bit-field-width)
     )
    (bit-field-width
     (logical-or-expression)
     ;;(logical-or-expression "?" expression ":" assignment-expression)
     ;; ^^ -- has SR conflict w.r.t later =
     (logical-or-expression "?" bit-field-width ":" bit-field-width)
     )
    (bit-field-init-declaration
     (bit-field-declaration)
     (bit-field-declaration "=" initializer-clause)
     )

    ;; A.9 Derived classes
    ;; (base-clause (":" base-specifier-list) ;; -- flattened
    (base-specifier-list
     (base-specifier)
     (base-specifier-list "," base-specifier)
     )
    (base-specifier
     (scoped-id)
     (access-specifier base-specifier)
     ("virtual" base-specifier)
     )
    (access-specifier ("private") ("protected") ("public"))

    ;; A.10 Special member functions
    (conversion-function-id ("operator" conversion-type-id))
    (conversion-type-id
     (type-specifier opt-ptr-operator-seq)
     (type-specifier conversion-type-id)
     )

    ;; Ctor-initialisers can look like a bit field declaration, given the
    ;; generalisation of names:
    ;;     Class(Type) : m1(1), m2(2) { }
    ;;     NonClass(bit-field) : int(2), second-variable, ...
    ;; The grammar below is used within a function-try-block or
    ;; function-definition. See simple-member-declaration for use in
    ;; normal member function-definition.
    (opt-ctor-initializer
     ($empty)
     (ctor-initializer)
     )
    (ctor-initializer
     (":" mem-initializer-list)
     (":" mem-initializer-list bang $error
      ($$ (UNBANG "Bad ctor-initializer.")))
     )
    (mem-initializer-list
     (mem-initializer)
     (mem-initializer-list-head mem-initializer)
     )
    (mem-initializer-list-head
     (mem-initializer-list ",")
     (mem-initializer-list bang $error ","
			   ($$ (UNBANG "Bad mem-initializer.")))
     )
    (mem-initializer
     (mem-initializer-id "(" opt-expression-list ")")
     )
    (mem-initializer-id
     (scoped-id)
     )

    ;; A.11 Overloading
    (operator-function-id ("operator" operator))

    ;; It is not clear from the ANSI standard whether spaces are permitted in
    ;; delete[]. If not then it can be recognised and returned as "delete[]".
    ;; by the lexer. Assuming spaces are permitted there is an ambiguity
    ;; created by the over generalised nature of expressions. operator new is
    ;; a valid delarator-id which we may have an undimensioned array of.
    ;; Semantic rubbish, but syntactically valid. Since the array form is
    ;; covered by the declarator consideration we can exclude the operator
    ;; here. The need for a semantic rescue can be eliminated at the expense
    ;; of a couple of shift-reduce conflicts by removing the comments on the
    ;; next four lines.
    (operator
     (#|++++|# "new") 
     (#|++++|# "delete")
     ;;(#|----|# "new" %prec "SHIFT_THERE")
     ;;(#|----|# "delete" %prec "SHIFT_THERE")
     ;;(#|----|# "new" "[" "]") ;; -- Covered by array of "operator" "new"
     ;;(#|----|# "delete" "[" "]") ;; 
     ;;^-- Covered by array of "operator" "delete"
     ("+" ($$ `(add ,$1)))
     ("-" ($$ `(sub ,$1)))
     ("*" ($$ `(mul ,$1)))
     ("/" ($$ `(div ,$1)))
     ("%" ($$ `(mod ,$1)))
     ("^" ($$ `(bw-xor ,$1)))
     ("&" ($$ `(bw-and ,$1)))
     ("|" ($$ `(bw-or ,$1)))
     ("~" ($$ `(bw-not ,$1)))
     ("!" ($$ `(not ,$1)))
     ("=" ($$ `(assn ,$1)))
     ("<" ($$ `(lt ,$1)))
     (">" ($$ `(gt ,$1)))
     ("+=" ($$ `(add-assn ,$1)))
     ("-=" ($$ `(sub-assn ,$1)))
     ("*=" ($$ `(mul-assn ,$1)))
     ("/=" ($$ `(div-assn ,$1)))
     ("%=" ($$ `(mod-assn ,$1)))
     ("^=" ($$ `(xor-assn ,$1)))
     ("&=" ($$ `(and-assn ,$1)))
     ("|=" ($$ `(or-assn ,$1)))
     ("<<" ($$ `(shl ,$1)))
     (">>" ($$ `(shr ,$1)))
     (">>=" ($$ `(rshift-assn ,$1)))
     ("<<=" ($$ `(lshift-assn ,$1)))
     ("==" ($$ `(eq ,$1)))
     ("!=" ($$ `(ne ,$1)))
     ("<=" ($$ `(le ,$1)))
     (">=" ($$ `(ge ,$1)))
     ("&&" ($$ `(and ,$1)))
     ("||" ($$ `(or ,$1)))
     ("++" ($$ `(inc ,$1)))
     ("--" ($$ `(dec ,$1)))
     ("," ($$ `(comma ,$1)))
     ("->*" ($$ `(arrow-star ,$1)))
     ("->" ($$ `(i-sel ,$1)))
     ("(" ")" ($$ `(todo1 ,$1)))
     ("[" "]" ($$ `(todo2 ,$1)))
     )

    ;; A.12 Templates
    (template-declaration
     (template-parameter-clause declaration)
     ("export" template-declaration)
     )
    (template-parameter-clause
     ("template" "<" template-parameter-list ">")
     )
    (template-parameter-list
     (template-parameter)
     (template-parameter-list "," template-parameter)
     )
    (template-parameter
     (simple-type-parameter)
     (simple-type-parameter "=" type-id)
     (templated-type-parameter)
     (templated-type-parameter "=" identifier)
     (templated-parameter-declaration)
     (bang $error ($$ (UNBANG "Bad template-parameter.")))
     )
    (simple-type-parameter
     ("class")
     ;;("class" identifier) <- covered by parameter-declaration
     ("typename")
     ;;("typename" identifier) <- covered by parameter-declaration
     )
    (templated-type-parameter
     (template-parameter-clause "class")
     (template-parameter-clause "class" identifier)
     )
    (template-id
     ("template" identifier "<" template-argument-list ">")
     ("template" template-id)
     )
    ;; template-argument is evaluated using a templated...expression so
    ;; that > resolves to end of template.
    (template-argument-list
     (template-argument)
     (template-argument-list "," template-argument)
     )
    (template-argument
     (templated-parameter-declaration)
     ;;(type-id) <- covered by templated-parameter-declaration
     ;;(template-name) <- covered by templated-parameter-declaration
     ;;(error) <- must allow template failure to re-search
     )
    
    ;; Generalised naming makes identifier a valid declaration, so "template"
    ;; identifier is too. The "template" prefix is therefore folded into all
    ;; names, parenthesis-clause and decl-specifier-prefix.
    ;; (explicit-instantiation ("template" declaration))
    (explicit-specialization ("template" "<" ">" declaration))

    ;; A.13 Exception Handling
    (try-block
     ("try" compound-statement handler-seq)
     )
    ;; (function-try-block ) ;; -- moved near function-block
    (handler-seq
     (handler)
     (handler handler-seq)
     )
    (handler
     ("catch" "(" exception-declaration ")" compound-statement)
     )
    (exception-declaration
     (parameter-declaration)
     ;;("...") ;; -- covered by parameter-declaration
     )
    (throw-expression
     ("throw")
     ("throw" assignment-expression)
     )
    (templated-throw-expression
     ("throw")
     ("throw" templated-assignment-expression)
     )
    (exception-specification
     ("throw" "(" ")")
     ("throw" "(" type-id-list ")")
     )
    (type-id-list
     (type-id)
     (type-id-list "," type-id)
     )
    (advance-search
     ($error ($$ (yyerrok) (yyclearin) (error) (advance-search))))
    (bang ($empty ($$ (BANG))))
    (mark ($empty ($$ (mark))))
    (nest ($empty ($$ (nest))))
    (start-search ($empty ($$ (start-search #f))))
    (start-search1 ($empty ($$ (start-search #t))))
    (util ($empty))
    )
   #|
   %term <character-literal> CharacterLiteral
   %term <floating-literal> FloatingLiteral
   %term <identifier> Identifier
   %term <integer-literal> IntegerLiteral
   %term <number-literal> NumberLiteral
   %term <string-literal> StringLiteral
   * The lexer need not treat "0" as distinct from IntegerLiteral in the hope 
   * that pure-specifier can be distinguished, It isn"t. Semantic rescue from
   *  = constant-expression is necessary.
   *
   * The lexer is not required to distinguish template or type names, although
   * a slight simplification to the grammar and elaboration of the action rules
   * could make good use of template name information.
   *
   * In return for not needing to use semantic information, the lexer must 
   * support back-tracking, which is easily achieved by a simple linear buffer,
   * a reference implementation of which may be found in the accompanying 
   * CxxParsing.cxx. Back-tracking is used to support:
   *
   * Binary search for a consistent parse of the template/arithmetic ambiguity.
   *   start-search() initialises the search
   *   advance-search() iterates the search
   *   end-search() cleans up after a search
   *   template-test() maintains context during a search
   *
   * Lookahead to resolve the inheritance/anonymous bit-field similarity
   *   mark() saves the starting context
   *   unmark() pops it
   *   rewind-colon() restores the context and forces the missing :
   *
   * Lookahead to resolve type 1 function parameter ambiguities
   *   mark-type1() potentially marks the starting position
   *   mark() marks the pre { position
   *   remark() rewinds to the starting position
   *   unmark() pops the starting position
   *
   * Note that lookaheads may nest. 

   * The parsing philosophy is unusual. The major ambiguities are resolved by 
   * creating a unified superset grammar rather than non-overlapping
   * subgrammars.  Thus the grammar for parameter-declaration covers an
   * assignment-expression.  Minor ambiguities whose resolution by
   * supersetting would create more ambiguities are resolved the normal way
   * with partitioned subgrammars. This eliminates the traditional
   * expression/declaration and constructor/parenthesised declarator
   * ambiguities at the syntactic level.  A subsequent semantic level has
   * to sort the problems out. The generality introduces four bogus 
   * ambiguities and defers the cast ambiguity for resolution once semantic
   * information is available.
   *
   * The C++ grammar comprises 558 rules and uses 894 states in yacc, with 0 
   * unresolved conflicts. 24 conflicts from 10 ambiguities are resolved by
   * 8 %prec"s, so that yacc and bison report 0 conflicts.
   *
   * The ambiguities are:
   * 1) dangling else resolved to inner-most if
   *    1 conflict in 1 state on else
   * 2) < as start-template or less-than
   *    1 conflict in 2 states on <
   * 3) a :: b :: c resolved to favour a::b::c rather than a::b ::c or a ::b::c
   *    1 conflicts in 1 state for ::
   * 4) pointer operators maximised at end of conversion id/new in preference
   *    to binary operators
   *    2 conflicts in 4 states on * and &
   * 5a) (a)@b resolved to favour binary a@b rather than cast unary (a)(@b)
   * 5b) (a)(b) resolved to favour cast rather than call
   *    8 conflicts in 1 state for the 8 prefix operators: 6 unaries and 
   *      ( and [.
   * 6) enum name { resolved to enum-specifier rather than function
   *    1 conflict in 1 state on {
   * 7) class name { resolved to class-specifier rather than function
   *    1 conflict in 1 state on {
   * 8) extern "C" resolved to linkage-specification rather than declaration
   *    1 conflict in 1 state on StringLiteral
   * 9) class X : forced to go through base-clause look-ahead
   *    1 conflict in 1 state on :
   * 10) id : forced to label-statement rather than constructor-head
   *    0 conflicts - but causes a double state for 2)
   * of which
   *    1 is a fundamental C conflict - always correctly resolved
   *      can be removed - see the Java spec
   *    2, 3, 4 are fundamental C++ conflicts
   *      2 always consistently resolved by iteration
   *      3 always correctly resolved
   *      4 always correctly resolved
   *    5 is a result of not using type information - deferred for semantic
   *      repair
   *    6,7 are caused by parsing over-generous superset - always correctly
   *        resolved
   *    8 is caused by parsing over-generous superset - always correctly 
   *      resolved
   *        can be removed at the expense of 7 rules and 5 states.
   *    9 is a look-ahead trick - always correctly resolved
   *      could be removed by marking one token sooner
   *   10 is caused by parsing over-generous superset - always correctly
   *      resolved
   *
   * The hard problem of distinguishing
   *   class A { class B : C, D, E {      -- A::B privately inherits C, D and E
   *   class A { class B : C, D, E ;      -- C is width of anon bit-field
   * is resolved by using a lookahead that assumes inheritance and rewinds for
   * the bit-field.
   *
   * The potential shift-reduce conflict on > is resolved by flattening part of
   * the expression grammar to know when the next > is template end or
   * arithmetic >.
   *
   * The grammar is SYNTACTICALLY context-free with respect to type. No
   * semantic assistance is required during syntactic analysis. However 
   * the cast ambiguity is deferred and must be recovered after syntactic
   * analysis of a statement has completed. 
   *
   * The grammar is SYNTACTICALLY context-free with respect to template-names. 
   * This is achieved by organising a binary search over all possible 
   * template/arithmetic ambiguities with respect to the enclosing statement.
   * This is potentially exponentially inefficient but well-behaved in
   * practice.  Approximately 1% of statements trigger a search and 
   * approximately 1% of those are misparsed, requiring the semantic analysis
   * to check and correct once template information is available.
   *  1.5 parse attempts are required on average per ambiguous statement.
   *
   * The grammar supports type I function declarations at severe impediment to
   * efficiency. A lookahead has to be performed after almost every
   * non-statement close parenthesis. A one-line plus corollary change to
   * postfix-expression is commented and strongly recommended to make this
   * grammar as efficient as the rather large number of reduction levels
   * permits.
   *
   * Error recovery occurs mostly at the statement/declaration level. Recovery
   * also occurs at the list-element level where this poses no hazard to
   * statement/declaration level recovery.  Note that since error propagation
   * interacts with the lookaheads for template iteration or type 1 function
   * arguments, introduction of finer grained error recovery may repair a false
   * parse and so cause a misparse.
   *
   * The following syntactic analysis errors occur, but are correctable
   * semantically:
   *  (cast)unary-op expr         is parsed as (parenthesised)binary-op expr
   *      The semantic test should look for a binary/call with a (type) as its
   *      left child.
   *  (parenthesised)(arguments)  is parsed as (cast)(parenthesised)
   *      The semantic test should look for a cast with a non-type as its
   *      left child.
   *  template < and arithmetic < may be cross-parsed (unless semnatic help is
   *  provided)
   *      approximately 0.01% are misparsed, and must be sorted out - not easy.
   *
   *  The syntactic analysis defers the following ambiguities for semantic 
   *  resolution:
   *  declaration/expression is parsed as a unified concept
   *      Use type and context to complete the parse.
   *  ~class-name                 is parsed as unary~ name
   *      The semantic test should look for ~ with a type as its child.
   *  delete[] expr               is parsed as delete []expr
   *      The semantic test should look for delete with a [] cast of its child.
   *  operator new/delete[]       are parsed as array of operator new/delete
   *      The semantic test should look for array of operator new/delete
   *      or activate the two extra commented rules in operator
   *  template of an explicit-instantiation is buried deep in the tree
   *      dig it out 
   *  pure-specifier and constant-initializer are covered by
   *             assignment-expression
   *      just another of the deferred declaration/expression ambiguities
   *  sizeof and typeid don't distinguish type/value syntaxes
   *      probably makes life polymorphically easier
   |#
   ))

(when #t
  (with-output-to-file "lang.txt"
    (lambda ()
      (pp-lalr-grammar cxx-spec)
      )))

;;(simple-format #t "prec=~S\n" (assq-ref cxx-spec 'prec))
;;(simple-format #t "terminals=~S\n" (assq-ref cxx-spec 'terminals))
;;(simple-format #t "non-terms=~S\n" (assq-ref cxx-spec 'non-terms))

(define cxx-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine/bison cxx-spec))))

;; The following are needed by the code in pbody.scm.
(define len-v (assq-ref cxx-mach 'len-v))
(define pat-v (assq-ref cxx-mach 'pat-v))
(define rto-v (assq-ref cxx-mach 'rto-v))
(define mtab (assq-ref cxx-mach 'mtab))
(define act-v (vector-map
               (lambda (ix f) (eval f (current-module)))
               (vector-map (lambda (ix actn) (wrap-action actn))
                           (assq-ref cxx-mach 'act-v))))
;; (include-from-path "nyacc/lang/cxx/body.scm")

;; (define raw-parser (make-lalr-parser cxx-mach))


(define (gen-cxx-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions cxx-mach (xtra-dir "cxxact.scm.new"))
  (write-lalr-tables cxx-mach (xtra-dir "cxxtab.scm.new"))
  (let ((a (move-if-changed (xtra-dir "cxxact.scm.new")
			    (xtra-dir "cxxact.scm")))
	(b (move-if-changed (xtra-dir "cxxtab.scm.new")
			    (xtra-dir "cxxtab.scm"))))
    #f
    #;(when (or a b) 
      (system (string-append "touch " (lang-dir "parser.scm"))))))

(gen-cxx-files)

;; --- last line --- 
