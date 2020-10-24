;; lang/java/mach.scm

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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;; ref: https://docs.oracle.com/javase/specs/jls/se7/html/

(define-module (nyacc lang java mach)
  #:export (java-spec java-mach gen-java-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

;; tough reduce-reduce conflict:
;; in (BlockStatement ...
;;   (Type Identifier ...)
;; vs
;;   (StatmentExpression) [starting with QualifiedIdentier]
;; What makes it worse is that Type can take the form
;;   Identifier "<" ...
;; so there is conflict between type def and expression.
;; See "HACK" in production rules for Type.
;; ...
;; But Identifier "<" ... will never be a statement in reality, so
;; statement expressions can be narrowed, so try that.

(define java-spec
  (lalr-spec
   (start CompliationUnit)
   (prec< 'then "else")
   ;;(prec< 'expr 'stmt)
   (prec< (right "=")
	  (left "||") (left "&&")
	  (left "|") (left "^") (left "&")
	  (left "==" "!=")
	  (left "<" ">" "<=" ">=")
	  (left "<<" ">>")
	  (left "+" "-")
	  (left "*" "/")
	  )
   ;;(expect 1)
   (grammar
    (Identifier
     ($ident ($$ `(Identifier $1))))

    (QualifiedIdentifier
     (QualifiedIdentifier-1 ($$ (tl->list $1))))
    ;; curious that adding this layer generates a shift-reduce conflict
    (QualifiedIdentifier-1
     (Identifier ($$ (make-tl 'QualifiedIdentifier $1)))
     (QualifiedIdentifier-1 "." Identifier ($$ ($tl-append $1 $3))))
    
    (QualifiedIdentifierList
     (QualifiedIdentifierList-1 ($$ (tl->list $1))))
    (QualifiedIdentifierList-1
     (QualifiedIdentifier ($$ (make-tl 'QualifiedIdentifierList $1)))
     (QualifiedIdentifierList-1 "," QualifiedIdentifier ($$ (tl-append $1 $3))))

    (CompliationUnit
     (ImportDeclaration-list TypeDeclaration-list ($$ `(ComplilationUnit $1 $2)))
     ("package" QualifiedIdentifier ";"
      ImportDeclaration-list TypeDeclaration-list
      ($$ `(CompilationUnit (Package ,$2 ,@(cdr $4) ,@(cdr $5)))))
     (Annotations
      "package" QualifiedIdentifier ";"
      ImportDeclaration-list TypeDeclaration-list
      ($$ `(CompilationUnit (Package ,$3 $1 ,@(cdr $5) ,@(cdr $6))))))

    (ImportDeclaration-list
     (ImportDeclaration-list-1 ($$ (tl->list $1))))
    (ImportDeclaration-list-1
     ($empty ($$ (make-tl 'ImportDeclaration-list)))
     (ImportDeclaration-list-1 ImportDeclaration ($$ (tl-append $1 $2))))
    (TypeDeclaration-list
     (TypeDeclaration-list-1 ($$ (tl->list $1))))
    (TypeDeclaration-list-1
     ($empty ($$ (make-tl 'TypeDeclaration-list)))
     (TypeDeclaration-list-1 TypeDeclaration ($$ (tl-append $1 $2))))
    
    (ImportDeclaration
     ("import" "static" QualifiedIdentifier "." "*" ";"
      ($$ `(ImportDeclaration (Modifier "static") ,(append $3 '(glob "*")))))
     ("import" "static" QualifiedIdentifier ";"
      ($$ `(ImportDeclaration (Modifier "static") ,$3)))
     ("import" QualifiedIdentifier "." "*" ";"
      ($$ `(ImportDeclaration ,(append $3 '(glob "*")))))
     ("import" QualifiedIdentifier ";" ($$ `(ImportDeclaration ,$3))))

    (TypeDeclaration
     (Modifier-list
      ClassOrInterfaceDeclaration ";"
      ($$ (sx-cons* (sx-tag $1) (sx-attr $1) (append $1 (sx-tail $2)))))
     (ClassOrInterfaceDeclaration ";"))

    (ClassOrInterfaceDeclaration 
     (ClassDeclaration)
     (InterfaceDeclaration))
    
    (ClassDeclaration
     (NormalClassDeclaration)
     (EnumDeclaration))

    (InterfaceDeclaration
     (NormalInterfaceDeclaration)
     ;;(AnnotationTypeDeclaration) ;; CONFLICTs
     )

    (NormalClassDeclaration
     ("class" Identifier
      opt-TypeParameters opt-extends opt-implements
      ClassBody
      ($$ (let* ((tail (list $6))
		 (tail (if (pair? $5) (cons $5 tail) tail))
		 (tail (if (pair? $4) (cons $4 tail) tail))
		 (tail (if (pair? $3) (cons $3 tail) tail)))
	    `(NormalClassDeclaration ,$1 ,@tail)))))

    (opt-TypeParameters
     ($empty)
     (TypeParameters))
    (opt-extends
     ($empty)
     ("extends" Type ($$ `(extends ,$2))))
    (opt-implements
     ($empty)
     ("implements" TypeList ($$ `(implements ,$2))))

    (EnumDeclaration
     ("enum" Identifier EnumBody
      ($$ `(EnumDeclaration ,$2 ,$3)))
     ("enum" Identifier "implements" TypeList EnumBody
      ($$ `(EnumDeclaration ,$2 (implements ,$4) ,$5))))

    (NormalInterfaceDeclaration
     ("interface" Identifier opt-TypeParameters "extends" TypeList InterfaceBody
      ($$ (if (pair? $3)
	      `(NormalInterfaceDeclaration ,$2 ,$3 (extends ,$5) ,$6)
	      `(NormalInterfaceDeclaration ,$2 (extends ,$5) ,$6))))
     ("interface" Identifier opt-TypeParameters InterfaceBody
      ($$ (if (pair? $3)
	      `(NormalInterfaceDeclaration ,$2 ,$3 ,$4)
	      `(NormalInterfaceDeclaration ,$2 ,$4)))))

    (Type
     (BasicType)
     (BasicType arrays ($$ (append $1 (list $2))))
     ;; HACK:
     ;; Replace ReferenceType with QualifiedIdentifier to eliminate
     ;; reduce-reduce conflict for decl's w/ parameterized types.
     #|
     (ReferenceType)
     (ReferenceType arrays)
     |#
     (QualifiedIdentifier)
     (QualifiedIdentifier arrays ($$ (append $1 (list $2))))
     )

    (opt-arrays
     ($empty)
     (arrays))
    (arrays
     (arrays-1 ($$ (tl->list $1))))
    (arrays-1
     ("[" "]" ($$ (make-tl 'arrays (array "[]"))))
     (arrays-1 "[" "]" ($$ (tl-append $1 (array "[]")))))

    (BasicType
     (BasicType-1 ($$ `(BasicType ,$1))))
    (BasicType-1
     ("byte")
     ("short")
     ("char")
     ("int")
     ("long")
     ("float")
     ("double")
     ("boolean"))

    (ReferenceType
     (ReferenceType-1 ($$ (tl->list $1))))
    (ReferenceType-1
     (Identifier ($$ (make-tl 'ReferenceType $1)))
     (Identifier TypeArguments ($$ (make-tl 'ReferenceType $1 $2)))
     (ReferenceType-1 "." Identifier ($$ (tl-append $1 $3)))
     (ReferenceType-1 "." Identifier TypeArguments ($$ (tl-append $1 $3))))

    ;; fix: shift-reduce on "<"
    (TypeArguments
     ("<" TypeArgument-list ">"))
    (TypeArgument-list
     (TypeArgument)
     (TypeArgument-list "," TypeArgument))

    (TypeArgument
     (ReferenceType "?")
     (ReferenceType "?" "extends" ReferenceType)
     (ReferenceType "?" "super" ReferenceType))

    (NonWildcardTypeArguments
     ("<" TypeList ">"))

    (TypeList
     (ReferenceType)
     (TypeList "," ReferenceType))

    (TypeArgumentsOrDiamond
     ("<" ">")
     (TypeArguments))

    (NonWildcardTypeArgumentsOrDiamond
     ("<" ">")
     (NonWildcardTypeArguments))

    ;; "<" shift-reduce conflict: reduce Expression2 looking at "<"
    (TypeParameters
     ("<" TypeParameter-list ">"))
    (TypeParameter-list
     (TypeParameter)
     (TypeParameter-list "," TypeParameter))

    (TypeParameter
     (Identifier)
     (Identifier "extends" Bound)
     )

    (Bound
     (ReferenceType-list))
    (ReferenceType-list
     (ReferenceType)
     (ReferenceType-list "&" ReferenceType))

    (Modifier
     ("public")
     ("protected")
     ("private")
     ("static ")
     ("abstract")
     ("final")
     ("native")
     ("synchronized")
     ("transient")
     ("volatile")
     ("strictfp"))

    (ElementValuePairs
     (ElementValuePair)
     (ElementValuePairs "," ElementValuePair))

    (ElementValuePair
     (Identifier "=" ElementValue))
    
    (ElementValue
     ;;(Annotation)
     (Expression1)
     (ElementValueArrayInitializer))

    (ElementValueArrayInitializer
     ("{" "}")
     ("{" ElementValues "}")
     ("{" ElementValues "," "}")
     )

    (ElementValues
     (ElementValue)
     (ElementValues "," ElementValue))

    (ClassBody
     ("{" ClassBodyDeclaration-list "}"))
    (ClassBodyDeclaration-list
     (ClassBodyDeclaration)
     (ClassBodyDeclaration-list ClassBodyDeclaration))

    (ClassBodyDeclaration
     (";")
     (MemberDecl)
     (Modifier MemberDecl)
     ("static" Block)
     (Block))

    ;; Potential conflict on "<" here between Type and
    ;; TypeParameters (within GenericMethodOrConstructorDecl)
    (MemberDecl
     (Type Identifier FieldDeclaratorsRest ";")
     (Type Identifier MethodDeclaratorRest)
     ("void" Identifier VoidMethodDeclaratorRest)
     (Identifier ConstructorDeclaratorRest)
     (GenericMethodOrConstructorDecl)
     (ClassDeclaration)
     (InterfaceDeclaration)
     )

    (FieldDeclaratorsRest
     (VariableDeclaratorRest)
     (FieldDeclaratorsRest "," VariableDeclarator))

    (MethodDeclaratorRest
     (FormalParameters opt-arrays Block)
     (FormalParameters opt-arrays "throws" QualifiedIdentifierList Block)
     (FormalParameters opt-arrays ";")
     (FormalParameters opt-arrays "throws" QualifiedIdentifierList ";")
     )

    (VoidMethodDeclaratorRest
     (FormalParameters Block)
     (FormalParameters "throws" QualifiedIdentifierList Block)
     (FormalParameters ";")
     (FormalParameters "throws" QualifiedIdentifierList ";")
     )

    (ConstructorDeclaratorRest
     (FormalParameters Block)
     (FormalParameters "throws" QualifiedIdentifierList Block)
     )

    (GenericMethodOrConstructorDecl
     (TypeParameters GenericMethodOrConstructorRest))

    (GenericMethodOrConstructorRest
     (Type Identifier MethodDeclaratorRest)
     ("void" Identifier MethodDeclaratorRest)
     (Identifier ConstructorDeclaratorRest)
     )

    (InterfaceBody
     ("{" InterfaceBodyDeclaration-list "}")
     ("{" "}")
     )
    (InterfaceBodyDeclaration-list
     (InterfaceBodyDeclaration)
     (InterfaceBodyDeclaration-list InterfaceBodyDeclaration))

    (InterfaceBodyDeclaration
     (";")
     (Modifier-list InterfaceMemberDecl)
     (InterfaceMemberDecl)
     )
    (Modifier-list
     (Modifier)
     (Modifier-list Modifier))
     
    (InterfaceMemberDecl
     (InterfaceMethodOrFieldDecl)
     ("void" Identifier VoidInterfaceMethodDeclaratorRest)
     (InterfaceGenericMethodDecl)
     (ClassDeclaration)
     (InterfaceDeclaration))

    (InterfaceMethodOrFieldDecl
     (Type Identifier InterfaceMethodOrFieldRest))

    (InterfaceMethodOrFieldRest
     (ConstantDeclaratorsRest ";")
     (InterfaceMethodDeclaratorRest))

    (ConstantDeclaratorsRest
     (ConstantDeclaratorRest)
     (ConstantDeclaratorsRest "," ConstantDeclarator))

    (ConstantDeclaratorRest
     ("=" VariableInitializer)
     (arrays "=" VariableInitializer))

    (ConstantDeclarator
     (Identifier ConstantDeclaratorRest))

    (InterfaceMethodDeclaratorRest
     (FormalParameters opt-arrays ";")
     (FormalParameters opt-arrays "throws" QualifiedIdentifierList ";")
     )

    (VoidInterfaceMethodDeclaratorRest
     (FormalParameters ";")
     (FormalParameters "throws" QualifiedIdentifierList ";")
     )

    (InterfaceGenericMethodDecl
     (TypeParameters Type Identifier InterfaceMethodDeclaratorRest)
     (TypeParameters "void" Identifier InterfaceMethodDeclaratorRest))

    (FormalParameters
     ("(" FormalParameterDecls ")")
     ("(" ")"))

    (FormalParameterDecls
     (Type FormalParameterDeclsRest))

    (FormalParameterDeclsRest
     (VariableDeclaratorId)
     (VariableDeclaratorId "," FormalParameterDecls)
     ("..." VariableDeclaratorId))

    (VariableDeclaratorId
     (Identifier)
     (Identifier arrays)
     )

    (VariableDeclarators
     (VariableDeclarator)
     (VariableDeclarators "," VariableDeclarator))

    (VariableDeclarators-tail
     ("," VariableDeclarator)
     (VariableDeclarators-tail "," VariableDeclarator))

    (VariableDeclarator
     (Identifier VariableDeclaratorRest))

    (VariableDeclaratorRest
     ($empty)
     ("=" VariableInitializer)
     (arrays)
     (arrays "=" VariableInitializer))

    (VariableInitializer
     (ArrayInitializer)
     (Expression))

    (ArrayInitializer
     ("{" VariableInitializer-list "}")
     ("{" VariableInitializer-list "," "}")
     ("{" "}"))
    
    (VariableInitializer-list
     (VariableInitializer)
     (VariableInitializer-list "," VariableInitializer))

    (Block
     ("{" BlockStatements "}")
     ("{" "}"))

    (BlockStatements
     (BlockStatement)
     (BlockStatements BlockStatement))

    ;; TODO: add opt-Annotations to front of "final" items
    (BlockStatement
     (Type VariableDeclarators ";")
     ("final" Type VariableDeclarators ";")
     (ClassOrInterfaceDeclaration)
     (Modifier-list ClassOrInterfaceDeclaration)
     (Statement)
     )

    (Statement
     (Block)
     (";")
     (Identifier ":" Statement)
     (StatementExpression ";")
     ("if" "(" Expression ")" Statement ($prec 'then))
     ("if" "(" Expression ")" Statement "else" Statement)
     ("assert" Expression ";")
     ("assert" Expression ":" Expression ";")
     ("switch" "(" Expression ")" "{" SwitchBlockStatementGroups "}" )
     ("while" "(" Expression ")" Statement)
     ("do" Statement "while" "(" Expression ")" ";")
     ("for" "(" ForControl ")" Statement)
     ("break" ";")
     ("break" Identifier ";")
     ("continue" ";")
     ("continue" Identifier ";")
     ("return" Expression ";")
     ("return" ";")
     ("throw" Expression ";")
     ("synchronized" "(" Expression ")" Block)
     ("try" Block Catches Finally)
     ("try" Block Catches)
     ("try" Block Finally)
     ("try" ResourceSpecification Block Catches Finally)
     ("try" ResourceSpecification Block Catches)
     ("try" ResourceSpecification Block Finally)
     )

    (StatementExpression
     #|
     (ReferenceType Arguments)
     (ReferenceType IdentifierSuffix Arguments)
     (ReferenceType AssignmentOperator Expression1)
     (ReferenceType IdentifierSuffix AssignmentOperator Expression1)
     (ReferenceType "++")
     (ReferenceType "--")
     |#
     (QualifiedIdentifier Arguments)
     (QualifiedIdentifier IdentifierSuffix Arguments)
     (QualifiedIdentifier AssignmentOperator Expression1)
     (QualifiedIdentifier IdentifierSuffix AssignmentOperator Expression1)
     (QualifiedIdentifier "++")
     (QualifiedIdentifier "--")
     ;; ^ may need to sub ReferenceType and sub back later via ...
     ;;  `(QualifiedIdentifier ,(cdr $1))

     ("++" QualifiedIdentifier)
     ("--" QualifiedIdentifier)
     )

    (Catches
     (CatchClause)
     (Catches CatchClause))

    ;; check
    (CatchClause
     ("catch" "(" CatchType Identifier ")" Block))

    (CatchType
     (QualifiedIdentifier)
     (CatchType "|" QualifiedIdentifier))

    (Finally
     ("finally" Block))

    (ResourceSpecification
     ("(" Resources ")")
     ("(" Resources ";" ")"))
    
    (Resources
     (Resource)
     (Resources ";" Resource))

    (Resource
     ;; first form missing opt-Annotations
     ("final" ReferenceType VariableDeclaratorId "=" Expression)
     (ReferenceType VariableDeclaratorId "=" Expression))

    (SwitchBlockStatementGroups
     (SwitchBlockStatementGroup)
     (SwitchBlockStatementGroups SwitchBlockStatementGroup))

    (SwitchBlockStatementGroup
     (SwitchLabels BlockStatements))

    (SwitchLabels
     (SwitchLabel)
     (SwitchLabels SwitchLabel))

    (SwitchLabel
     ("case" Expression ":")
     ;;("case" EnumConstantName ":") CONFLICT
     ("default" ":"))

    #;(EnumConstantName
     (Identifier))

    (ForControl
     (ForVarControl)
     (ForInit ";" opt-Expression ";" opt-ForUpdate)
     )
    (opt-Expression
     ($empty)
     (Expression))
    (opt-ForUpdate
     ($empty)
     (ForUpdate))

    (ForVarControl
     ;; add opt-Annotations
     (Type VariableDeclaratorId  ForVarControlRest))

    (ForVarControlRest
     (ForVariableDeclaratorsRest ";" opt-Expression ";" opt-ForUpdate)
     (":" Expression))

    (ForVariableDeclaratorsRest
     ($empty)
     ("=" VariableInitializer)
     ("=" VariableInitializer "," VariableDeclarator-list)
     ("," VariableDeclarator-list)
     )
    (VariableDeclarator-list
     (VariableDeclarator)
     (VariableDeclarator-list "," VariableDeclarator))

    (ForInit
     (ForUpdate))

    (ForUpdate
     (StatementExpression)
     (ForUpdate "," StatementExpression))

    (Expression
     (Expression1)
     (Expression1 AssignmentOperator Expression1)
     )

    (AssignmentOperator
     ("=")
     ("+=")
     ("-=")
     ("*=")
     ("/=")
     ("&=")
     ("|=")
     ("^=")
     ("%=")
     ("<<=")
     (">>=")
     (">>>="))

    (Expression1 
     (Expression2)
     (Expression2 Expression1Rest))

    (Expression1Rest
     ("?" Expression ":" Expression1))

    (Expression2
     (Expression3)
     (Expression3 "instanceof" Type)
     (Expression2 InfixOp Expression3))

    (InfixOp
     ("||")
     ("&&")
     ("|")
     ("^")
     ("&")
     ("==")
     ("!=")
     ("<")
     (">")
     ("<=")
     (">=")
     ("<<")
     (">>")
     (">>>")
     ("+")
     ("-")
     ("*")
     ("/")
     ("%"))

    (Expression3
     (PrefixOp Expression3)
     ;;("(" Expression ")" Expression3) CONFLICT
     ;;("(" Type ")" Expression3) CONFLICT
     ;; (Primary { Selector } { PostfixOp }) => (check?)
     (Primary)
     ;;(Primary Selector-list)
     (Primary PostfixOp)
     ;;(Primary Selector-list PostfixOp)
     )
    (Selector-list
     (Selector)
     (Selector-list Selector))
      
    (PrefixOp
     ("++")
     ("--")
     ("!")
     ("~")
     ("+")
     ("-"))

    (PostfixOp
     ("++")
     ("--"))

    (Primary
     (Literal)
     ("(" Expression ")")
     ("this")
     ("this" Arguments)
     ("super" SuperSuffix)
     ("new" Creator)
     ;;(NonWildcardTypeArguments ExplicitGenericInvocationSuffix)
     ;;(NonWildcardTypeArguments "this" Arguments)
     (QualifiedIdentifier)
     (QualifiedIdentifier IdentifierSuffix)
     (QualifiedIdentifier Arguments)
     (BasicType opt-arrays "." "class")
     ("void" "." "class"))

    (Literal
     (IntegerLiteral)
     (FloatingPointLiteral)
     (CharacterLiteral)
     (StringLiteral)
     (BooleanLiteral)
     (NullLiteral))

    (Arguments
     ("(" Expression-list ")")
     ("(" ")"))

    (Expression-list
     (Expression)
     (Expression-list "," Expression))

    (SuperSuffix
     (Arguments)
     ("." Identifier Arguments)
     ("." Identifier))

    (ExplicitGenericInvocationSuffix
     ("super" SuperSuffix)
     (Identifier Arguments))

    (Creator
     ;;(NonWildcardTypeArguments CreatedName ClassCreatorRest)
     (CreatedName ClassCreatorRest)
     (CreatedName ArrayCreatorRest))

    (CreatedName
     (Identifier)
     (Identifier TypeArgumentsOrDiamond)
     (CreatedName "." Identifier)
     (CreatedName "." Identifier TypeArgumentsOrDiamond)
     )

    (ClassCreatorRest
     (Arguments)
     (Arguments ClassBody)
     )

    ;; TODO: needs work
    (ArrayCreatorRest
     ;;[ (] {[]} ArrayInitializer  |  Expression ] {"[" Expression "]"} {[]})
     (array-expr-list opt-arrays)
     (arrays)
     )
    (array-expr-list
     ("[" Expression "]")
     (array-expr-list "[" Expression "]"))

    (IdentifierSuffix
     ("[" "." "class" "]")
     ("[" arrays "." "class" "]")
     ("[" Expression "]")
     ("." "class")
     ;;("." ExplicitGenericInvocation)
     ("." "this")
     ("." "super" Arguments)
     ("." "new" InnerCreator)
     ("." "new" NonWildcardTypeArguments InnerCreator)
     )

    (ExplicitGenericInvocation
     (NonWildcardTypeArguments ExplicitGenericInvocationSuffix))

    (InnerCreator
     (Identifier ClassCreatorRest)
     (Identifier NonWildcardTypeArgumentsOrDiamond ClassCreatorRest)
     )

    (Selector
     ("." Identifier)
     ("." Identifier Arguments)
     ("." ExplicitGenericInvocation)
     ("." "this")
     ("." "super" SuperSuffix)
     ("." "new" InnerCreator)
     ;;("." "new" NonWildcardTypeArguments InnerCreator)
     ("[" Expression "]")		; ??? check
     )

    ;; check
    (EnumBody
     ("{" "}")
     ("{" EnumConstants "}")
     ("{" EnumConstants "," EnumBodyDeclarations "}")
     ("{" EnumBodyDeclarations "}")
     )

    (EnumConstants
     (EnumConstant)
     (EnumConstants "," EnumConstant))

    (EnumConstant
     ;;(opt-Annotations Identifier opt-Arguments opt-ClassBody)
     (Identifier opt-Arguments opt-ClassBody)
     )
    #;(opt-Annotations
     ($empty)
     (Annotations))
    (opt-Arguments
     ($empty)
     (Arguments))
    (opt-ClassBody
     ($empty)
     (ClassBody))

    (EnumBodyDeclarations
     (";" ClassBodyDeclaration)
     (EnumBodyDeclarations ClassBodyDeclaration))

    (IntegerLiteral ($fixed ($$ `(IntegerLiteral ,$1))))
    (FloatingPointLiteral ($float ($$ `(FloatingPointLiteral ,$1))))
    (CharacterLiteral ($chlit ($$ `(CharacterLiteral ,$1))))
    (StringLiteral ($string ($$ `(StringLiteral ,$1))))
    (BooleanLiteral
     ("true" ($$ `(BooleanLiteral ,$1)))
     ("false" ($$ `(BooleanLiteral ,$1))))
    (NullLiteral ("null" ($$ `(NullLiteral ,$1))))

    (Annotations
     (Annotation)
     (Annotations Annotation))

    (Annotation
     ("@" QualifiedIdentifier)
     ("@" QualifiedIdentifier "(" ")")
     ("@" QualifiedIdentifier "(" AnnotationElement ")"))

    (AnnotationElement
     (ElementValuePairs)
     (ElementValue))

    (AnnotationTypeDeclaration
     ("@" "interface" Identifier AnnotationTypeBody))

    (AnnotationTypeBody
     ("{" "}")
     ("{" AnnotationTypeElementDeclarations "}"))

    (AnnotationTypeElementDeclarations
     (AnnotationTypeElementDeclaration)
     (AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration))

    (AnnotationTypeElementDeclaration
     (Modifier-list AnnotationTypeElementRest))

    (AnnotationTypeElementRest
     (Type Identifier AnnotationMethodOrConstantRest ";")
     (ClassDeclaration)
     (InterfaceDeclaration)
     (EnumDeclaration)
     (AnnotationTypeDeclaration)
     )

    (AnnotationMethodOrConstantRest
     (AnnotationMethodRest)
     (ConstantDeclaratorsRest))

    (AnnotationMethodRest
     ("(" ")" opt-arrays)
     ("(" ")" opt-arrays "default" ElementValue))

    )))

;; === parsers ==========================

(define java-mach
  (and java-spec
       (hashify-machine
	(compact-machine
	 (make-lalr-machine java-spec)))))

;; === automaton file generators =========

(define (gen-java-files . rest)
  (define (lang-dir path)
    (if (pair? rest) (string-append (car rest) "/" path) path))
  (define (xtra-dir path)
    (lang-dir (string-append "mach.d/" path)))

  (write-lalr-actions java-mach (xtra-dir "java-act.scm.new") #:prefix "java-")
  (write-lalr-tables java-mach (xtra-dir "java-tab.scm.new") #:prefix "java-")
  (let ((a (move-if-changed (xtra-dir "java-act.scm.new")
			    (xtra-dir "java-act.scm")))
	(b (move-if-changed (xtra-dir "java-tab.scm.new")
			    (xtra-dir "java-tab.scm"))))
    (or a b)))

;; --- last line ---
