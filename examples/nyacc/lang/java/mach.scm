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

;;; Notes

;; heavily modified from
;; ref: https://docs.oracle.com/javase/specs/jls/se7/html/

;; DimExprs is DimExpr ... Dim ...

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

;; dealing with TypeParameters is hmmm

;; changed: 

;;; Code:

(define-module (nyacc lang java mach)
  #:export (java-spec java-mach gen-java-files)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

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
     (QualifiedIdentifier-1 "." Identifier ($$ (tl-append $1 $3))))
    
    (QualifiedIdentifierList
     (QualifiedIdentifierList-1 ($$ (tl->list $1))))
    (QualifiedIdentifierList-1
     (QualifiedIdentifier ($$ (make-tl 'QualifiedIdentifierList $1)))
     (QualifiedIdentifierList-1 "," QualifiedIdentifier ($$ (tl-append $1 $3))))

    (QualifiedIdentifierMaybeGlob
     (QualifiedIdentifierMaybeGlob-1 ($$ (tl->list $1))))
    (QualifiedIdentifierMaybeGlob-1
     (Identifier ($$ (make-tl 'ExtendedIdentifier $1)))
     (QualifiedIdentifierMaybeGlob-1 "." Identifier ($$ (tl-append $1 $3)))
     (QualifiedIdentifierMaybeGlob-1 "." "*" ($$ (tl-append $1 (glob)))))
    
    #;(QualifiedIdentifierMaybeSuffix
     (QI+suffix-1 ($$ (tl->list $1))))
    #;(QI+suffix-1
     (Identifier ($$ (make-tl 'ExtendedIdentifier $1)))
     (QI+suffix-1 "." Identifier ($$ (tl-append $1 $3)))
     ;; IdentifierSuffix:
     (QI+suffix-1 "." "this" ($$ (tl-append $1 `(this))))
     (QI+suffix-1 "." "super" Arguments ($$ (tl-append $1 `(super ,$4))))
     (QI+suffix-1 "." "new" InnerCreator ($$ (tl-append $1 `(new ,$4))))
     (QI+suffix-1 "." "new" NonWildcardTypeArguments InnerCreator
		  ($$ (tl-append $1 `(new ,$4 ,$5))))
     )
     
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
     ("import" "static" QualifiedIdentifierMaybeGlob ";"
      ($$ `(ImportDeclaration (Modified "static" ,$3))))
     ("import" QualifiedIdentifierMaybeGlob ";" ($$ `(ImportDeclaration ,$3))))

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
     ;;(AnnotationTypeDeclaration) ;; CONFLICTs w/ Modifiers ("protected" ...)
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
     (BasicType Dims ($$ (append $1 (list $2))))
     ;; HACK:
     ;; Replace ReferenceType with QualifiedIdentifier to eliminate
     ;; reduce-reduce conflict for decl's w/ parameterized types.
     #|
     (ReferenceType)
     (ReferenceType Dims)
     |#
     (QualifiedIdentifier)
     (QualifiedIdentifier Dims ($$ (append $1 (list $2))))
     )

    (opt-Dims
     ($empty)
     (Dims))
    (Dims
     (Dims-1 ($$ (tl->list $1))))
    (Dims-1
     ("[" "]" ($$ (make-tl 'Dims `(Dim "[]"))))
     (Dims-1 "[" "]" ($$ (tl-append $1 `(Dim "[]")))))

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

    (TypeArguments
     ("<" TypeArgument-list ">" ($$ (tl->list $2))))
    (TypeArgument-list
     (TypeArgument ($$ (make-tl 'TypeArguments $1)))
     (TypeArgument-list "," TypeArgument ($$ (tl-append $1 $3))))

    (TypeArgument
     (ReferenceType "?" ($$ `(TypeArument ,$1)))
     (ReferenceType "?" "extends" ReferenceType
		    ($$ `(TypeArument ,$1 (extends ,$4))))
     (ReferenceType "?" "super" ReferenceType
		    ($$ `(TypeArument ,$1 (super ,$4)))))

    (NonWildcardTypeArguments
     ("<" TypeList ">" ($$ `(TypeArguments ,(sx-tail $2)))))

    (TypeList
     (TypeList-1 ($$ (tl->list $1))))
    (TypeList-1
     (ReferenceType ($$ (make-tl 'TypeList $1)))
     (TypeList "," ReferenceType ($$ (tl-append $1 $3))))

    (TypeArgumentsOrDiamond
     ("<" ">" ($$ '(Diamond "<>")))
     (TypeArguments))

    (NonWildcardTypeArgumentsOrDiamond
     ("<" ">" ($$ '(Diamond "<>")))
     (NonWildcardTypeArguments))

    ;; "<" shift-reduce conflict: reduce Expression2 looking at "<"
    (TypeParameters
     ("<" TypeParameter-list ">" ($$ (tl->list $2))))
    (TypeParameter-list
     (TypeParameter ($$ (make-tl 'TypeParameters $1)))
     (TypeParameter-list "," TypeParameter ($$ (tl-append $1 $3))))

    (TypeParameter
     (Identifier ($$ `(TypeParameter ,$1)))
     (Identifier "extends" Bound ($$ `(TypeParameter ,$1 (extends ,$3)))))

    (Bound
     (ReferenceType-list ($$ (tl->list $1))))
    (ReferenceType-list
     (ReferenceType ($$ (make-tl 'Bound $1)))
     (ReferenceType-list "&" ReferenceType ($$ (tl-append $1 $3))))

    (Modifier
     (Modifier-1 ($$ `(Modifier ,$1))))
    (Modifier-1
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
     (ElementValuePairs-1 ($$ (tl->list $1))))
    (ElementValuePairs-1
     (ElementValuePair ($$ (make-tl 'ElementValuePair $1)))
     (ElementValuePairs-1 "," ElementValuePair ($$ (tl-append $1 $3))))

    (ElementValuePair
     (Identifier "=" ElementValue ($$ `(ElementValuePair ,$1 ,$3))))
    
    (ElementValue
     ;;(Annotation)
     (Expression1)
     (ElementValueArrayInitializer))

    (ElementValueArrayInitializer
     ("{" "}" ($$ `(ElementValueArrayInitializer (ElementValues))))
     ("{" ElementValues "}" ($$ `(ElementValueArrayInitializer ,$2)))
     ("{" ElementValues "," "}" ($$ `(ElementValueArrayInitializer ,$2))))

    (ElementValues
     (ElementValues-1 ($$ (tl->list $1))))
    (ElementValues-1
     (ElementValue ($$ (make-tl 'ElementValues $1)))
     (ElementValues "," ElementValue ($$ (tl-append $1 $3))))

    (ClassBody
     ("{" ClassBodyDeclaration-list "}" ($$ `(ClassBody ,(cdr (reverse $2))))))
    (ClassBodyDeclaration-list
     (ClassBodyDeclaration ($$ (list $1)))
     (ClassBodyDeclaration-list ClassBodyDeclaration ($$ (cons $2 $1))))

    (ClassBodyDeclaration
     (";" ($$ '(MemberDecl)))
     (MemberDecl)
     (Modifier MemberDecl ($$ `(Modified ,$1 ,$2)))
     ("static" Block ($$ `(Modified (Modifier "static") ,$2)))
     (Block))

    ;; Potential conflict on "<" here between Type and
    ;; TypeParameters (within GenericMethodOrConstructorDecl)
    (MemberDecl
     ;;(Type Identifier FieldDeclaratorsRest ";"
     (Type VariableDeclarators ";"
	   ($$ `(MemberDecl ,$1 ,$2)))
     (Type Identifier MethodDeclaratorRest ($$ `(MemberDecl ,$1 ,$2 ,@$3)))
     ("void" Identifier VoidMethodDeclaratorRest
      ($$ `(MemberDecl (void-type "void") ,$2 ,$3)))
     (Identifier ConstructorDeclaratorRest ($$ `(MemberDecl ,$1 ,@(cdr $2))))
     (GenericMethodOrConstructorDecl)
     (ClassDeclaration)
     (InterfaceDeclaration))

    #;(FieldDeclaratorsRest
     (FieldDeclaratorsRest-1 ($$ (cdr (tl->list $1)))))
    #;(FieldDeclaratorsRest-1
     (VariableDeclaratorRest ($$ (make-tl 'list $1)))
     (FieldDeclaratorsRest-1 "," VariableDeclarator ($$ (tl-append $1 $3))))

    (MethodDeclaratorRest
     (FormalParameters
      opt-Dims Block
      ($$ (if (pair? $2)
	      (list $1 $2 $3)
	      (list $1 $3))))
     (FormalParameters
      opt-Dims "throws" QualifiedIdentifierList Block
      ($$ (if (pair? $2)
	      (list $1 $2 $5 `(throws $4))
	      (list $1 $5 `(throws $4)))))
     (FormalParameters
      opt-Dims ";" ($$ (if (pair? $2) (list $1 $2) (list $1))))
     (FormalParameters
      opt-Dims "throws" QualifiedIdentifierList ";"
      ($$ (if (pair? $2)
	      (list $1 $2 `(throws $4))
	      (list $1 (throws $4)))))
     )

    (VoidMethodDeclaratorRest
     (FormalParameters
      Block ($$ (list $1 $2)))
     (FormalParameters
      "throws" QualifiedIdentifierList Block
      ($$ (list $1 $4 `(throws ,$3))))
     (FormalParameters ";" ($$ (list $1)))
     (FormalParameters
      "throws" QualifiedIdentifierList ";"
      ($$ (list $1 `(throws ,$3)))))

    (ConstructorDeclaratorRest
     (FormalParameters Block ($$ (list $1 $2)))
     (FormalParameters "throws" QualifiedIdentifierList Block
		       ($$ (list $1 $4 `(throws ,$3)))))

    (GenericMethodOrConstructorDecl
     (TypeParameters
      GenericMethodOrConstructorRest
      ($$ (let loop ((out '()) (in $2))
	    (if (member (sx-tag (car out)) '(Identifier QualifiedIdentifier))
		(cons* (resverse out) $1 in)
		(loop (cons (car in) out) (cdr in)))))))

    (GenericMethodOrConstructorRest
     (Type Identifier MethodDeclaratorRest ($$ `(Method $1 $2 ,@$3)))
     ("void" Identifier MethodDeclaratorRest
      ($$ `(Method (void "void") $2 ,@$3)))
     (Identifier ConstructorDeclaratorRest ($$ `(Constructor $2 ,@$2))))

    (InterfaceBody
     ("{" InterfaceBodyDeclaration-list "}" ($$ `(Body ,@(sx-tail $2))))
     ("{" "}" ($$ `(Body))))
    (InterfaceBodyDeclaration-list
     (InterfaceBodyDeclaration-list-1 ($$ (tl->list $1))))
    (InterfaceBodyDeclaration-list-1
     (InterfaceBodyDeclaration ($$ (make-tl 'ibl $1)))
     (InterfaceBodyDeclaration-list-1 InterfaceBodyDeclaration
				      ($$ (tl-append $1 $2))))

    (InterfaceBodyDeclaration
     (";" ($$ `(InterfaceDecl)))
     (Modifier-list InterfaceMemberDecl ($$ `(Modified ,$1 ,$2)))
     (InterfaceMemberDecl))

    (Modifier-list
     (Modifier-list-1 ($$ (tl->list $1))))
    (Modifier-list-1
     (Modifier ($$ (make-tl 'Modifiers $1)))
     (Modifier-list Modifier ($$ (tl-append $1 $2))))
     
    (InterfaceMemberDecl
     ;;(InterfaceMethodOrFieldDecl)
     (Type Identifier ConstantDeclaratorRest ";"
	   ($$ `(Decl $1 $2 ,@$3)))
     (Type Identifier InterfaceMethodDeclaratorRest
	   ($$ `(Decl $1 $2 ,@$3)))
     ("void" Identifier VoidInterfaceMethodDeclaratorRest
      ($$ `(MethodDecl (void "void") $2 ,@$3)))
     (InterfaceGenericMethodDecl)
     (ClassDeclaration)
     (InterfaceDeclaration))

    #;(InterfaceMethodOrFieldDecl
     (Type Identifier InterfaceMethodOrFieldRest))

    #;(InterfaceMethodOrFieldRest
     (ConstantDeclaratorsRest ";")
     (InterfaceMethodDeclaratorRest))

    (ConstantDeclaratorsRest
     (ConstantDeclaratorsRest-1 ($$ (sx-tail (tl->list $1)))))
    (ConstantDeclaratorsRest-1
     (ConstantDeclaratorRest ($$ (make-tl 'rest $1)))
     (ConstantDeclaratorsRest-1 "," ConstantDeclarator ($$ (tl->append $1 $3))))

    (ConstantDeclaratorRest
     ("=" VariableInitializer)
     (Dims "=" VariableInitializer))

    (ConstantDeclarator
     (Identifier ConstantDeclaratorRest))

    (InterfaceMethodDeclaratorRest
     (FormalParameters opt-Dims ";")
     (FormalParameters opt-Dims "throws" QualifiedIdentifierList ";")
     )

    (VoidInterfaceMethodDeclaratorRest
     (FormalParameters ";")
     (FormalParameters "throws" QualifiedIdentifierList ";")
     )

    (InterfaceGenericMethodDecl
     (TypeParameters Type Identifier InterfaceMethodDeclaratorRest)
     (TypeParameters "void" Identifier InterfaceMethodDeclaratorRest))

    (FormalParameters
     ("(" FormalParameterDecls ")" ($$ `(FormalParameters (tl->list $1))))
     ("(" ")" ($$ `(FormalParameters))))

    (FormalParameterDecls
     (Type VariableDeclaratorId ($$ `(make-tl 'FPDs `(param ,$1 ,$2))))
     (Type VariableDeclaratorId "," FormalParameterDecls
	   ($$ (tl-insert $3 `(param ,$1 ,$2))))
     (Type "..." VariableDeclaratorId
	   ($$ `(make-tl 'FPDs `(xxx-param ,$1 ,$3)))))

   #;(FormalParameterDeclsRest
     (VariableDeclaratorId)
     (VariableDeclaratorId "," FormalParameterDecls)
     ("..." VariableDeclaratorId))

    (VariableDeclaratorId
     (Identifier)
     (Identifier Dims ($$ `(array-decl-id ,$1 ,$2))))

    (VariableDeclarators
     (VariableDeclarators-1 ($$ (tl->list $1))))
    (VariableDeclarators-1
     (VariableDeclarator ($$ (make-tl 'VariableDeclartors $1)))
     (VariableDeclarators-1 "," VariableDeclarator ($$ (tl-append $1 $3))))

    (VariableDeclarator
     ;;(Identifier VariableDeclaratorRest)
     (Identifier ($$ `(Declr ,$1)))
     (Identifier "=" VariableInitializer ($$ `(Declr ,$1) ,$2))
     (Identifier Dims ($$ `(Declr (Dimmed ,$1 ,$2))))
     (Identifier Dims "=" VariableInitializer
		 ($$ `(Declr (Dimmed ,$1 ,$2) ,$3))))

   #;(VariableDeclaratorRest
     ($empty)
     ("=" VariableInitializer)
     (Dims)
     (Dims "=" VariableInitializer))

    (VariableInitializer
     (ArrayInitializer)
     (Expression))

    (ArrayInitializer
     ("{" VariableInitializer-list "}" ($$ `(ArrayInitializer ,$2)))
     ("{" VariableInitializer-list "," "}" ($$ `(ArrayInitializer ,$2)))
     ("{" "}" ($$ `(ArrayInitializer (VariableInitializers)))))
    
    (VariableInitializer-list
     (VariableInitializer-list-1 ($$ (tl->list $1))))
    (VariableInitializer-list-1
     (VariableInitializer ($$ (make-tl 'VariableInitializers $1)))
     (VariableInitializer-list-1 "," VariableInitializer ($$ (tl-append $1 $3))))

    (Block
     ("{" BlockStatements "}" ($$ `(Block ,(cdr $2))))
     ("{" "}" ($$ '(Block))))

    (BlockStatements
     (BlockStatements-1 ($$ (tl->list $1))))
    (BlockStatements-1
     (BlockStatement ($$ (make-tl 'BlockStatements $1)))
     (BlockStatements-1 BlockStatement ($$ (tl-append $1 $2))))

    ;; TODO: add opt-Annotations to front of "final" items
    (BlockStatement
     (Type VariableDeclarators ";"
	   ($$ `(Decl ,$1 ,$2)))
     ("final" Type VariableDeclarators ";"
      ($$ `(Modified (Modifier "final") (Decl ,$2 ,$3))))
     (ClassOrInterfaceDeclaration)
     (Modifier-list ClassOrInterfaceDeclaration ($$ `(Modified ,$1 ,$2)))
     (Statement))

    (Statement
     (Block)
     (";" ($$ `(Statement)))
     (Identifier ":" Statement ($$ `(LabelledStatement ,$1 ,$3)))
     (StatementExpression ";" ($$ `(StatementExpression ,$1)))
     ("if" "(" Expression ")" Statement ($prec 'then) ($$ `(if ,$3 ,$5)))
     ("if" "(" Expression ")" Statement "else" Statement ($$ `(if ,$3 ,$5 ,$7)))
     ("assert" Expression ";" ($$ `(Assert ,$2)))
     ("assert" Expression ":" Expression ";" ($$ `(Assert ,$2 ,$5)))
     ("switch" "(" Expression ")" "{" SwitchBlockStatementGroups "}"
      ($$ `(Switch ,$3 ,$6)))
     ("while" "(" Expression ")" Statement ($$ `(While ,$3 ,$5)))
     ("do" Statement "while" "(" Expression ")" ";" ($$ `(Do ,$2 ,$5)))
     ("for" "(" ForControl ")" Statement
      ;; todo
      )
     ("break" ";" ($$ `(Break)))
     ("break" Identifier ";" ($$ `(Break ,$2)))
     ("continue" ";" ($$ `(Continue)))
     ("continue" Identifier ";" ($$ `(Continue ,$2)))
     ("return" Expression ";" ($$ `(Return ,$2)))
     ("return" ";" ($$ `(Return)))
     ("throw" Expression ";" ($$ `(Throw ,$2)))
     ("synchronized" "(" Expression ")" Block ($$ `(Synchronized ,$3 ,$5)))
     ("try" Block Catches Finally ($$ `(Try ,$2 ,$3 ,$4)))
     ("try" Block Catches ($$ `(Try ,$2 ,$3)))
     ("try" Block Finally ($$ `(Try ,$2 ,$3)))
     ("try" ResourceSpecification Block Catches Finally
      ($$ `(Try ,$2 ,$3 ,$4 ,$5)))
     ("try" ResourceSpecification Block Catches ($$ `(Try ,$2 ,$3 ,$4)))
     ("try" ResourceSpecification Block Finally ($$ `(Try ,$2 ,$3 ,$4)))
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
     (QualifiedIdentifier Arguments ($$ `(CallStmt ,$1 ,$2)))
     (QualifiedIdentifier IdentifierSuffix Arguments
			  ($$ `(CallStmt ,$1 ,$2 ,$3)))
     (QualifiedIdentifier AssignmentOperator Expression1
			  ($$ `(AssnStmt ,$1 ,$2 ,$3)))
     (QualifiedIdentifier IdentifierSuffix AssignmentOperator Expression1
			  ($$ `(AssnStmt ,$1 ,$2 ,$3 ,$4)))
     (QualifiedIdentifier "++" ($$ `(PostIncrStmt ,$1)))
     (QualifiedIdentifier "--" ($$ `(PostDecrStmt ,$1)))
     ;; ^ may need to sub ReferenceType and sub back later via ...
     ;;  `(QualifiedIdentifier ,(cdr $1))
     ("++" QualifiedIdentifier ($$ `(PreIncrStmt ,$1)))
     ("--" QualifiedIdentifier ($$ `(PreDecrStmt ,$1)))
     )

    (Catches
     (Catches-1 ($$ (tl->list $1))))
    (Catches-1
     (CatchClause ($$ (make-tl 'Catches $1)))
     (Catches-1 CatchClause ($$ (tl-append $1 $2))))

    ;; check
    (CatchClause
     ("catch" "(" CatchType Identifier ")" Block
      ($$ `(CatchClause ,$3 ,$4 ,$6))))

    (CatchType
     (CatchType-1 ($$ (tl->list $1))))
    (CatchType-1
     (QualifiedIdentifier ($$ (make-tl 'CatchType $1)))
     (CatchType-1 "|" QualifiedIdentifier ($$ (tl-append $1 $3))))

    (Finally
     ("finally" Block ($$ `(Finally ,$2))))

    (ResourceSpecification
     ("(" Resources ")" ($$ $2))
     ("(" Resources ";" ")" ($$ $2)))
    
    (Resources
     (Resources-1 ($$ (tl->list $1))))
    (Resources-1
     (Resource ($$ (make-tl 'Resources $1)))
     (Resources-1 ";" Resource ($$ (tl-append $1 $3))))

    (Resource
     ;; first form missing opt-Annotations
     ("final" ReferenceType VariableDeclaratorId "=" Expression
      ($$ `(Modified "final" (Resource ,$2 ,$3 ,$5))))
     (ReferenceType VariableDeclaratorId "=" Expression
		    ($$ `(Resource ,$2 ,$3 ,$5))))

    (SwitchBlockStatementGroups
     (SwitchBlockStatementGroups-1 ($$ (tl->list $1))))
    (SwitchBlockStatementGroups-1
     (SwitchBlockStatementGroup ($$ (make-tl 'SwitchBlockStatementGroups $1)))
     (SwitchBlockStatementGroups-1 SwitchBlockStatementGroup
				   ($$ (tl-append $1 $2))))

    (SwitchBlockStatementGroup
     (SwitchLabels BlockStatements ($$ `(SwitchBlockStatementGroup ,$1 ,$2))))

    (SwitchLabels
     (SwitchLabels-1 ($$ (tl->list $1))))
    (SwitchLabels-1
     (SwitchLabel ($$ (make-tl 'SwitchLabels $1)))
     (SwitchLabels-1 SwitchLabel ($$ (tl->append $1 $2))))

    (SwitchLabel
     ("case" Expression ":" ($$ `(SwitchLabel ,$2)))
     ;;("case" EnumConstantName ":") CONFLICT
     ("default" ":" ($$ `(SwitchLabel (default)))))

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
     (Expression1 AssignmentOperator Expression1
		  ($$ `(assn-expr $1 (op ,$2) $3))))

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
     (Expression2 "?" Expression ":" Expression1
		  ($$ `(cond-expr ,$1 ,$3 ,$5))))

    (Expression2
     (Expression3)
     (Expression3 "instanceof" Type ($$ `(instanceof ,$1 ,$3)))
     (Expression2 InfixOp Expression3 ($$ (list $2 $1 $3))))

    (InfixOp
     ("||" ($$ 'or))
     ("&&" ($$ 'and))
     ("|" ($$ 'bitwise-or))
     ("^" ($$ 'bitwise-xor))
     ("&" ($$ 'bitwise-and))
     ("==" ($$ 'eq))
     ("!=" ($$ 'ne))
     ("<" ($$ 'lt))
     (">" ($$ 'gt))
     ("<=" ($$ 'le))
     (">=" ($$ 'ge))
     ("<<" ($$ 'lshift))
     (">>" ($$ 'rshift))
     (">>>" ($$ 'rrshift))
     ("+" ($$ 'add))
     ("-" ($$ 'sub))
     ("*" ($$ 'mul))
     ("/" ($$ 'div))
     ("%" ($$ 'mod)))

    (Expression3
     (PrefixOp Expression3 ($$ (list $1 $2)))
     ;;("(" Expression ")" Expression3) ;; sr-conf . ++ -- + -
     ;;("(" Type ")" Expression3) ;; rr-conf ")" 
     (Primary)
     (Primary PostfixOp ($$ (list $2 $1)))
     ;;(Primary Selector-list) ;; sr-conf . [
     ;;(Primary Selector-list PostfixOp)
     )
    (Selector-list
     (Selector)
     (Selector-list Selector))
      
    (PrefixOp
     ("++" ($$ 'pre-inc))
     ("--" ($$ 'pre-dec))
     ("!" ($$ 'not))
     ("~" ($$ 'bitwise-not))
     ("+" ($$ 'pos))
     ("-" ($$ 'neg)))

    (PostfixOp
     ("++" ($$ 'post-inc))
     ("--" ($$ 'post-dec)))

    (Primary
     (Literal)
     ("(" Expression ")" ($$ $2))
     ("this" ($$ '(this)))
     ("this" Arguments ($$ `(this ,$2)))
     ("super" SuperSuffix ($$ `(super ,$2)))
     ("new" Creator ($$ `(new ,$2)))
     (NonWildcardTypeArguments "this" Arguments)
     (NonWildcardTypeArguments "super" SuperSuffix)
     (NonWildcardTypeArguments Identifier Arguments)
     ;;(QualifiedIdentifierMaybeSuffix)
     (QualifiedIdentifier)
     (QualifiedIdentifier IdentifierSuffix)
     (QualifiedIdentifier Arguments)
     (BasicType opt-Dims "." "class")
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

    (Creator
     (NonWildcardTypeArguments CreatedName Arguments)
     (NonWildcardTypeArguments CreatedName Arguments ClassBody)
     (CreatedName Arguments)
     (CreatedName Arguments ClassBody)
     (CreatedName DimExprs)
     (CreatedName Dims)
     (CreatedName Dims ArrayInitializer)
     )

    (CreatedName
     (Identifier)
     (Identifier TypeArgumentsOrDiamond)
     (CreatedName "." Identifier)
     (CreatedName "." Identifier TypeArgumentsOrDiamond)
     )

    (DimExprs
     (DimExprs-1 ($$ (tl->list $1))))
    (DimExprs-1
     ("[" Expression "]" ($$ (make-tl 'DimExprs `(DimExpr ,$2))))
     (DimExprs-1 "[" Expression "]" ($$ (tl-append $1 `(DimExpr ,$3))))
     (DimExprs-1 "[" "]" ($$ (tl-append $1 `(Dim "[]"))))
     )

    (IdentifierSuffix
     ;; ???
     ;; example: a[(a=b)[3]]
     ;;("." "class")
     ;;("[" Dims "." "class" "]")
     ;;("[" Expression "]")
     ("." "class")
     ;;("." ExplicitGenericInvocation) ;; sr-conf (
     ("." "this")
     ("." "super" Arguments)
     ("." "new" InnerCreator)
     ("." "new" NonWildcardTypeArguments InnerCreator)
     )

    (ExplicitGenericInvocation
     (NonWildcardTypeArguments "super" SuperSuffix)
     (NonWildcardTypeArguments Identifier Arguments))

    (InnerCreator
     (Identifier Arguments)
     (Identifier Arguments ClassBody)
     (Identifier NonWildcardTypeArgumentsOrDiamond Arguments)
     (Identifier NonWildcardTypeArgumentsOrDiamond Arguments ClassBody)
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

    (EnumBody
     ("{" "}" ($$ `(EnumBody)))
     ("{" EnumConstants "}" ($$ `(EnumBody ,$2)))
     ("{" EnumConstants "," "}" ($$ `(EnumBody ,$2)))
     ("{" EnumConstants ";" EnumBodyDeclarations "}" ($$ `(EnumBody ,$2 ,$4)))
     ("{" EnumConstants "," ";" EnumBodyDeclarations "}"
      ($$ `(EnumBody ,$2 ,$5)))
     ("{" ";" EnumBodyDeclarations "}" ($$ `(EnumBody ,$3)))
     )

    (EnumConstants
     (EnumConstants-1 ($$ (tl->list $1))))
    (EnumConstants-1
     (EnumConstant ($$ (make-tl 'EnumConstants $1)))
     (EnumConstants-1 "," EnumConstant ($$ (tl-append $1 $3))))

    (EnumBodyDeclarations
     (EnumBodyDeclarations-1 ($$ (tl->list $1))))
    (EnumBodyDeclarations-1
     (ClassBodyDeclaration ($$ (make-tl 'EnumBodyDeclarations $1)))
     (EnumBodyDeclarations-1 ClassBodyDeclaration ($$ (tl-append $1 $2))))

    (EnumConstant
     ;;(opt-Annotations Identifier opt-Arguments opt-ClassBody)
     (Identifier
      opt-Arguments opt-ClassBody
      ($$ (let* ((tail (if (pair? opt-ClassBody) (list opt-ClassBody) '()))
		 (tail (if (pair? opt-Arguments) (append opt-Arguments tail))))
	    `(EnumConstant ,$1 . ,tail))))
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
     ("(" ")" opt-Dims)
     ("(" ")" opt-Dims "default" ElementValue))

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
