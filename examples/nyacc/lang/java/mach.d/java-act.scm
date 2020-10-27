;; java-act.scm

(define java-act-v
  (vector
   ;; $start => CompliationUnit
   (lambda ($1 . $rest) $1)
   ;; Identifier => '$ident
   (lambda ($1 . $rest) `(Identifier $1))
   ;; QualifiedIdentifier => QualifiedIdentifier-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; QualifiedIdentifier-1 => Identifier
   (lambda ($1 . $rest)
     (make-tl 'QualifiedIdentifier $1))
   ;; QualifiedIdentifier-1 => QualifiedIdentifier-1 "." Identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; QualifiedIdentifierList => QualifiedIdentifierList-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; QualifiedIdentifierList-1 => QualifiedIdentifier
   (lambda ($1 . $rest)
     (make-tl 'QualifiedIdentifierList $1))
   ;; QualifiedIdentifierList-1 => QualifiedIdentifierList-1 "," QualifiedI...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; QualifiedIdentifierMaybeGlob => QualifiedIdentifierMaybeGlob-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; QualifiedIdentifierMaybeGlob-1 => Identifier
   (lambda ($1 . $rest)
     (make-tl 'ExtendedIdentifier $1))
   ;; QualifiedIdentifierMaybeGlob-1 => QualifiedIdentifierMaybeGlob-1 "." ...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; QualifiedIdentifierMaybeGlob-1 => QualifiedIdentifierMaybeGlob-1 "." "*"
   (lambda ($3 $2 $1 . $rest) (tl-append $1 (glob)))
   ;; CompliationUnit => ImportDeclaration-list TypeDeclaration-list
   (lambda ($2 $1 . $rest)
     `(ComplilationUnit $1 $2))
   ;; CompliationUnit => "package" QualifiedIdentifier ";" ImportDeclaratio...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(CompilationUnit
        (Package ,$2 ,@(cdr $4) ,@(cdr $5))))
   ;; CompliationUnit => Annotations "package" QualifiedIdentifier ";" Impo...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(CompilationUnit
        (Package ,$3 $1 ,@(cdr $5) ,@(cdr $6))))
   ;; ImportDeclaration-list => ImportDeclaration-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ImportDeclaration-list-1 => 
   (lambda $rest (make-tl 'ImportDeclaration-list))
   ;; ImportDeclaration-list-1 => ImportDeclaration-list-1 ImportDeclaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; TypeDeclaration-list => TypeDeclaration-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; TypeDeclaration-list-1 => 
   (lambda $rest (make-tl 'TypeDeclaration-list))
   ;; TypeDeclaration-list-1 => TypeDeclaration-list-1 TypeDeclaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; ImportDeclaration => "import" "static" QualifiedIdentifierMaybeGlob ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ImportDeclaration (Modified "static" ,$3)))
   ;; ImportDeclaration => "import" QualifiedIdentifierMaybeGlob ";"
   (lambda ($3 $2 $1 . $rest)
     `(ImportDeclaration ,$3))
   ;; TypeDeclaration => Modifier-list ClassOrInterfaceDeclaration ";"
   (lambda ($3 $2 $1 . $rest)
     (sx-cons*
       (sx-tag $1)
       (sx-attr $1)
       (append $1 (sx-tail $2))))
   ;; TypeDeclaration => ClassOrInterfaceDeclaration ";"
   (lambda ($2 $1 . $rest) $1)
   ;; ClassOrInterfaceDeclaration => ClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; ClassOrInterfaceDeclaration => InterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; ClassDeclaration => NormalClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; ClassDeclaration => EnumDeclaration
   (lambda ($1 . $rest) $1)
   ;; InterfaceDeclaration => NormalInterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; NormalClassDeclaration => "class" Identifier opt-TypeParameters opt-e...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (let* ((tail (list $6))
            (tail (if (pair? $5) (cons $5 tail) tail))
            (tail (if (pair? $4) (cons $4 tail) tail))
            (tail (if (pair? $3) (cons $3 tail) tail)))
       `(NormalClassDeclaration ,$1 ,@tail)))
   ;; opt-TypeParameters => 
   (lambda $rest (list))
   ;; opt-TypeParameters => TypeParameters
   (lambda ($1 . $rest) $1)
   ;; opt-extends => 
   (lambda $rest (list))
   ;; opt-extends => "extends" Type
   (lambda ($2 $1 . $rest) `(extends ,$2))
   ;; opt-implements => 
   (lambda $rest (list))
   ;; opt-implements => "implements" TypeList
   (lambda ($2 $1 . $rest) `(implements ,$2))
   ;; EnumDeclaration => "enum" Identifier EnumBody
   (lambda ($3 $2 $1 . $rest)
     `(EnumDeclaration ,$2 ,$3))
   ;; EnumDeclaration => "enum" Identifier "implements" TypeList EnumBody
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(EnumDeclaration ,$2 (implements ,$4) ,$5))
   ;; NormalInterfaceDeclaration => "interface" Identifier opt-TypeParamete...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (if (pair? $3)
       `(NormalInterfaceDeclaration
          ,$2
          ,$3
          (extends ,$5)
          ,$6)
       `(NormalInterfaceDeclaration
          ,$2
          (extends ,$5)
          ,$6)))
   ;; NormalInterfaceDeclaration => "interface" Identifier opt-TypeParamete...
   (lambda ($4 $3 $2 $1 . $rest)
     (if (pair? $3)
       `(NormalInterfaceDeclaration ,$2 ,$3 ,$4)
       `(NormalInterfaceDeclaration ,$2 ,$4)))
   ;; Type => BasicType
   (lambda ($1 . $rest) $1)
   ;; Type => BasicType Dims
   (lambda ($2 $1 . $rest) (append $1 (list $2)))
   ;; Type => QualifiedIdentifier
   (lambda ($1 . $rest) $1)
   ;; Type => QualifiedIdentifier Dims
   (lambda ($2 $1 . $rest) (append $1 (list $2)))
   ;; opt-Dims => 
   (lambda $rest (list))
   ;; opt-Dims => Dims
   (lambda ($1 . $rest) $1)
   ;; Dims => Dims-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; Dims-1 => "[" "]"
   (lambda ($2 $1 . $rest)
     (make-tl 'Dims `(Dim "[]")))
   ;; Dims-1 => Dims-1 "[" "]"
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 `(Dim "[]")))
   ;; BasicType => BasicType-1
   (lambda ($1 . $rest) `(BasicType ,$1))
   ;; BasicType-1 => "byte"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "short"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "char"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "int"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "long"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "float"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "double"
   (lambda ($1 . $rest) $1)
   ;; BasicType-1 => "boolean"
   (lambda ($1 . $rest) $1)
   ;; ReferenceType => ReferenceType-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ReferenceType-1 => Identifier
   (lambda ($1 . $rest) (make-tl 'ReferenceType $1))
   ;; ReferenceType-1 => Identifier TypeArguments
   (lambda ($2 $1 . $rest)
     (make-tl 'ReferenceType $1 $2))
   ;; ReferenceType-1 => ReferenceType-1 "." Identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; ReferenceType-1 => ReferenceType-1 "." Identifier TypeArguments
   (lambda ($4 $3 $2 $1 . $rest) (tl-append $1 $3))
   ;; TypeArguments => "<" TypeArgument-list ">"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; TypeArgument-list => TypeArgument
   (lambda ($1 . $rest) (make-tl 'TypeArguments $1))
   ;; TypeArgument-list => TypeArgument-list "," TypeArgument
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; TypeArgument => ReferenceType "?"
   (lambda ($2 $1 . $rest) `(TypeArument ,$1))
   ;; TypeArgument => ReferenceType "?" "extends" ReferenceType
   (lambda ($4 $3 $2 $1 . $rest)
     `(TypeArument ,$1 (extends ,$4)))
   ;; TypeArgument => ReferenceType "?" "super" ReferenceType
   (lambda ($4 $3 $2 $1 . $rest)
     `(TypeArument ,$1 (super ,$4)))
   ;; NonWildcardTypeArguments => "<" TypeList ">"
   (lambda ($3 $2 $1 . $rest)
     `(TypeArguments ,(sx-tail $2)))
   ;; TypeList => TypeList-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; TypeList-1 => ReferenceType
   (lambda ($1 . $rest) (make-tl 'TypeList $1))
   ;; TypeList-1 => TypeList "," ReferenceType
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; TypeArgumentsOrDiamond => "<" ">"
   (lambda ($2 $1 . $rest) '(Diamond "<>"))
   ;; TypeArgumentsOrDiamond => TypeArguments
   (lambda ($1 . $rest) $1)
   ;; NonWildcardTypeArgumentsOrDiamond => "<" ">"
   (lambda ($2 $1 . $rest) '(Diamond "<>"))
   ;; NonWildcardTypeArgumentsOrDiamond => NonWildcardTypeArguments
   (lambda ($1 . $rest) $1)
   ;; TypeParameters => "<" TypeParameter-list ">"
   (lambda ($3 $2 $1 . $rest) (tl->list $2))
   ;; TypeParameter-list => TypeParameter
   (lambda ($1 . $rest)
     (make-tl 'TypeParameters $1))
   ;; TypeParameter-list => TypeParameter-list "," TypeParameter
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; TypeParameter => Identifier
   (lambda ($1 . $rest) `(TypeParameter ,$1))
   ;; TypeParameter => Identifier "extends" Bound
   (lambda ($3 $2 $1 . $rest)
     `(TypeParameter ,$1 (extends ,$3)))
   ;; Bound => ReferenceType-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; ReferenceType-list => ReferenceType
   (lambda ($1 . $rest) (make-tl 'Bound $1))
   ;; ReferenceType-list => ReferenceType-list "&" ReferenceType
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; Modifier => Modifier-1
   (lambda ($1 . $rest) `(Modifier ,$1))
   ;; Modifier-1 => "public"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "protected"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "private"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "static "
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "abstract"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "final"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "native"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "synchronized"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "transient"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "volatile"
   (lambda ($1 . $rest) $1)
   ;; Modifier-1 => "strictfp"
   (lambda ($1 . $rest) $1)
   ;; ElementValuePairs => ElementValuePairs-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ElementValuePairs-1 => ElementValuePair
   (lambda ($1 . $rest)
     (make-tl 'ElementValuePair $1))
   ;; ElementValuePairs-1 => ElementValuePairs-1 "," ElementValuePair
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; ElementValuePair => Identifier "=" ElementValue
   (lambda ($3 $2 $1 . $rest)
     `(ElementValuePair ,$1 ,$3))
   ;; ElementValue => Expression1
   (lambda ($1 . $rest) $1)
   ;; ElementValue => ElementValueArrayInitializer
   (lambda ($1 . $rest) $1)
   ;; ElementValueArrayInitializer => "{" "}"
   (lambda ($2 $1 . $rest)
     `(ElementValueArrayInitializer (ElementValues)))
   ;; ElementValueArrayInitializer => "{" ElementValues "}"
   (lambda ($3 $2 $1 . $rest)
     `(ElementValueArrayInitializer ,$2))
   ;; ElementValueArrayInitializer => "{" ElementValues "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ElementValueArrayInitializer ,$2))
   ;; ElementValues => ElementValues-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; ElementValues-1 => ElementValue
   (lambda ($1 . $rest) (make-tl 'ElementValues $1))
   ;; ElementValues-1 => ElementValues "," ElementValue
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; ClassBody => "{" ClassBodyDeclaration-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(ClassBody ,(cdr (reverse $2))))
   ;; ClassBodyDeclaration-list => ClassBodyDeclaration
   (lambda ($1 . $rest) (list $1))
   ;; ClassBodyDeclaration-list => ClassBodyDeclaration-list ClassBodyDecla...
   (lambda ($2 $1 . $rest) (cons $2 $1))
   ;; ClassBodyDeclaration => ";"
   (lambda ($1 . $rest) '(MemberDecl))
   ;; ClassBodyDeclaration => MemberDecl
   (lambda ($1 . $rest) $1)
   ;; ClassBodyDeclaration => Modifier MemberDecl
   (lambda ($2 $1 . $rest) `(Modified ,$1 ,$2))
   ;; ClassBodyDeclaration => "static" Block
   (lambda ($2 $1 . $rest)
     `(Modified (Modifier "static") ,$2))
   ;; ClassBodyDeclaration => Block
   (lambda ($1 . $rest) $1)
   ;; MemberDecl => Type VariableDeclarators ";"
   (lambda ($3 $2 $1 . $rest) `(MemberDecl ,$1 ,$2))
   ;; MemberDecl => Type Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest)
     `(MemberDecl ,$1 ,$2 ,@$3))
   ;; MemberDecl => "void" Identifier VoidMethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest)
     `(MemberDecl (void-type "void") ,$2 ,$3))
   ;; MemberDecl => Identifier ConstructorDeclaratorRest
   (lambda ($2 $1 . $rest)
     `(MemberDecl ,$1 ,@(cdr $2)))
   ;; MemberDecl => GenericMethodOrConstructorDecl
   (lambda ($1 . $rest) $1)
   ;; MemberDecl => ClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; MemberDecl => InterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; MethodDeclaratorRest => FormalParameters opt-Dims Block
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $2) (list $1 $2 $3) (list $1 $3)))
   ;; MethodDeclaratorRest => FormalParameters opt-Dims "throws" QualifiedI...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $2)
       (list $1 $2 $5 `(throws $4))
       (list $1 $5 `(throws $4))))
   ;; MethodDeclaratorRest => FormalParameters opt-Dims ";"
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $2) (list $1 $2) (list $1)))
   ;; MethodDeclaratorRest => FormalParameters opt-Dims "throws" QualifiedI...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $2)
       (list $1 $2 `(throws $4))
       (list $1 (throws $4))))
   ;; VoidMethodDeclaratorRest => FormalParameters Block
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; VoidMethodDeclaratorRest => FormalParameters "throws" QualifiedIdenti...
   (lambda ($4 $3 $2 $1 . $rest)
     (list $1 $4 `(throws ,$3)))
   ;; VoidMethodDeclaratorRest => FormalParameters ";"
   (lambda ($2 $1 . $rest) (list $1))
   ;; VoidMethodDeclaratorRest => FormalParameters "throws" QualifiedIdenti...
   (lambda ($4 $3 $2 $1 . $rest)
     (list $1 `(throws ,$3)))
   ;; ConstructorDeclaratorRest => FormalParameters Block
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; ConstructorDeclaratorRest => FormalParameters "throws" QualifiedIdent...
   (lambda ($4 $3 $2 $1 . $rest)
     (list $1 $4 `(throws ,$3)))
   ;; GenericMethodOrConstructorDecl => TypeParameters GenericMethodOrConst...
   (lambda ($2 $1 . $rest)
     (let loop ((out '()) (in $2))
       (if (member
             (sx-tag (car out))
             '(Identifier QualifiedIdentifier))
         (cons* (resverse out) $1 in)
         (loop (cons (car in) out) (cdr in)))))
   ;; GenericMethodOrConstructorRest => Type Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest) `(Method $1 $2 ,@$3))
   ;; GenericMethodOrConstructorRest => "void" Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest)
     `(Method (void "void") $2 ,@$3))
   ;; GenericMethodOrConstructorRest => Identifier ConstructorDeclaratorRest
   (lambda ($2 $1 . $rest) `(Constructor $2 ,@$2))
   ;; InterfaceBody => "{" InterfaceBodyDeclaration-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(Body ,@(sx-tail $2)))
   ;; InterfaceBody => "{" "}"
   (lambda ($2 $1 . $rest) `(Body))
   ;; InterfaceBodyDeclaration-list => InterfaceBodyDeclaration-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; InterfaceBodyDeclaration-list-1 => InterfaceBodyDeclaration
   (lambda ($1 . $rest) (make-tl 'ibl $1))
   ;; InterfaceBodyDeclaration-list-1 => InterfaceBodyDeclaration-list-1 In...
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; InterfaceBodyDeclaration => ";"
   (lambda ($1 . $rest) `(InterfaceDecl))
   ;; InterfaceBodyDeclaration => Modifier-list InterfaceMemberDecl
   (lambda ($2 $1 . $rest) `(Modified ,$1 ,$2))
   ;; InterfaceBodyDeclaration => InterfaceMemberDecl
   (lambda ($1 . $rest) $1)
   ;; Modifier-list => Modifier-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; Modifier-list-1 => Modifier
   (lambda ($1 . $rest) (make-tl 'Modifiers $1))
   ;; Modifier-list-1 => Modifier-list Modifier
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; InterfaceMemberDecl => Type Identifier ConstantDeclaratorRest ";"
   (lambda ($4 $3 $2 $1 . $rest) `(Decl $1 $2 ,@$3))
   ;; InterfaceMemberDecl => Type Identifier InterfaceMethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest) `(Decl $1 $2 ,@$3))
   ;; InterfaceMemberDecl => "void" Identifier VoidInterfaceMethodDeclarato...
   (lambda ($3 $2 $1 . $rest)
     `(MethodDecl (void "void") $2 ,@$3))
   ;; InterfaceMemberDecl => InterfaceGenericMethodDecl
   (lambda ($1 . $rest) $1)
   ;; InterfaceMemberDecl => ClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; InterfaceMemberDecl => InterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; ConstantDeclaratorsRest => ConstantDeclaratorsRest-1
   (lambda ($1 . $rest) (sx-tail (tl->list $1)))
   ;; ConstantDeclaratorsRest-1 => ConstantDeclaratorRest
   (lambda ($1 . $rest) (make-tl 'rest $1))
   ;; ConstantDeclaratorsRest-1 => ConstantDeclaratorsRest-1 "," ConstantDe...
   (lambda ($3 $2 $1 . $rest) (tl->append $1 $3))
   ;; ConstantDeclaratorRest => "=" VariableInitializer
   (lambda ($2 $1 . $rest) $1)
   ;; ConstantDeclaratorRest => Dims "=" VariableInitializer
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ConstantDeclarator => Identifier ConstantDeclaratorRest
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceMethodDeclaratorRest => FormalParameters opt-Dims ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InterfaceMethodDeclaratorRest => FormalParameters opt-Dims "throws" Q...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; VoidInterfaceMethodDeclaratorRest => FormalParameters ";"
   (lambda ($2 $1 . $rest) $1)
   ;; VoidInterfaceMethodDeclaratorRest => FormalParameters "throws" Qualif...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; InterfaceGenericMethodDecl => TypeParameters Type Identifier Interfac...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; InterfaceGenericMethodDecl => TypeParameters "void" Identifier Interf...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; FormalParameters => "(" FormalParameterDecls ")"
   (lambda ($3 $2 $1 . $rest)
     `(FormalParameters (tl->list $1)))
   ;; FormalParameters => "(" ")"
   (lambda ($2 $1 . $rest) `(FormalParameters))
   ;; FormalParameterDecls => Type VariableDeclaratorId
   (lambda ($2 $1 . $rest)
     `(make-tl 'FPDs `(param ,$1 ,$2)))
   ;; FormalParameterDecls => Type VariableDeclaratorId "," FormalParameter...
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-insert $3 `(param ,$1 ,$2)))
   ;; FormalParameterDecls => Type "..." VariableDeclaratorId
   (lambda ($3 $2 $1 . $rest)
     `(make-tl 'FPDs `(xxx-param ,$1 ,$3)))
   ;; VariableDeclaratorId => Identifier
   (lambda ($1 . $rest) $1)
   ;; VariableDeclaratorId => Identifier Dims
   (lambda ($2 $1 . $rest) `(array-decl-id ,$1 ,$2))
   ;; VariableDeclarators => VariableDeclarators-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; VariableDeclarators-1 => VariableDeclarator
   (lambda ($1 . $rest)
     (make-tl 'VariableDeclartors $1))
   ;; VariableDeclarators-1 => VariableDeclarators-1 "," VariableDeclarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; VariableDeclarator => Identifier
   (lambda ($1 . $rest) `(Declr ,$1))
   ;; VariableDeclarator => Identifier "=" VariableInitializer
   (lambda ($3 $2 $1 . $rest) `(Declr ,$1) ,$2)
   ;; VariableDeclarator => Identifier Dims
   (lambda ($2 $1 . $rest)
     `(Declr (Dimmed ,$1 ,$2)))
   ;; VariableDeclarator => Identifier Dims "=" VariableInitializer
   (lambda ($4 $3 $2 $1 . $rest)
     `(Declr (Dimmed ,$1 ,$2) ,$3))
   ;; VariableInitializer => ArrayInitializer
   (lambda ($1 . $rest) $1)
   ;; VariableInitializer => Expression
   (lambda ($1 . $rest) $1)
   ;; ArrayInitializer => "{" VariableInitializer-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(ArrayInitializer ,$2))
   ;; ArrayInitializer => "{" VariableInitializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ArrayInitializer ,$2))
   ;; ArrayInitializer => "{" "}"
   (lambda ($2 $1 . $rest)
     `(ArrayInitializer (VariableInitializers)))
   ;; VariableInitializer-list => VariableInitializer-list-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; VariableInitializer-list-1 => VariableInitializer
   (lambda ($1 . $rest)
     (make-tl 'VariableInitializers $1))
   ;; VariableInitializer-list-1 => VariableInitializer-list-1 "," Variable...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; Block => "{" BlockStatements "}"
   (lambda ($3 $2 $1 . $rest) `(Block ,(cdr $2)))
   ;; Block => "{" "}"
   (lambda ($2 $1 . $rest) '(Block))
   ;; BlockStatements => BlockStatements-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; BlockStatements-1 => BlockStatement
   (lambda ($1 . $rest)
     (make-tl 'BlockStatements $1))
   ;; BlockStatements-1 => BlockStatements-1 BlockStatement
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; BlockStatement => Type VariableDeclarators ";"
   (lambda ($3 $2 $1 . $rest) `(Decl ,$1 ,$2))
   ;; BlockStatement => "final" Type VariableDeclarators ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(Modified (Modifier "final") (Decl ,$2 ,$3)))
   ;; BlockStatement => ClassOrInterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; BlockStatement => Modifier-list ClassOrInterfaceDeclaration
   (lambda ($2 $1 . $rest) `(Modified ,$1 ,$2))
   ;; BlockStatement => Statement
   (lambda ($1 . $rest) $1)
   ;; Statement => Block
   (lambda ($1 . $rest) $1)
   ;; Statement => ";"
   (lambda ($1 . $rest) `(Statement))
   ;; Statement => Identifier ":" Statement
   (lambda ($3 $2 $1 . $rest)
     `(LabelledStatement ,$1 ,$3))
   ;; Statement => StatementExpression ";"
   (lambda ($2 $1 . $rest)
     `(StatementExpression ,$1))
   ;; Statement => "if" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$3 ,$5))
   ;; Statement => "if" "(" Expression ")" Statement "else" Statement
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$3 ,$5 ,$7))
   ;; Statement => "assert" Expression ";"
   (lambda ($3 $2 $1 . $rest) `(Assert ,$2))
   ;; Statement => "assert" Expression ":" Expression ";"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Assert ,$2 ,$5))
   ;; Statement => "switch" "(" Expression ")" "{" SwitchBlockStatementGrou...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(Switch ,$3 ,$6))
   ;; Statement => "while" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(While ,$3 ,$5))
   ;; Statement => "do" Statement "while" "(" Expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(Do ,$2 ,$5))
   ;; Statement => "for" "(" ForControl ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "break" ";"
   (lambda ($2 $1 . $rest) `(Break))
   ;; Statement => "break" Identifier ";"
   (lambda ($3 $2 $1 . $rest) `(Break ,$2))
   ;; Statement => "continue" ";"
   (lambda ($2 $1 . $rest) `(Continue))
   ;; Statement => "continue" Identifier ";"
   (lambda ($3 $2 $1 . $rest) `(Continue ,$2))
   ;; Statement => "return" Expression ";"
   (lambda ($3 $2 $1 . $rest) `(Return ,$2))
   ;; Statement => "return" ";"
   (lambda ($2 $1 . $rest) `(Return))
   ;; Statement => "throw" Expression ";"
   (lambda ($3 $2 $1 . $rest) `(Throw ,$2))
   ;; Statement => "synchronized" "(" Expression ")" Block
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Synchronized ,$3 ,$5))
   ;; Statement => "try" Block Catches Finally
   (lambda ($4 $3 $2 $1 . $rest) `(Try ,$2 ,$3 ,$4))
   ;; Statement => "try" Block Catches
   (lambda ($3 $2 $1 . $rest) `(Try ,$2 ,$3))
   ;; Statement => "try" Block Finally
   (lambda ($3 $2 $1 . $rest) `(Try ,$2 ,$3))
   ;; Statement => "try" ResourceSpecification Block Catches Finally
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Try ,$2 ,$3 ,$4 ,$5))
   ;; Statement => "try" ResourceSpecification Block Catches
   (lambda ($4 $3 $2 $1 . $rest) `(Try ,$2 ,$3 ,$4))
   ;; Statement => "try" ResourceSpecification Block Finally
   (lambda ($4 $3 $2 $1 . $rest) `(Try ,$2 ,$3 ,$4))
   ;; StatementExpression => QualifiedIdentifier Arguments
   (lambda ($2 $1 . $rest) `(CallStmt ,$1 ,$2))
   ;; StatementExpression => QualifiedIdentifier IdentifierSuffix Arguments
   (lambda ($3 $2 $1 . $rest)
     `(CallStmt ,$1 ,$2 ,$3))
   ;; StatementExpression => QualifiedIdentifier AssignmentOperator Express...
   (lambda ($3 $2 $1 . $rest)
     `(AssnStmt ,$1 ,$2 ,$3))
   ;; StatementExpression => QualifiedIdentifier IdentifierSuffix Assignmen...
   (lambda ($4 $3 $2 $1 . $rest)
     `(AssnStmt ,$1 ,$2 ,$3 ,$4))
   ;; StatementExpression => QualifiedIdentifier "++"
   (lambda ($2 $1 . $rest) `(PostIncrStmt ,$1))
   ;; StatementExpression => QualifiedIdentifier "--"
   (lambda ($2 $1 . $rest) `(PostDecrStmt ,$1))
   ;; StatementExpression => "++" QualifiedIdentifier
   (lambda ($2 $1 . $rest) `(PreIncrStmt ,$1))
   ;; StatementExpression => "--" QualifiedIdentifier
   (lambda ($2 $1 . $rest) `(PreDecrStmt ,$1))
   ;; Catches => Catches-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; Catches-1 => CatchClause
   (lambda ($1 . $rest) (make-tl 'Catches $1))
   ;; Catches-1 => Catches-1 CatchClause
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; CatchClause => "catch" "(" CatchType Identifier ")" Block
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(CatchClause ,$3 ,$4 ,$6))
   ;; CatchType => CatchType-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; CatchType-1 => QualifiedIdentifier
   (lambda ($1 . $rest) (make-tl 'CatchType $1))
   ;; CatchType-1 => CatchType-1 "|" QualifiedIdentifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; Finally => "finally" Block
   (lambda ($2 $1 . $rest) `(Finally ,$2))
   ;; ResourceSpecification => "(" Resources ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; ResourceSpecification => "(" Resources ";" ")"
   (lambda ($4 $3 $2 $1 . $rest) $2)
   ;; Resources => Resources-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; Resources-1 => Resource
   (lambda ($1 . $rest) (make-tl 'Resources $1))
   ;; Resources-1 => Resources-1 ";" Resource
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; Resource => "final" ReferenceType VariableDeclaratorId "=" Expression
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(Modified "final" (Resource ,$2 ,$3 ,$5)))
   ;; Resource => ReferenceType VariableDeclaratorId "=" Expression
   (lambda ($4 $3 $2 $1 . $rest)
     `(Resource ,$2 ,$3 ,$5))
   ;; SwitchBlockStatementGroups => SwitchBlockStatementGroups-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; SwitchBlockStatementGroups-1 => SwitchBlockStatementGroup
   (lambda ($1 . $rest)
     (make-tl 'SwitchBlockStatementGroups $1))
   ;; SwitchBlockStatementGroups-1 => SwitchBlockStatementGroups-1 SwitchBl...
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; SwitchBlockStatementGroup => SwitchLabels BlockStatements
   (lambda ($2 $1 . $rest)
     `(SwitchBlockStatementGroup ,$1 ,$2))
   ;; SwitchLabels => SwitchLabels-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; SwitchLabels-1 => SwitchLabel
   (lambda ($1 . $rest) (make-tl 'SwitchLabels $1))
   ;; SwitchLabels-1 => SwitchLabels-1 SwitchLabel
   (lambda ($2 $1 . $rest) (tl->append $1 $2))
   ;; SwitchLabel => "case" Expression ":"
   (lambda ($3 $2 $1 . $rest) `(SwitchLabel ,$2))
   ;; SwitchLabel => "default" ":"
   (lambda ($2 $1 . $rest) `(SwitchLabel (default)))
   ;; ForControl => ForVarControl
   (lambda ($1 . $rest) $1)
   ;; ForControl => ForInit ";" opt-Expression ";" opt-ForUpdate
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; opt-Expression => 
   (lambda $rest (list))
   ;; opt-Expression => Expression
   (lambda ($1 . $rest) $1)
   ;; opt-ForUpdate => 
   (lambda $rest (list))
   ;; opt-ForUpdate => ForUpdate
   (lambda ($1 . $rest) $1)
   ;; ForVarControl => Type VariableDeclaratorId ForVarControlRest
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ForVarControlRest => ForVariableDeclaratorsRest ";" opt-Expression ";...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; ForVarControlRest => ":" Expression
   (lambda ($2 $1 . $rest) $1)
   ;; ForVariableDeclaratorsRest => 
   (lambda $rest (list))
   ;; ForVariableDeclaratorsRest => "=" VariableInitializer
   (lambda ($2 $1 . $rest) $1)
   ;; ForVariableDeclaratorsRest => "=" VariableInitializer "," VariableDec...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; ForVariableDeclaratorsRest => "," VariableDeclarator-list
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclarator-list => VariableDeclarator
   (lambda ($1 . $rest) $1)
   ;; VariableDeclarator-list => VariableDeclarator-list "," VariableDeclar...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ForInit => ForUpdate
   (lambda ($1 . $rest) $1)
   ;; ForUpdate => StatementExpression
   (lambda ($1 . $rest) $1)
   ;; ForUpdate => ForUpdate "," StatementExpression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Expression => Expression1
   (lambda ($1 . $rest) $1)
   ;; Expression => Expression1 AssignmentOperator Expression1
   (lambda ($3 $2 $1 . $rest)
     `(assn-expr $1 (op ,$2) $3))
   ;; AssignmentOperator => "="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "+="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "-="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "*="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "/="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "&="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "|="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "^="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "%="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => "<<="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => ">>="
   (lambda ($1 . $rest) $1)
   ;; AssignmentOperator => ">>>="
   (lambda ($1 . $rest) $1)
   ;; Expression1 => Expression2
   (lambda ($1 . $rest) $1)
   ;; Expression1 => Expression2 "?" Expression ":" Expression1
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr ,$1 ,$3 ,$5))
   ;; Expression2 => Expression3
   (lambda ($1 . $rest) $1)
   ;; Expression2 => Expression3 "instanceof" Type
   (lambda ($3 $2 $1 . $rest) `(instanceof ,$1 ,$3))
   ;; Expression2 => Expression2 InfixOp Expression3
   (lambda ($3 $2 $1 . $rest) (list $2 $1 $3))
   ;; InfixOp => "||"
   (lambda ($1 . $rest) 'or)
   ;; InfixOp => "&&"
   (lambda ($1 . $rest) 'and)
   ;; InfixOp => "|"
   (lambda ($1 . $rest) 'bitwise-or)
   ;; InfixOp => "^"
   (lambda ($1 . $rest) 'bitwise-xor)
   ;; InfixOp => "&"
   (lambda ($1 . $rest) 'bitwise-and)
   ;; InfixOp => "=="
   (lambda ($1 . $rest) 'eq)
   ;; InfixOp => "!="
   (lambda ($1 . $rest) 'ne)
   ;; InfixOp => "<"
   (lambda ($1 . $rest) 'lt)
   ;; InfixOp => ">"
   (lambda ($1 . $rest) 'gt)
   ;; InfixOp => "<="
   (lambda ($1 . $rest) 'le)
   ;; InfixOp => ">="
   (lambda ($1 . $rest) 'ge)
   ;; InfixOp => "<<"
   (lambda ($1 . $rest) 'lshift)
   ;; InfixOp => ">>"
   (lambda ($1 . $rest) 'rshift)
   ;; InfixOp => ">>>"
   (lambda ($1 . $rest) 'rrshift)
   ;; InfixOp => "+"
   (lambda ($1 . $rest) 'add)
   ;; InfixOp => "-"
   (lambda ($1 . $rest) 'sub)
   ;; InfixOp => "*"
   (lambda ($1 . $rest) 'mul)
   ;; InfixOp => "/"
   (lambda ($1 . $rest) 'div)
   ;; InfixOp => "%"
   (lambda ($1 . $rest) 'mod)
   ;; Expression3 => PrefixOp Expression3
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; Expression3 => Primary
   (lambda ($1 . $rest) $1)
   ;; Expression3 => Primary PostfixOp
   (lambda ($2 $1 . $rest) (list $2 $1))
   ;; Selector-list => Selector
   (lambda ($1 . $rest) $1)
   ;; Selector-list => Selector-list Selector
   (lambda ($2 $1 . $rest) $1)
   ;; PrefixOp => "++"
   (lambda ($1 . $rest) 'pre-inc)
   ;; PrefixOp => "--"
   (lambda ($1 . $rest) 'pre-dec)
   ;; PrefixOp => "!"
   (lambda ($1 . $rest) 'not)
   ;; PrefixOp => "~"
   (lambda ($1 . $rest) 'bitwise-not)
   ;; PrefixOp => "+"
   (lambda ($1 . $rest) 'pos)
   ;; PrefixOp => "-"
   (lambda ($1 . $rest) 'neg)
   ;; PostfixOp => "++"
   (lambda ($1 . $rest) 'post-inc)
   ;; PostfixOp => "--"
   (lambda ($1 . $rest) 'post-dec)
   ;; Primary => Literal
   (lambda ($1 . $rest) $1)
   ;; Primary => "(" Expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; Primary => "this"
   (lambda ($1 . $rest) '(this))
   ;; Primary => "this" Arguments
   (lambda ($2 $1 . $rest) `(this ,$2))
   ;; Primary => "super" SuperSuffix
   (lambda ($2 $1 . $rest) `(super ,$2))
   ;; Primary => "new" Creator
   (lambda ($2 $1 . $rest) `(new ,$2))
   ;; Primary => NonWildcardTypeArguments "this" Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Primary => NonWildcardTypeArguments "super" SuperSuffix
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Primary => NonWildcardTypeArguments Identifier Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Primary => QualifiedIdentifier
   (lambda ($1 . $rest) $1)
   ;; Primary => QualifiedIdentifier IdentifierSuffix
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => QualifiedIdentifier Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => BasicType opt-Dims "." "class"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Primary => "void" "." "class"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Literal => IntegerLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => FloatingPointLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => CharacterLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => StringLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => BooleanLiteral
   (lambda ($1 . $rest) $1)
   ;; Literal => NullLiteral
   (lambda ($1 . $rest) $1)
   ;; Arguments => "(" Expression-list ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Arguments => "(" ")"
   (lambda ($2 $1 . $rest) $1)
   ;; Expression-list => Expression
   (lambda ($1 . $rest) $1)
   ;; Expression-list => Expression-list "," Expression
   (lambda ($3 $2 $1 . $rest) $1)
   ;; SuperSuffix => Arguments
   (lambda ($1 . $rest) $1)
   ;; SuperSuffix => "." Identifier Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; SuperSuffix => "." Identifier
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => NonWildcardTypeArguments CreatedName Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Creator => NonWildcardTypeArguments CreatedName Arguments ClassBody
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Creator => CreatedName Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => CreatedName Arguments ClassBody
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Creator => CreatedName DimExprs
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => CreatedName Dims
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => CreatedName Dims ArrayInitializer
   (lambda ($3 $2 $1 . $rest) $1)
   ;; CreatedName => Identifier
   (lambda ($1 . $rest) $1)
   ;; CreatedName => Identifier TypeArgumentsOrDiamond
   (lambda ($2 $1 . $rest) $1)
   ;; CreatedName => CreatedName "." Identifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; CreatedName => CreatedName "." Identifier TypeArgumentsOrDiamond
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; DimExprs => DimExprs-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; DimExprs-1 => "[" Expression "]"
   (lambda ($3 $2 $1 . $rest)
     (make-tl 'DimExprs `(DimExpr ,$2)))
   ;; DimExprs-1 => DimExprs-1 "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 `(DimExpr ,$3)))
   ;; DimExprs-1 => DimExprs-1 "[" "]"
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 `(Dim "[]")))
   ;; IdentifierSuffix => "." "class"
   (lambda ($2 $1 . $rest) $1)
   ;; IdentifierSuffix => "." "this"
   (lambda ($2 $1 . $rest) $1)
   ;; IdentifierSuffix => "." "super" Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; IdentifierSuffix => "." "new" InnerCreator
   (lambda ($3 $2 $1 . $rest) $1)
   ;; IdentifierSuffix => "." "new" NonWildcardTypeArguments InnerCreator
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; ExplicitGenericInvocation => NonWildcardTypeArguments "super" SuperSu...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ExplicitGenericInvocation => NonWildcardTypeArguments Identifier Argu...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InnerCreator => Identifier Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; InnerCreator => Identifier Arguments ClassBody
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InnerCreator => Identifier NonWildcardTypeArgumentsOrDiamond Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InnerCreator => Identifier NonWildcardTypeArgumentsOrDiamond Argument...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Selector => "." Identifier
   (lambda ($2 $1 . $rest) $1)
   ;; Selector => "." Identifier Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Selector => "." ExplicitGenericInvocation
   (lambda ($2 $1 . $rest) $1)
   ;; Selector => "." "this"
   (lambda ($2 $1 . $rest) $1)
   ;; Selector => "." "super" SuperSuffix
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Selector => "." "new" InnerCreator
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Selector => "[" Expression "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; EnumBody => "{" "}"
   (lambda ($2 $1 . $rest) `(EnumBody))
   ;; EnumBody => "{" EnumConstants "}"
   (lambda ($3 $2 $1 . $rest) `(EnumBody ,$2))
   ;; EnumBody => "{" EnumConstants "," "}"
   (lambda ($4 $3 $2 $1 . $rest) `(EnumBody ,$2))
   ;; EnumBody => "{" EnumConstants ";" EnumBodyDeclarations "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(EnumBody ,$2 ,$4))
   ;; EnumBody => "{" EnumConstants "," ";" EnumBodyDeclarations "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(EnumBody ,$2 ,$5))
   ;; EnumBody => "{" ";" EnumBodyDeclarations "}"
   (lambda ($4 $3 $2 $1 . $rest) `(EnumBody ,$3))
   ;; EnumConstants => EnumConstants-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; EnumConstants-1 => EnumConstant
   (lambda ($1 . $rest) (make-tl 'EnumConstants $1))
   ;; EnumConstants-1 => EnumConstants-1 "," EnumConstant
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; EnumBodyDeclarations => EnumBodyDeclarations-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; EnumBodyDeclarations-1 => ClassBodyDeclaration
   (lambda ($1 . $rest)
     (make-tl 'EnumBodyDeclarations $1))
   ;; EnumBodyDeclarations-1 => EnumBodyDeclarations-1 ClassBodyDeclaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; EnumConstant => Identifier opt-Arguments opt-ClassBody
   (lambda ($3 $2 $1 . $rest)
     (let* ((tail (if (pair? opt-ClassBody)
                    (list opt-ClassBody)
                    '()))
            (tail (if (pair? opt-Arguments)
                    (append opt-Arguments tail))))
       `(EnumConstant ,$1 unquote tail)))
   ;; opt-Arguments => 
   (lambda $rest (list))
   ;; opt-Arguments => Arguments
   (lambda ($1 . $rest) $1)
   ;; opt-ClassBody => 
   (lambda $rest (list))
   ;; opt-ClassBody => ClassBody
   (lambda ($1 . $rest) $1)
   ;; IntegerLiteral => '$fixed
   (lambda ($1 . $rest) `(IntegerLiteral ,$1))
   ;; FloatingPointLiteral => '$float
   (lambda ($1 . $rest) `(FloatingPointLiteral ,$1))
   ;; CharacterLiteral => '$chlit
   (lambda ($1 . $rest) `(CharacterLiteral ,$1))
   ;; StringLiteral => '$string
   (lambda ($1 . $rest) `(StringLiteral ,$1))
   ;; BooleanLiteral => "true"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; BooleanLiteral => "false"
   (lambda ($1 . $rest) `(BooleanLiteral ,$1))
   ;; NullLiteral => "null"
   (lambda ($1 . $rest) `(NullLiteral ,$1))
   ;; Annotations => Annotation
   (lambda ($1 . $rest) $1)
   ;; Annotations => Annotations Annotation
   (lambda ($2 $1 . $rest) $1)
   ;; Annotation => "@" QualifiedIdentifier
   (lambda ($2 $1 . $rest) $1)
   ;; Annotation => "@" QualifiedIdentifier "(" ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Annotation => "@" QualifiedIdentifier "(" AnnotationElement ")"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; AnnotationElement => ElementValuePairs
   (lambda ($1 . $rest) $1)
   ;; AnnotationElement => ElementValue
   (lambda ($1 . $rest) $1)
   ;; AnnotationTypeDeclaration => "@" "interface" Identifier AnnotationTyp...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; AnnotationTypeBody => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; AnnotationTypeBody => "{" AnnotationTypeElementDeclarations "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; AnnotationTypeElementDeclarations => AnnotationTypeElementDeclaration
   (lambda ($1 . $rest) $1)
   ;; AnnotationTypeElementDeclarations => AnnotationTypeElementDeclaration...
   (lambda ($2 $1 . $rest) $1)
   ;; AnnotationTypeElementDeclaration => Modifier-list AnnotationTypeEleme...
   (lambda ($2 $1 . $rest) $1)
   ;; AnnotationTypeElementRest => Type Identifier AnnotationMethodOrConsta...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; AnnotationTypeElementRest => ClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; AnnotationTypeElementRest => InterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; AnnotationTypeElementRest => EnumDeclaration
   (lambda ($1 . $rest) $1)
   ;; AnnotationTypeElementRest => AnnotationTypeDeclaration
   (lambda ($1 . $rest) $1)
   ;; AnnotationMethodOrConstantRest => AnnotationMethodRest
   (lambda ($1 . $rest) $1)
   ;; AnnotationMethodOrConstantRest => ConstantDeclaratorsRest
   (lambda ($1 . $rest) $1)
   ;; AnnotationMethodRest => "(" ")" opt-Dims
   (lambda ($3 $2 $1 . $rest) $1)
   ;; AnnotationMethodRest => "(" ")" opt-Dims "default" ElementValue
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ))

;;; end tables
