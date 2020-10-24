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
   ;; ImportDeclaration => "import" "static" QualifiedIdentifier "." "*" ";"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(ImportDeclaration
        (Modifier "static")
        ,(append $3 '(glob "*"))))
   ;; ImportDeclaration => "import" "static" QualifiedIdentifier ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ImportDeclaration (Modifier "static") ,$3))
   ;; ImportDeclaration => "import" QualifiedIdentifier "." "*" ";"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(ImportDeclaration ,(append $3 '(glob "*"))))
   ;; ImportDeclaration => "import" QualifiedIdentifier ";"
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
   ;; Type => BasicType arrays
   (lambda ($2 $1 . $rest) (append $1 (list $2)))
   ;; Type => QualifiedIdentifier
   (lambda ($1 . $rest) $1)
   ;; Type => QualifiedIdentifier arrays
   (lambda ($2 $1 . $rest) (append $1 (list $2)))
   ;; opt-arrays => 
   (lambda $rest (list))
   ;; opt-arrays => arrays
   (lambda ($1 . $rest) $1)
   ;; arrays => arrays-1
   (lambda ($1 . $rest) (tl->list $1))
   ;; arrays-1 => "[" "]"
   (lambda ($2 $1 . $rest)
     (make-tl 'arrays `(array "[]")))
   ;; arrays-1 => arrays-1 "[" "]"
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 `(array "[]")))
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
   (lambda ($2 $1 . $rest) `(Modified ,$2 ,$1))
   ;; ClassBodyDeclaration => "static" Block
   (lambda ($2 $1 . $rest)
     `(Modified ,$2 (Modifier "static")))
   ;; ClassBodyDeclaration => Block
   (lambda ($1 . $rest) $1)
   ;; MemberDecl => Type Identifier FieldDeclaratorsRest ";"
   (lambda ($4 $3 $2 $1 . $rest)
     `(MemberDecl ,$1 ,$2 ,$3))
   ;; MemberDecl => Type Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest)
     `(MemberDecl ,$1 ,$2 ,$3))
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
   ;; FieldDeclaratorsRest => FieldDeclaratorsRest-1
   (lambda ($1 . $rest) $1)
   ;; FieldDeclaratorsRest-1 => VariableDeclaratorRest
   (lambda ($1 . $rest) (make-tl 'list $1))
   ;; FieldDeclaratorsRest-1 => FieldDeclaratorsRest-1 "," VariableDeclarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; MethodDeclaratorRest => FormalParameters opt-arrays Block
   (lambda ($3 $2 $1 . $rest) $1)
   ;; MethodDeclaratorRest => FormalParameters opt-arrays "throws" Qualifie...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; MethodDeclaratorRest => FormalParameters opt-arrays ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; MethodDeclaratorRest => FormalParameters opt-arrays "throws" Qualifie...
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; VoidMethodDeclaratorRest => FormalParameters Block
   (lambda ($2 $1 . $rest) $1)
   ;; VoidMethodDeclaratorRest => FormalParameters "throws" QualifiedIdenti...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; VoidMethodDeclaratorRest => FormalParameters ";"
   (lambda ($2 $1 . $rest) $1)
   ;; VoidMethodDeclaratorRest => FormalParameters "throws" QualifiedIdenti...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; ConstructorDeclaratorRest => FormalParameters Block
   (lambda ($2 $1 . $rest) $1)
   ;; ConstructorDeclaratorRest => FormalParameters "throws" QualifiedIdent...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; GenericMethodOrConstructorDecl => TypeParameters GenericMethodOrConst...
   (lambda ($2 $1 . $rest) $1)
   ;; GenericMethodOrConstructorRest => Type Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest) $1)
   ;; GenericMethodOrConstructorRest => "void" Identifier MethodDeclaratorRest
   (lambda ($3 $2 $1 . $rest) $1)
   ;; GenericMethodOrConstructorRest => Identifier ConstructorDeclaratorRest
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceBody => "{" InterfaceBodyDeclaration-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InterfaceBody => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceBodyDeclaration-list => InterfaceBodyDeclaration
   (lambda ($1 . $rest) $1)
   ;; InterfaceBodyDeclaration-list => InterfaceBodyDeclaration-list Interf...
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceBodyDeclaration => ";"
   (lambda ($1 . $rest) $1)
   ;; InterfaceBodyDeclaration => Modifier-list InterfaceMemberDecl
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceBodyDeclaration => InterfaceMemberDecl
   (lambda ($1 . $rest) $1)
   ;; Modifier-list => Modifier
   (lambda ($1 . $rest) $1)
   ;; Modifier-list => Modifier-list Modifier
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceMemberDecl => InterfaceMethodOrFieldDecl
   (lambda ($1 . $rest) $1)
   ;; InterfaceMemberDecl => "void" Identifier VoidInterfaceMethodDeclarato...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InterfaceMemberDecl => InterfaceGenericMethodDecl
   (lambda ($1 . $rest) $1)
   ;; InterfaceMemberDecl => ClassDeclaration
   (lambda ($1 . $rest) $1)
   ;; InterfaceMemberDecl => InterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; InterfaceMethodOrFieldDecl => Type Identifier InterfaceMethodOrFieldRest
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InterfaceMethodOrFieldRest => ConstantDeclaratorsRest ";"
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceMethodOrFieldRest => InterfaceMethodDeclaratorRest
   (lambda ($1 . $rest) $1)
   ;; ConstantDeclaratorsRest => ConstantDeclaratorRest
   (lambda ($1 . $rest) $1)
   ;; ConstantDeclaratorsRest => ConstantDeclaratorsRest "," ConstantDeclar...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ConstantDeclaratorRest => "=" VariableInitializer
   (lambda ($2 $1 . $rest) $1)
   ;; ConstantDeclaratorRest => arrays "=" VariableInitializer
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ConstantDeclarator => Identifier ConstantDeclaratorRest
   (lambda ($2 $1 . $rest) $1)
   ;; InterfaceMethodDeclaratorRest => FormalParameters opt-arrays ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InterfaceMethodDeclaratorRest => FormalParameters opt-arrays "throws"...
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
   (lambda ($3 $2 $1 . $rest) $1)
   ;; FormalParameters => "(" ")"
   (lambda ($2 $1 . $rest) $1)
   ;; FormalParameterDecls => Type FormalParameterDeclsRest
   (lambda ($2 $1 . $rest) $1)
   ;; FormalParameterDeclsRest => VariableDeclaratorId
   (lambda ($1 . $rest) $1)
   ;; FormalParameterDeclsRest => VariableDeclaratorId "," FormalParameterD...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; FormalParameterDeclsRest => "..." VariableDeclaratorId
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclaratorId => Identifier
   (lambda ($1 . $rest) $1)
   ;; VariableDeclaratorId => Identifier arrays
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclarators => VariableDeclarator
   (lambda ($1 . $rest) $1)
   ;; VariableDeclarators => VariableDeclarators "," VariableDeclarator
   (lambda ($3 $2 $1 . $rest) $1)
   ;; VariableDeclarators-tail => "," VariableDeclarator
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclarators-tail => VariableDeclarators-tail "," VariableDecl...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; VariableDeclarator => Identifier VariableDeclaratorRest
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclaratorRest => 
   (lambda $rest (list))
   ;; VariableDeclaratorRest => "=" VariableInitializer
   (lambda ($2 $1 . $rest) $1)
   ;; VariableDeclaratorRest => arrays
   (lambda ($1 . $rest) $1)
   ;; VariableDeclaratorRest => arrays "=" VariableInitializer
   (lambda ($3 $2 $1 . $rest) $1)
   ;; VariableInitializer => ArrayInitializer
   (lambda ($1 . $rest) $1)
   ;; VariableInitializer => Expression
   (lambda ($1 . $rest) $1)
   ;; ArrayInitializer => "{" VariableInitializer-list "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ArrayInitializer => "{" VariableInitializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; ArrayInitializer => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; VariableInitializer-list => VariableInitializer
   (lambda ($1 . $rest) $1)
   ;; VariableInitializer-list => VariableInitializer-list "," VariableInit...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Block => "{" BlockStatements "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Block => "{" "}"
   (lambda ($2 $1 . $rest) $1)
   ;; BlockStatements => BlockStatement
   (lambda ($1 . $rest) $1)
   ;; BlockStatements => BlockStatements BlockStatement
   (lambda ($2 $1 . $rest) $1)
   ;; BlockStatement => Type VariableDeclarators ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; BlockStatement => "final" Type VariableDeclarators ";"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; BlockStatement => ClassOrInterfaceDeclaration
   (lambda ($1 . $rest) $1)
   ;; BlockStatement => Modifier-list ClassOrInterfaceDeclaration
   (lambda ($2 $1 . $rest) $1)
   ;; BlockStatement => Statement
   (lambda ($1 . $rest) $1)
   ;; Statement => Block
   (lambda ($1 . $rest) $1)
   ;; Statement => ";"
   (lambda ($1 . $rest) $1)
   ;; Statement => Identifier ":" Statement
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => StatementExpression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; Statement => "if" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "if" "(" Expression ")" Statement "else" Statement
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "assert" Expression ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "assert" Expression ":" Expression ";"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "switch" "(" Expression ")" "{" SwitchBlockStatementGrou...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "while" "(" Expression ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "do" Statement "while" "(" Expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "for" "(" ForControl ")" Statement
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "break" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; Statement => "break" Identifier ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "continue" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; Statement => "continue" Identifier ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "return" Expression ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "return" ";"
   (lambda ($2 $1 . $rest) $1)
   ;; Statement => "throw" Expression ";"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "synchronized" "(" Expression ")" Block
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "try" Block Catches Finally
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Statement => "try" Block Catches
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "try" Block Finally
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Statement => "try" ResourceSpecification Block Catches Finally
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Statement => "try" ResourceSpecification Block Catches
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Statement => "try" ResourceSpecification Block Finally
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier IdentifierSuffix Arguments
   (lambda ($3 $2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier AssignmentOperator Express...
   (lambda ($3 $2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier IdentifierSuffix Assignmen...
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier "++"
   (lambda ($2 $1 . $rest) $1)
   ;; StatementExpression => QualifiedIdentifier "--"
   (lambda ($2 $1 . $rest) $1)
   ;; StatementExpression => "++" QualifiedIdentifier
   (lambda ($2 $1 . $rest) $1)
   ;; StatementExpression => "--" QualifiedIdentifier
   (lambda ($2 $1 . $rest) $1)
   ;; Catches => CatchClause
   (lambda ($1 . $rest) $1)
   ;; Catches => Catches CatchClause
   (lambda ($2 $1 . $rest) $1)
   ;; CatchClause => "catch" "(" CatchType Identifier ")" Block
   (lambda ($6 $5 $4 $3 $2 $1 . $rest) $1)
   ;; CatchType => QualifiedIdentifier
   (lambda ($1 . $rest) $1)
   ;; CatchType => CatchType "|" QualifiedIdentifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Finally => "finally" Block
   (lambda ($2 $1 . $rest) $1)
   ;; ResourceSpecification => "(" Resources ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; ResourceSpecification => "(" Resources ";" ")"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Resources => Resource
   (lambda ($1 . $rest) $1)
   ;; Resources => Resources ";" Resource
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Resource => "final" ReferenceType VariableDeclaratorId "=" Expression
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; Resource => ReferenceType VariableDeclaratorId "=" Expression
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; SwitchBlockStatementGroups => SwitchBlockStatementGroup
   (lambda ($1 . $rest) $1)
   ;; SwitchBlockStatementGroups => SwitchBlockStatementGroups SwitchBlockS...
   (lambda ($2 $1 . $rest) $1)
   ;; SwitchBlockStatementGroup => SwitchLabels BlockStatements
   (lambda ($2 $1 . $rest) $1)
   ;; SwitchLabels => SwitchLabel
   (lambda ($1 . $rest) $1)
   ;; SwitchLabels => SwitchLabels SwitchLabel
   (lambda ($2 $1 . $rest) $1)
   ;; SwitchLabel => "case" Expression ":"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; SwitchLabel => "default" ":"
   (lambda ($2 $1 . $rest) $1)
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
   (lambda ($3 $2 $1 . $rest) $1)
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
   ;; Expression1 => Expression2 Expression1Rest
   (lambda ($2 $1 . $rest) $1)
   ;; Expression1Rest => "?" Expression ":" Expression1
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; Expression2 => Expression3
   (lambda ($1 . $rest) $1)
   ;; Expression2 => Expression3 "instanceof" Type
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Expression2 => Expression2 InfixOp Expression3
   (lambda ($3 $2 $1 . $rest) $1)
   ;; InfixOp => "||"
   (lambda ($1 . $rest) `(or ,$1))
   ;; InfixOp => "&&"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "|"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "^"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "&"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "=="
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "!="
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "<"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => ">"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "<="
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => ">="
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "<<"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => ">>"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => ">>>"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "+"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "-"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "*"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "/"
   (lambda ($1 . $rest) `(x ,$1))
   ;; InfixOp => "%"
   (lambda ($1 . $rest) `(x ,$1))
   ;; Expression3 => PrefixOp Expression3
   (lambda ($2 $1 . $rest) $1)
   ;; Expression3 => Primary
   (lambda ($1 . $rest) $1)
   ;; Expression3 => Primary PostfixOp
   (lambda ($2 $1 . $rest) $1)
   ;; Selector-list => Selector
   (lambda ($1 . $rest) $1)
   ;; Selector-list => Selector-list Selector
   (lambda ($2 $1 . $rest) $1)
   ;; PrefixOp => "++"
   (lambda ($1 . $rest) $1)
   ;; PrefixOp => "--"
   (lambda ($1 . $rest) $1)
   ;; PrefixOp => "!"
   (lambda ($1 . $rest) $1)
   ;; PrefixOp => "~"
   (lambda ($1 . $rest) $1)
   ;; PrefixOp => "+"
   (lambda ($1 . $rest) $1)
   ;; PrefixOp => "-"
   (lambda ($1 . $rest) $1)
   ;; PostfixOp => "++"
   (lambda ($1 . $rest) $1)
   ;; PostfixOp => "--"
   (lambda ($1 . $rest) $1)
   ;; Primary => Literal
   (lambda ($1 . $rest) $1)
   ;; Primary => "(" Expression ")"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; Primary => "this"
   (lambda ($1 . $rest) $1)
   ;; Primary => "this" Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => "super" SuperSuffix
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => "new" Creator
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => QualifiedIdentifier
   (lambda ($1 . $rest) $1)
   ;; Primary => QualifiedIdentifier IdentifierSuffix
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => QualifiedIdentifier Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; Primary => BasicType opt-arrays "." "class"
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
   ;; ExplicitGenericInvocationSuffix => "super" SuperSuffix
   (lambda ($2 $1 . $rest) $1)
   ;; ExplicitGenericInvocationSuffix => Identifier Arguments
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => CreatedName ClassCreatorRest
   (lambda ($2 $1 . $rest) $1)
   ;; Creator => CreatedName ArrayCreatorRest
   (lambda ($2 $1 . $rest) $1)
   ;; CreatedName => Identifier
   (lambda ($1 . $rest) $1)
   ;; CreatedName => Identifier TypeArgumentsOrDiamond
   (lambda ($2 $1 . $rest) $1)
   ;; CreatedName => CreatedName "." Identifier
   (lambda ($3 $2 $1 . $rest) $1)
   ;; CreatedName => CreatedName "." Identifier TypeArgumentsOrDiamond
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; ClassCreatorRest => Arguments
   (lambda ($1 . $rest) $1)
   ;; ClassCreatorRest => Arguments ClassBody
   (lambda ($2 $1 . $rest) $1)
   ;; ArrayCreatorRest => array-expr-list opt-arrays
   (lambda ($2 $1 . $rest) $1)
   ;; ArrayCreatorRest => arrays
   (lambda ($1 . $rest) $1)
   ;; array-expr-list => "[" Expression "]"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; array-expr-list => array-expr-list "[" Expression "]"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; IdentifierSuffix => "[" "." "class" "]"
   (lambda ($4 $3 $2 $1 . $rest) $1)
   ;; IdentifierSuffix => "[" arrays "." "class" "]"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; IdentifierSuffix => "[" Expression "]"
   (lambda ($3 $2 $1 . $rest) $1)
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
   ;; ExplicitGenericInvocation => NonWildcardTypeArguments ExplicitGeneric...
   (lambda ($2 $1 . $rest) $1)
   ;; InnerCreator => Identifier ClassCreatorRest
   (lambda ($2 $1 . $rest) $1)
   ;; InnerCreator => Identifier NonWildcardTypeArgumentsOrDiamond ClassCre...
   (lambda ($3 $2 $1 . $rest) $1)
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
   (lambda ($2 $1 . $rest) $1)
   ;; EnumBody => "{" EnumConstants "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; EnumBody => "{" EnumConstants "," EnumBodyDeclarations "}"
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ;; EnumBody => "{" EnumBodyDeclarations "}"
   (lambda ($3 $2 $1 . $rest) $1)
   ;; EnumConstants => EnumConstant
   (lambda ($1 . $rest) $1)
   ;; EnumConstants => EnumConstants "," EnumConstant
   (lambda ($3 $2 $1 . $rest) $1)
   ;; EnumConstant => Identifier opt-Arguments opt-ClassBody
   (lambda ($3 $2 $1 . $rest) $1)
   ;; opt-Arguments => 
   (lambda $rest (list))
   ;; opt-Arguments => Arguments
   (lambda ($1 . $rest) $1)
   ;; opt-ClassBody => 
   (lambda $rest (list))
   ;; opt-ClassBody => ClassBody
   (lambda ($1 . $rest) $1)
   ;; EnumBodyDeclarations => ";" ClassBodyDeclaration
   (lambda ($2 $1 . $rest) $1)
   ;; EnumBodyDeclarations => EnumBodyDeclarations ClassBodyDeclaration
   (lambda ($2 $1 . $rest) $1)
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
   ;; AnnotationMethodRest => "(" ")" opt-arrays
   (lambda ($3 $2 $1 . $rest) $1)
   ;; AnnotationMethodRest => "(" ")" opt-arrays "default" ElementValue
   (lambda ($5 $4 $3 $2 $1 . $rest) $1)
   ))

;;; end tables
