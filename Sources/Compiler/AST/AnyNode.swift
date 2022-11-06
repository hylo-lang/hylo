import Utils

/// A type-erasing container for AST nodes.
struct AnyNode: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case kind, data

  }

  /// The wrapped node.
  let node: Node

  /// Creates a type-erased container that wraps `node`.
  init(_ node: Node) {
    self.node = node
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)

    let kind = try container.decode(NodeKind.self, forKey: .kind)
    switch kind {
    case .associatedTypeDecl        : node = try container.decode(AssociatedTypeDecl.self)
    case .associatedValueDecl       : node = try container.decode(AssociatedValueDecl.self)
    case .bindingDecl               : node = try container.decode(BindingDecl.self)
    case .builtinDecl               : node = try container.decode(BuiltinDecl.self)
    case .conformanceDecl           : node = try container.decode(ConformanceDecl.self)
    case .extensionDecl             : node = try container.decode(ExtensionDecl.self)
    case .functionDecl              : node = try container.decode(FunctionDecl.self)
    case .genericTypeParamDecl      : node = try container.decode(GenericTypeParamDecl.self)
    case .genericValueParamDecl     : node = try container.decode(GenericValueParamDecl.self)
    case .importDecl                : node = try container.decode(ImportDecl.self)
    case .methodDecl                : node = try container.decode(MethodDecl.self)
    case .methodImplDecl            : node = try container.decode(MethodImplDecl.self)
    case .moduleDecl                : node = try container.decode(ModuleDecl.self)
    case .namespaceDecl             : node = try container.decode(NamespaceDecl.self)
    case .operatorDecl              : node = try container.decode(OperatorDecl.self)
    case .parameterDecl             : node = try container.decode(ParameterDecl.self)
    case .productTypeDecl           : node = try container.decode(ProductTypeDecl.self)
    case .subscriptDecl             : node = try container.decode(SubscriptDecl.self)
    case .subscriptImplDecl         : node = try container.decode(SubscriptImplDecl.self)
    case .traitDecl                 : node = try container.decode(TraitDecl.self)
    case .typeAliasDecl             : node = try container.decode(TypeAliasDecl.self)
    case .varDecl                   : node = try container.decode(VarDecl.self)

    case .assignExpr                : node = try container.decode(AssignExpr.self)
    case .asyncExpr                 : node = try container.decode(AsyncExpr.self)
    case .awaitExpr                 : node = try container.decode(AwaitExpr.self)
    case .booleanLiteralExpr        : node = try container.decode(BooleanLiteralExpr.self)
    case .bufferLiteralExpr         : node = try container.decode(BufferLiteralExpr.self)
    case .condExpr                  : node = try container.decode(CondExpr.self)
    case .errorExpr                 : node = try container.decode(ErrorExpr.self)
    case .floatLiteralExpr          : node = try container.decode(FloatLiteralExpr.self)
    case .funCallExpr               : node = try container.decode(FunCallExpr.self)
    case .inoutExpr                 : node = try container.decode(InoutExpr.self)
    case .integerLiteralExpr        : node = try container.decode(IntegerLiteralExpr.self)
    case .lambdaExpr                : node = try container.decode(LambdaExpr.self)
    case .mapLiteralExpr            : node = try container.decode(MapLiteralExpr.self)
    case .matchExpr                 : node = try container.decode(MatchExpr.self)
    case .nameExpr                  : node = try container.decode(NameExpr.self)
    case .nilExpr                   : node = try container.decode(NilExpr.self)
    case .sequenceExpr              : node = try container.decode(SequenceExpr.self)
    case .storedProjectionExpr      : node = try container.decode(StoredProjectionExpr.self)
    case .stringLiteralExpr         : node = try container.decode(StringLiteralExpr.self)
    case .subscriptCallExpr         : node = try container.decode(SubscriptCallExpr.self)
    case .tupleExpr                 : node = try container.decode(TupleExpr.self)
    case .tupleMemberExpr           : node = try container.decode(TupleMemberExpr.self)
    case .unicodeScalarLiteralExpr  : node = try container.decode(UnicodeScalarLiteralExpr.self)

    case .bindingPattern            : node = try container.decode(BindingPattern.self)
    case .exprPattern               : node = try container.decode(ExprPattern.self)
    case .namePattern               : node = try container.decode(NamePattern.self)
    case .tuplePattern              : node = try container.decode(TuplePattern.self)
    case .wildcardPattern           : node = try container.decode(WildcardPattern.self)

    case .braceStmt                 : node = try container.decode(BraceStmt.self)
    case .breakStmt                 : node = try container.decode(BreakStmt.self)
    case .continueStmt              : node = try container.decode(ContinueStmt.self)
    case .declStmt                  : node = try container.decode(DeclStmt.self)
    case .doWhileStmt               : node = try container.decode(DoWhileStmt.self)
    case .exprStmt                  : node = try container.decode(ExprStmt.self)
    case .forStmt                   : node = try container.decode(ForStmt.self)
    case .returnStmt                : node = try container.decode(ReturnStmt.self)
    case .whileStmt                 : node = try container.decode(WhileStmt.self)
    case .yieldStmt                 : node = try container.decode(YieldStmt.self)

    case .asyncTypeExpr             : node = try container.decode(AsyncTypeExpr.self)
    case .conformanceLensTypeExpr   : node = try container.decode(ConformanceLensTypeExpr.self)
    case .existentialTypeExpr       : node = try container.decode(ExistentialTypeExpr.self)
    case .indirectTypeExpr          : node = try container.decode(IndirectTypeExpr.self)
    case .lambdaTypeExpr            : node = try container.decode(LambdaTypeExpr.self)
    case .nameTypeExpr              : node = try container.decode(NameTypeExpr.self)
    case .parameterTypeExpr         : node = try container.decode(ParameterTypeExpr.self)
    case .storedProjectionTypeExpr  : node = try container.decode(StoredProjectionTypeExpr.self)
    case .tupleTypeExpr             : node = try container.decode(TupleTypeExpr.self)
    case .unionTypeExpr             : node = try container.decode(UnionTypeExpr.self)
    case .wildcardTypeExpr          : node = try container.decode(WildcardTypeExpr.self)

    case .matchCase                 : node = try container.decode(MatchCase.self)
    case .topLevelDeclSet           : node = try container.decode(TopLevelDeclSet.self)

    default:
      unreachable()
    }
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)

    let kind = type(of: node).kind
    try container.encode(kind, forKey: .kind)

    switch kind {
    case .associatedTypeDecl        : try container.encode(node as! AssociatedTypeDecl)
    case .associatedValueDecl       : try container.encode(node as! AssociatedValueDecl)
    case .bindingDecl               : try container.encode(node as! BindingDecl)
    case .builtinDecl               : try container.encode(node as! BuiltinDecl)
    case .conformanceDecl           : try container.encode(node as! ConformanceDecl)
    case .extensionDecl             : try container.encode(node as! ExtensionDecl)
    case .functionDecl              : try container.encode(node as! FunctionDecl)
    case .genericTypeParamDecl      : try container.encode(node as! GenericTypeParamDecl)
    case .genericValueParamDecl     : try container.encode(node as! GenericValueParamDecl)
    case .importDecl                : try container.encode(node as! ImportDecl)
    case .methodDecl                : try container.encode(node as! MethodDecl)
    case .methodImplDecl            : try container.encode(node as! MethodImplDecl)
    case .moduleDecl                : try container.encode(node as! ModuleDecl)
    case .namespaceDecl             : try container.encode(node as! NamespaceDecl)
    case .operatorDecl              : try container.encode(node as! OperatorDecl)
    case .parameterDecl             : try container.encode(node as! ParameterDecl)
    case .productTypeDecl           : try container.encode(node as! ProductTypeDecl)
    case .subscriptDecl             : try container.encode(node as! SubscriptDecl)
    case .subscriptImplDecl         : try container.encode(node as! SubscriptImplDecl)
    case .traitDecl                 : try container.encode(node as! TraitDecl)
    case .typeAliasDecl             : try container.encode(node as! TypeAliasDecl)
    case .varDecl                   : try container.encode(node as! VarDecl)

    case .assignExpr                : try container.encode(node as! AssignExpr)
    case .asyncExpr                 : try container.encode(node as! AsyncExpr)
    case .awaitExpr                 : try container.encode(node as! AwaitExpr)
    case .castExpr                  : try container.encode(node as! CastExpr)
    case .booleanLiteralExpr        : try container.encode(node as! BooleanLiteralExpr)
    case .bufferLiteralExpr         : try container.encode(node as! BufferLiteralExpr)
    case .condExpr                  : try container.encode(node as! CondExpr)
    case .errorExpr                 : try container.encode(node as! ErrorExpr)
    case .floatLiteralExpr          : try container.encode(node as! FloatLiteralExpr)
    case .funCallExpr               : try container.encode(node as! FunCallExpr)
    case .inoutExpr                 : try container.encode(node as! InoutExpr)
    case .integerLiteralExpr        : try container.encode(node as! IntegerLiteralExpr)
    case .lambdaExpr                : try container.encode(node as! LambdaExpr)
    case .mapLiteralExpr            : try container.encode(node as! MapLiteralExpr)
    case .matchExpr                 : try container.encode(node as! MatchExpr)
    case .nameExpr                  : try container.encode(node as! NameExpr)
    case .nilExpr                   : try container.encode(node as! NilExpr)
    case .sequenceExpr              : try container.encode(node as! SequenceExpr)
    case .storedProjectionExpr      : try container.encode(node as! StoredProjectionExpr)
    case .stringLiteralExpr         : try container.encode(node as! StringLiteralExpr)
    case .subscriptCallExpr         : try container.encode(node as! SubscriptCallExpr)
    case .tupleExpr                 : try container.encode(node as! TupleExpr)
    case .tupleMemberExpr           : try container.encode(node as! TupleMemberExpr)
    case .unicodeScalarLiteralExpr  : try container.encode(node as! UnicodeScalarLiteralExpr)

    case .bindingPattern            : try container.encode(node as! BindingPattern)
    case .exprPattern               : try container.encode(node as! ExprPattern)
    case .namePattern               : try container.encode(node as! NamePattern)
    case .tuplePattern              : try container.encode(node as! TuplePattern)
    case .wildcardPattern           : try container.encode(node as! WildcardPattern)

    case .braceStmt                 : try container.encode(node as! BraceStmt)
    case .breakStmt                 : try container.encode(node as! BreakStmt)
    case .continueStmt              : try container.encode(node as! ContinueStmt)
    case .declStmt                  : try container.encode(node as! DeclStmt)
    case .doWhileStmt               : try container.encode(node as! DoWhileStmt)
    case .exprStmt                  : try container.encode(node as! ExprStmt)
    case .forStmt                   : try container.encode(node as! ForStmt)
    case .returnStmt                : try container.encode(node as! ReturnStmt)
    case .whileStmt                 : try container.encode(node as! WhileStmt)
    case .yieldStmt                 : try container.encode(node as! YieldStmt)

    case .asyncTypeExpr             : try container.encode(node as! AsyncTypeExpr)
    case .conformanceLensTypeExpr   : try container.encode(node as! ConformanceLensTypeExpr)
    case .existentialTypeExpr       : try container.encode(node as! ExistentialTypeExpr)
    case .indirectTypeExpr          : try container.encode(node as! IndirectTypeExpr)
    case .lambdaTypeExpr            : try container.encode(node as! LambdaTypeExpr)
    case .nameTypeExpr              : try container.encode(node as! NameTypeExpr)
    case .parameterTypeExpr         : try container.encode(node as! ParameterTypeExpr)
    case .storedProjectionTypeExpr  : try container.encode(node as! StoredProjectionTypeExpr)
    case .tupleTypeExpr             : try container.encode(node as! TupleTypeExpr)
    case .unionTypeExpr             : try container.encode(node as! UnionTypeExpr)
    case .wildcardTypeExpr          : try container.encode(node as! WildcardTypeExpr)

    case .matchCase                 : try container.encode(node as! MatchCase)
    case .topLevelDeclSet           : try container.encode(node as! TopLevelDeclSet)

    default:
      unreachable()
    }
  }

}

fileprivate extension KeyedDecodingContainer where K == AnyNode.CodingKeys {

  func decode<T: Node>(_ type: T.Type) throws -> T {
    try decode(type, forKey: .data)
  }

}


fileprivate extension KeyedEncodingContainer where K == AnyNode.CodingKeys {

  mutating func encode<T: Node>(_ node: T) throws {
    try encode(node, forKey: .data)
  }

}
