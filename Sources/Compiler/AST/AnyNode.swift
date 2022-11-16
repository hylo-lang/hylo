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
    case .initializerDecl           : node = try container.decode(InitializerDecl.self)
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
    try container.encode(node)
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
