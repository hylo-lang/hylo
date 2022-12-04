/// Helper protocol that allows its implementers to have easy access to type aliases for typed nodes.
public protocol TypedChecked {}

extension TypedChecked {

  /// Alias for a typed checked node.
  public typealias Node<T: AST.Node> = TypedProgram.Node<T>
  
  /// Any typed scoped node.
  typealias AnyTypedScope = TypedProgram.AnyScope
  /// Any typed decl node.
  typealias AnyTypedDecl = TypedProgram.AnyDecl
  /// Any typed expression node.
  typealias AnyTypedExpr = TypedProgram.AnyExpr
  /// Any typed node.
  typealias AnyTypedNode = TypedProgram.AnyNode

  // The type aliases below are kepy in sync with the node kinds mentioned in `NodeKind.allValue`.
  
  // MARK: Declarations
  public typealias TypedAssociatedTypeDecl = TypedProgram.Node<AssociatedTypeDecl>
  public typealias TypedAssociatedValueDecl = TypedProgram.Node<AssociatedValueDecl>
  public typealias TypedBindingDecl = TypedProgram.Node<BindingDecl>
  public typealias TypedBuiltinDecl = TypedProgram.Node<BuiltinDecl>
  public typealias TypedConformanceDecl = TypedProgram.Node<ConformanceDecl>
  public typealias TypedExtensionDecl = TypedProgram.Node<ExtensionDecl>
  public typealias TypedFunctionDecl = TypedProgram.Node<FunctionDecl>
  public typealias TypedGenericParameterDecl = TypedProgram.Node<GenericParameterDecl>
  public typealias TypedImportDecl = TypedProgram.Node<ImportDecl>
  public typealias TypedInitializerDecl = TypedProgram.Node<InitializerDecl>
  public typealias TypedMethodDecl = TypedProgram.Node<MethodDecl>
  public typealias TypedMethodImplDecl = TypedProgram.Node<MethodImplDecl>
  public typealias TypedModuleDecl = TypedProgram.Node<ModuleDecl>
  public typealias TypedNamespaceDecl = TypedProgram.Node<NamespaceDecl>
  public typealias TypedOperatorDecl = TypedProgram.Node<OperatorDecl>
  public typealias TypedParameterDecl = TypedProgram.Node<ParameterDecl>
  public typealias TypedProductTypeDecl = TypedProgram.Node<ProductTypeDecl>
  public typealias TypedSubscriptDecl = TypedProgram.Node<SubscriptDecl>
  public typealias TypedSubscriptImplDecl = TypedProgram.Node<SubscriptImplDecl>
  public typealias TypedTraitDecl = TypedProgram.Node<TraitDecl>
  public typealias TypedTypeAliasDecl = TypedProgram.Node<TypeAliasDecl>
  public typealias TypedVarDecl = TypedProgram.Node<VarDecl>

  // MARK: Value expressions
  public typealias TypedAssignExpr = TypedProgram.Node<AssignExpr>
  public typealias TypedBooleanLiteralExpr = TypedProgram.Node<BooleanLiteralExpr>
  public typealias TypedBufferLiteralExpr = TypedProgram.Node<BufferLiteralExpr>
  public typealias TypedCastExpr = TypedProgram.Node<CastExpr>
  public typealias TypedCondExpr = TypedProgram.Node<CondExpr>
  public typealias TypedErrorExpr = TypedProgram.Node<ErrorExpr>
  public typealias TypedFloatLiteralExpr = TypedProgram.Node<FloatLiteralExpr>
  public typealias TypedFunCallExpr = TypedProgram.Node<FunCallExpr>
  public typealias TypedInoutExpr = TypedProgram.Node<InoutExpr>
  public typealias TypedIntegerLiteralExpr = TypedProgram.Node<IntegerLiteralExpr>
  public typealias TypedLambdaExpr = TypedProgram.Node<LambdaExpr>
  public typealias TypedMapLiteralExpr = TypedProgram.Node<MapLiteralExpr>
  public typealias TypedMatchExpr = TypedProgram.Node<MatchExpr>
  public typealias TypedNameExpr = TypedProgram.Node<NameExpr>
  public typealias TypedNilLiteralExpr = TypedProgram.Node<NilLiteralExpr>
  public typealias TypedSequenceExpr = TypedProgram.Node<SequenceExpr>
  public typealias TypedSpawnExpr = TypedProgram.Node<SpawnExpr>
  public typealias TypedStringLiteralExpr = TypedProgram.Node<StringLiteralExpr>
  public typealias TypedSubscriptCallExpr = TypedProgram.Node<SubscriptCallExpr>
  public typealias TypedTupleExpr = TypedProgram.Node<TupleExpr>
  public typealias TypedTupleMemberExpr = TypedProgram.Node<TupleMemberExpr>
  public typealias TypedUnicodeScalarLiteralExpr = TypedProgram.Node<UnicodeScalarLiteralExpr>

  // MARK: Patterns
  public typealias TypedBindingPattern = TypedProgram.Node<BindingPattern>
  public typealias TypedExprPattern = TypedProgram.Node<ExprPattern>
  public typealias TypedNamePattern = TypedProgram.Node<NamePattern>
  public typealias TypedTuplePattern = TypedProgram.Node<TuplePattern>
  public typealias TypedWildcardPattern = TypedProgram.Node<WildcardPattern>

  // MARK: Statements
  public typealias TypedBraceStmt = TypedProgram.Node<BraceStmt>
  public typealias TypedBreakStmt = TypedProgram.Node<BreakStmt>
  public typealias TypedCondBindingStmt = TypedProgram.Node<CondBindingStmt>
  public typealias TypedContinueStmt = TypedProgram.Node<ContinueStmt>
  public typealias TypedDeclStmt = TypedProgram.Node<DeclStmt>
  public typealias TypedDiscardStmt = TypedProgram.Node<DiscardStmt>
  public typealias TypedDoWhileStmt = TypedProgram.Node<DoWhileStmt>
  public typealias TypedExprStmt = TypedProgram.Node<ExprStmt>
  public typealias TypedForStmt = TypedProgram.Node<ForStmt>
  public typealias TypedReturnStmt = TypedProgram.Node<ReturnStmt>
  public typealias TypedWhileStmt = TypedProgram.Node<WhileStmt>
  public typealias TypedYieldStmt = TypedProgram.Node<YieldStmt>

  // MARK: Type expressions
  public typealias TypedConformanceLensTypeExpr = TypedProgram.Node<ConformanceLensTypeExpr>
  public typealias TypedExistentialTypeExpr = TypedProgram.Node<ExistentialTypeExpr>
  public typealias TypedLambdaTypeExpr = TypedProgram.Node<LambdaTypeExpr>
  public typealias TypedParameterTypeExpr = TypedProgram.Node<ParameterTypeExpr>
  public typealias TypedRemoteTypeExpr = TypedProgram.Node<RemoteTypeExpr>
  public typealias TypedTupleTypeExpr = TypedProgram.Node<TupleTypeExpr>
  public typealias TypedUnionTypeExpr = TypedProgram.Node<UnionTypeExpr>

  // MARK: Others
  public typealias TypedMatchCase = TypedProgram.Node<MatchCase>
  public typealias TypedTopLevelDeclSet = TypedProgram.Node<TopLevelDeclSet>
}
