/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram {

  /// The AST of the program.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<Type>

  /// The type of each expression.
  public let exprTypes: ExprProperty<Type>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// Creates a typed program from an AST and property maps describing its type annotations.
  public init(
    ast: AST,
    scopeHierarchy: ScopeHierarchy,
    declTypes: DeclProperty<Type>,
    exprTypes: ExprProperty <Type>,
    referredDecls: [NodeID<NameExpr>: DeclRef]
  ) {
    self.ast = ast
    self.scopeHierarchy = scopeHierarchy
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.referredDecls = referredDecls
  }

  /// Returns a locator for the specified declaration.
  public func locator<T: DeclID>(identifying decl: T) -> DeclLocator {
    DeclLocator(
      identifying: decl,
      in: ast,
      withScopeHierarchy: scopeHierarchy,
      withDeclTypes: declTypes)
  }

}
