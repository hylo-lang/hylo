/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram {

  /// The AST of the program.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclMap<Type>

  /// The type of each expression.
  public let exprTypes: ExprMap<Type>

  /// A table mapping name expressions to referred declarations.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// Creates a typed program from an AST and property maps describing its type annotations.
  public init(
    ast: AST,
    scopeHierarchy: ScopeHierarchy,
    declTypes: DeclMap<Type>,
    exprTypes: ExprMap <Type>,
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
