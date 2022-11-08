/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<Type>

  /// The type of each expression.
  public let exprTypes: ExprProperty<Type>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  public init(
    annotating program: ScopedProgram,
    declTypes: DeclProperty<Type>,
    exprTypes: ExprProperty <Type>,
    referredDecls: [NodeID<NameExpr>: DeclRef]
  ) {
    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.varToBinding = program.varToBinding
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.referredDecls = referredDecls
  }

}
