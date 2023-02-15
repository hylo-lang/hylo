/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<AnyType>

  /// The type of each expression.
  public let exprTypes: ExprProperty<AnyType>

  /// A map from function and subscript declarations to their implicit captures.
  public let implicitCaptures: DeclProperty<[ImplicitCapture]>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// A map from sequence expressions to their evaluation order.
  public let foldedSequenceExprs: [NodeID<SequenceExpr>: FoldedSequenceExpr]

  /// The type relations of the program.
  public let relations: TypeRelations

  /// The Val standard library module.
  public var corelib: ModuleDecl.Typed? {
    ast.corelib.map({ self[$0] })
  }

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  ///
  /// - Requires: All modules in `program` have been typed checked.
  public init(
    annotating program: ScopedProgram,
    declTypes: DeclProperty<AnyType>,
    exprTypes: ExprProperty<AnyType>,
    implicitCaptures: DeclProperty<[ImplicitCapture]>,
    referredDecls: [NodeID<NameExpr>: DeclRef],
    foldedSequenceExprs: [NodeID<SequenceExpr>: FoldedSequenceExpr],
    relations: TypeRelations
  ) {
    precondition(program.ast.modules.allSatisfy({ declTypes[$0]?.base is ModuleType }))

    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.varToBinding = program.varToBinding
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.implicitCaptures = implicitCaptures
    self.referredDecls = referredDecls
    self.foldedSequenceExprs = foldedSequenceExprs
    self.relations = relations
  }

}
