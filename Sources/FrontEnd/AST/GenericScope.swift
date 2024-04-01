/// An AST node that outlines a generic lexical scope.
public protocol GenericScope: Decl, LexicalScope {

  /// The generic parameters introduced in this scope.
  var genericParameters: [GenericParameterDecl.ID] { get }

}

extension GenericScope where Self: GenericDecl {

  public var genericParameters: [GenericParameterDecl.ID] {
    genericClause?.value.parameters ?? []
  }

}

extension GenericScope where Self: TypeExtendingDecl {

  public var genericParameters: [GenericParameterDecl.ID] {
    []
  }

}
