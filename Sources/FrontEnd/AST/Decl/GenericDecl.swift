/// A generic declaration.
public protocol GenericDecl: Decl, GenericScope {

  /// The generic clause of the declaration, if any.
  var genericClause: SourceRepresentable<GenericClause>? { get }

}
