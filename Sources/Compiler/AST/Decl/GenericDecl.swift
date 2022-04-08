/// A generic declaration.
public protocol GenericDecl: Decl {

  /// The generic clause of the declaration, if any.
  var genericClause: SourceRepresentable<GenericClause>? { get }

}
