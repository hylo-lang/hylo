/// A generic type declaration that may be annotated with constraints.
public protocol ConstrainedGenericTypeDecl: Decl, Sendable {

  /// The conformances listed in the declaration.
  var conformances: [NameExpr.ID] { get }

}
