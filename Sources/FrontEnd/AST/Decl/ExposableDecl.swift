/// A declaration that may be exposed to other scopes.
public protocol ExposableDecl: Decl, Sendable {

  /// The access modifier of the declaration, if any.
  var accessModifier: SourceRepresentable<AccessModifier> { get }

}
