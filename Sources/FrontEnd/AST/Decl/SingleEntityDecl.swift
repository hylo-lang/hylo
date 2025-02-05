/// A declaration that introduces a single entity.
public protocol SingleEntityDecl: Decl, Sendable {

  /// The stem identifier of the entity introduced by the declaration.
  var baseName: String { get }

}
