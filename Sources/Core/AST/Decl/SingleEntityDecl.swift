/// A declaration that introduces a single entity.
public protocol SingleEntityDecl: Decl {

  /// The name of the entity introduced by the declaration.
  var name: String { get }

}
