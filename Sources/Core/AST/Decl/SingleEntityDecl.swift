/// A declaration that introduces a single entity.
public protocol SingleEntityDecl: Decl {

  /// The stem identifier of the entity introduced by the declaration.
  var baseName: String { get }

  /// The operator used to mangle instances of `Self`.
  static var manglingOperator: ManglingOperator { get }

}
