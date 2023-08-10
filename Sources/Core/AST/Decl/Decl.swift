/// A declaration.
public protocol Decl: Node {

  /// `true` iff `Self` is the declaration of a callable entity.
  static var isCallable: Bool { get }

  /// `true` iff `self` is a definition of the entity that it declares.
  var isDefinition: Bool { get }

}

extension Decl {

  public static var isCallable: Bool { false }

  public var isDefinition: Bool { true }

}
