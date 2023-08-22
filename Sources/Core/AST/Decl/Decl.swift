/// A declaration.
public protocol Decl: Node {

  /// The description of the construct represented by `Self` in diagnostics.
  static var constructDescription: String { get }

  /// `true` iff `Self` is the declaration of a callable entity.
  static var isCallable: Bool { get }

  /// `true` iff `self` is a definition of the entity that it declares.
  var isDefinition: Bool { get }

}

extension Decl {

  public static var isCallable: Bool { false }

  public var isDefinition: Bool { true }

}
