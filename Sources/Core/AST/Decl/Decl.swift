/// A declaration.
public protocol Decl: Node {

  /// `true` is `Self` is the declaration of a callable entity.
  static var isCallable: Bool { get }

}

extension Decl {

  public static var isCallable: Bool { false }

}
