/// An AST node.
public protocol Node {

  /// A unique identifier denoting the type of this node.
  static var typeID: ObjectIdentifier { get }

}

extension Node {

  public static var typeID: ObjectIdentifier { ObjectIdentifier(Self.self) }

}
