import FrontEnd

/// The lowered (static) type of an entity.
///
/// Note: when qualified, this must be spelled IR.`Type`
/// (https://github.com/apple/swift/issues/67378)
public struct Type: Hashable {

  /// A high-level representation of the type.
  public let ast: AnyType

  /// Indicates whether this is an address type.
  public let isAddress: Bool

  /// Creates a lowered type.
  ///
  /// - Requires: `ast` must be canonical.
  public init<T: TypeProtocol>(ast: T, isAddress: Bool) {
    precondition(ast.isCanonical, "source type is not canonical")
    self.ast = ^ast
    self.isAddress = isAddress
  }

  /// Indicates whether this is an object type.
  public var isObject: Bool { !isAddress }

  /// Creates an object type.
  public static func object<T: TypeProtocol>(_ type: T) -> Self {
    IR.`Type`(ast: type, isAddress: false)
  }

  /// Creates and address type.
  public static func address<T: TypeProtocol>(_ type: T) -> Self {
    IR.`Type`(ast: type, isAddress: true)
  }

}

extension IR.`Type`: CustomStringConvertible {

  public var description: String {
    if isAddress {
      return "&" + String(describing: ast)
    } else {
      return String(describing: ast)
    }
  }

}
