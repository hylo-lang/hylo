import FrontEnd

/// The lowered (static) type of an entity.
///
/// Note: when qualified, this must be spelled IR.`Type`
/// (https://github.com/apple/swift/issues/67378)
public struct Type: Hashable, Sendable {

  /// A high-level representation of the type.
  public let ast: AnyType

  /// Indicates whether this is a place type.
  public let isPlace: Bool

  /// Creates a lowered type.
  ///
  /// - Requires: `ast` must be canonical.
  public init<T: TypeProtocol>(ast: T, isPlace: Bool) {
    precondition(ast.isCanonical, "source type is not canonical")
    self.ast = ^ast
    self.isPlace = isPlace
  }

  /// Indicates whether this is an object type.
  public var isObject: Bool { !isPlace }

  /// Creates an object type.
  public static func object<T: TypeProtocol>(_ type: T) -> Self {
    IR.`Type`(ast: type, isPlace: false)
  }

  /// Creates a place type.
  public static func place<T: TypeProtocol>(_ type: T) -> Self {
    IR.`Type`(ast: type, isPlace: true)
  }

}

extension IR.`Type`: CustomStringConvertible {

  public var description: String {
    if isPlace {
      return "&" + String(describing: ast)
    } else {
      return String(describing: ast)
    }
  }

}
