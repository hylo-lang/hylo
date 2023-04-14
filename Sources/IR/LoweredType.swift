import Core

/// The lowered (static) type of an entity.
public struct LoweredType: Hashable {

  /// A high-level representation of the type.
  public let ast: AnyType

  /// Indicates whether this is an address type.
  public let isAddress: Bool

  /// Creates a lowered type.
  ///
  /// - Requires: `ast` must be canonical.
  private init<T: TypeProtocol>(ast: T, isAddress: Bool) {
    precondition(ast[.isCanonical], "source type is not canonical")
    self.ast = ^ast
    self.isAddress = isAddress
  }

  /// Creates a lowered type from a high-level type.
  public init(lowering type: AnyType) {
    switch type.base {
    case let ty as RemoteType:
      precondition(ty.access != .yielded, "cannot lower yielded type")
      self.ast = ty.bareType
      self.isAddress = true

    case let ty as ParameterType:
      self.ast = ty.bareType
      switch ty.access {
      case .let, .inout, .set:
        self.isAddress = true
      case .sink:
        self.isAddress = false
      case .yielded:
        preconditionFailure("cannot lower yielded type")
      }

    default:
      self.ast = type
      self.isAddress = false
    }
  }

  /// Indicates whether this is an object type.
  public var isObject: Bool { !isAddress }

  /// Creates an object type.
  public static func object<T: TypeProtocol>(_ type: T) -> Self {
    LoweredType(ast: type, isAddress: false)
  }

  /// Creates and address type.
  public static func address<T: TypeProtocol>(_ type: T) -> Self {
    LoweredType(ast: type, isAddress: true)
  }

}

extension LoweredType: CustomStringConvertible {

  public var description: String {
    if isAddress {
      return "&" + String(describing: ast)
    } else {
      return String(describing: ast)
    }
  }

}
