import Core

/// The lowered (static) type of an entity.
public struct LoweredType: Hashable {

  /// A high-level representation of the type.
  public let astType: AnyType

  /// Indicates whether this is an address type.
  public let isAddress: Bool

  /// Creates a lowered type.
  ///
  /// - Requires: `astType` must be canonical.
  private init<T: TypeProtocol>(astType: T, isAddress: Bool) {
    precondition(astType[.isCanonical], "source type is not canonical")
    self.astType = ^astType
    self.isAddress = isAddress
  }

  /// Creates a lowered type from a high-level type.
  public init(lowering type: AnyType) {
    switch type.base {
    case let ty as RemoteType:
      precondition(ty.capability != .yielded, "cannot lower yielded type")
      self.astType = ty.base
      self.isAddress = true

    case let ty as ParameterType:
      self.astType = ty.bareType
      switch ty.convention {
      case .let, .inout, .set:
        self.isAddress = true
      case .sink:
        self.isAddress = false
      case .yielded:
        preconditionFailure("cannot lower yielded type")
      }

    default:
      self.astType = type
      self.isAddress = false
    }
  }

  /// Creates an object type.
  public static func object<T: TypeProtocol>(_ type: T) -> Self {
    LoweredType(astType: type, isAddress: false)
  }

  /// Creates and address type.
  public static func address<T: TypeProtocol>(_ type: T) -> Self {
    LoweredType(astType: type, isAddress: true)
  }

}

extension LoweredType: CustomStringConvertible {

  public var description: String {
    if isAddress {
      return "&" + String(describing: astType)
    } else {
      return String(describing: astType)
    }
  }

}
