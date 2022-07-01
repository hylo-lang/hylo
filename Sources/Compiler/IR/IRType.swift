/// The lowered (static) type of an entity.
public enum IRType: Hashable {

  /// The type of an owned object.
  case owned(Type)

  /// The type of an object's address.
  case address(Type)

  /// Creates a lowered type from a Val type.
  init(_ type: Type) {
    switch type {
    case .projection(let ty):
      precondition(ty.capability != .yielded, "cannot lower yielded type")
      self = .address(ty.base)

    case .parameter(let ty):
      switch ty.convention {
      case .let, .inout, .set:
        self = .address(ty.bareType)
      case .sink:
        self = .owned(ty.bareType)
      case .yielded:
        preconditionFailure("cannot lower yielded type")
      }

    default:
      self = .owned(type)
    }
  }

  /// The represented Val type.
  public var valType: Type {
    switch self {
    case .owned(let ty):
      return ty
    case .address(let ty):
      return ty
    }
  }

}
