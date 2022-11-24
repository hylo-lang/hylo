import Utils

/// A generic type bound to arguments.
public struct BoundGenericType: TypeProtocol {

  /// An argument of a bound generic type.
  public enum Argument: Hashable {

    /// A type argument.
    case type(AnyType)

    /// A value argument.
    case value(AnyExprID)

  }

  /// The underlying generic type.
  public let base: AnyType

  /// The type and value arguments of the base type.
  public let arguments: [Argument]

  public let flags: TypeFlags

  /// Creates a bound generic type binding `base` to the given `arguments`.
  public init<S: Sequence>(_ base: AnyType, arguments: S) where S.Element == Argument {
    self.base = base

    var args: [Argument] = []
    var flags: TypeFlags = base.flags
    for a in arguments {
      args.append(a)
      switch a {
      case .type(let t):
        flags.merge(t.flags)
      case .value:
        fatalError("not implemented")
      }
    }

    self.arguments = args
    self.flags = flags
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    BoundGenericType(
      base.transform(transformer),
      arguments: arguments.map({ (a: Argument) -> Argument in
        switch a {
        case .type(let type):
          // Argument is a type.
          return .type(type.transform(transformer))

        case .value:
          // Argument is a value.
          fatalError("not implemented")
        }
      }))
  }

}

extension BoundGenericType: CustomStringConvertible {

  public var description: String { "\(base)<\(arguments.descriptions())>" }

}

extension BoundGenericType.Argument: CustomStringConvertible {

  public var description: String {
    switch self {
    case .type(let a):
      return String(describing: a)
    case .value(let a):
      return String(describing: a)
    }
  }

}
