/// A generic type bound to arguments.
public struct BoundGenericType: TypeProtocol, Hashable {

  /// An argument of a bound generic type.
  public enum Argument: Hashable {

    case type(Type)

    case value(AnyExprID)

  }

  /// The underlying generic type.
  public let base: Type

  /// The type and value arguments of the base type.
  public var arguments: [Argument]

  public let flags: TypeFlags

  /// Creates a bound generic type binding `base` to the given `arguments`.
  public init<S: Sequence>(_ base: Type, arguments: S) where S.Element == Argument {
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

}

extension BoundGenericType: CustomStringConvertible {

  public var description: String {
    let arguments = arguments.map({ (a) -> String in
      switch a {
      case .type(let a):
        return "\(a)"
      case .value:
        fatalError("not implemented")
      }
    }).joined(separator: ", ")
    return "\(base)<\(arguments)>"
  }

}
