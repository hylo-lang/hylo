/// A built-in type.
public enum BuiltinType: TypeProtocol, Hashable {

  /// A built-in integer.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    }
  }

}
