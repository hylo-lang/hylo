/// A built-in type.
public enum BuiltinType: TypeProtocol, Hashable {

  /// A built-in integer literal type.
  case integerLiteral

  /// A built-in integer.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(bitWidth: Int)

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .integerLiteral:
      return "IntegerLiteral"
    case .i(let bitWidth):
      return "i\(bitWidth)"
    }
  }

}
