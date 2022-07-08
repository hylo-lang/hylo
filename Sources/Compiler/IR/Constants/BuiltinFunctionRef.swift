/// A Val IR reference to a built-in function.
public struct BuiltinFunctionRef: ConstantProtocol, Hashable {

  /// The name of the function.
  public let name: String

  /// The type of the function.
  public let type: LoweredType

}

extension BuiltinFunctionRef: CustomStringConvertible {

  public var description: String {
    "@builtin.\(name)"
  }

}
