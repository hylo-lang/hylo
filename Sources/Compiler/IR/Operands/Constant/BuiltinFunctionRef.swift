/// A Val IR reference to a built-in function.
public struct BuiltinFunctionRef: ConstantProtocol, Hashable {

  /// The name of the function.
  public let name: String

  /// The type of the function.
  public let type: LoweredType

  /// Returns a reference to the built-in function with the given name.
  public static subscript(_ name: String) -> BuiltinFunctionRef? {
    if let type = BuiltinSymbols[name] {
      return BuiltinFunctionRef(name: name, type: .address(type))
    } else {
      return nil
    }
  }

}

extension BuiltinFunctionRef: CustomStringConvertible {

  public var description: String {
    "@builtin.\(name)"
  }

}
