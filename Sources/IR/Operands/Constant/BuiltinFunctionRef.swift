import Core

/// A Val IR reference to a built-in function.
public struct BuiltinFunctionRef: ConstantProtocol, Hashable {

  /// The function being referred to.
  public let function: BuiltinFunction

  /// Creates an instance referring to `f`.
  public init(referringTo f: BuiltinFunction) {
    self.function = f
  }

  /// The type of the function.
  public var type: LoweredType { .address(function.type) }

}

extension BuiltinFunctionRef: CustomStringConvertible {

  public var description: String {
    "@builtin.\(function)"
  }

}

extension BuiltinFunction {

  /// Returns a Val IR reference to this instance.
  var reference: BuiltinFunctionRef {
    BuiltinFunctionRef(referringTo: self)
  }

}
