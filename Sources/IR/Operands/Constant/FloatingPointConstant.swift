import Core

/// A floating-point number Val IR constant.
public struct FloatingPointConstant: Constant, Hashable {

  /// The serialized value of this constant.
  public let value: String

  /// The Val IR type of this instance.
  public let type: LoweredType

  /// Creates a new floating-point Val IR constant with value `v` and the given `type`.
  private init(_ v: String, type: BuiltinType) {
    self.value = v
    self.type = .object(type)
  }

  /// Creates a new Val IR `float64` constant with value `v`.
  public static func float64(_ v: String) -> Self {
    .init(v, type: .float64)
  }

  /// Creates a new Val IR `float32` constant with value `v`.
  public static func float32(_ v: String) -> Self {
    .init(v, type: .float32)
  }

}

extension FloatingPointConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(\(value))"
  }

}
