import Core

/// A floating-point number Val IR constant.
public struct FloatingPointConstant: ConstantProtocol, Hashable {

  /// The serialized value of this constant.
  public let value: String

  /// The Val IR type of this instance.
  public let type: LoweredType

  /// Creates a new floating-point Val IR constant with value `v` and the given `type`.
  private init(_ v: String, type: BuiltinType) {
    self.value = v
    self.type = .object(type)
  }

  /// Creates a nre Val IR `double` constant with value `v`.
  public static func double(_ v: String) -> Self {
    .init(v, type: .double)
  }

  /// Creates a nre Val IR `float` constant with value `v`.
  public static func float(_ v: String) -> Self {
    .init(v, type: .float)
  }

}

extension FloatingPointConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(\(value))"
  }

}
