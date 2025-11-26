import FrontEnd
import Utils

/// A floating-point number Hylo IR constant.
public struct FloatingPointConstant: Constant, Hashable {

  /// The serialized value of this constant.
  public let value: String

  /// The Hylo IR type of this instance.
  public let type: IR.`Type`

  /// Creates a new floating-point Hylo IR constant with value `v` and the given `type`.
  private init(_ v: String, type: BuiltinType) {
    self.value = v
    self.type = .object(type)
  }

  /// Creates a new Hylo IR `float64` constant with value `v`.
  public static func float64(_ v: String) -> Self {
    .init(v, type: .float64)
  }

  /// Creates a new Hylo IR `float32` constant with value `v`.
  public static func float32(_ v: String) -> Self {
    .init(v, type: .float32)
  }

}

extension FloatingPointConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(\(value))"
  }

}

extension FloatingPointConstant: ColoredDescribable {

  public var coloredDescription: String {
    "\(type.coloredDescription)(\(styledConstant(value)))"
  }

}
