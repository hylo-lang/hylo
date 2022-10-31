/// A unicode scalar literal expression.
public struct UnicodeScalarLiteralExpr: Expr {

  public static let kind = NodeKind.unicodeScalarLiteralExpr

  /// The value of the literal.
  public let value: Unicode.Scalar

  public init(value: Unicode.Scalar) {
    self.value = value
  }

}

extension UnicodeScalarLiteralExpr: Codable {

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    if let scalar = Unicode.Scalar(try container.decode(UInt32.self)) {
      value = scalar
    } else {
      throw DecodingError.dataCorrupted(DecodingError.Context(
        codingPath: decoder.codingPath, debugDescription: "Invalid Unicode scalar value"))
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(value.value)
  }

}
