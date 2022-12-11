/// A unicode scalar literal expression.
public struct UnicodeScalarLiteralExpr: Expr {

  public let origin: SourceRange?

  /// The value of the literal.
  public let value: Unicode.Scalar

  public init(value: Unicode.Scalar, origin: SourceRange?) {
    self.origin = origin
    self.value = value
  }

}

extension UnicodeScalarLiteralExpr: Codable {

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()

    // Decode the source origin.
    origin = try container.decode(SourceRange?.self)

    // Decode the unicode scalar value.
    if let scalar = Unicode.Scalar(try container.decode(UInt32.self)) {
      value = scalar
    } else {
      throw DecodingError.dataCorrupted(
        DecodingError.Context(
          codingPath: decoder.codingPath, debugDescription: "Invalid Unicode scalar value"))
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(origin)
    try container.encode(value.value)
  }

}
