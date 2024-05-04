/// A unicode scalar literal expression.
public struct UnicodeScalarLiteralExpr: Expr {

  public let site: SourceRange

  /// The value of the literal.
  public let value: Unicode.Scalar

  public init(value: Unicode.Scalar, site: SourceRange) {
    self.site = site
    self.value = value
  }

}

extension UnicodeScalarLiteralExpr: Codable {

  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()

    // Decode the source site.
    site = try container.decode(SourceRange.self)

    // Decode the unicode scalar value.
    if let scalar = Unicode.Scalar(try container.decode(UInt32.self)) {
      value = scalar
    } else {
      throw DecodingError.dataCorrupted(
        DecodingError.Context(
          codingPath: decoder.codingPath,
          debugDescription: "Invalid Unicode scalar value"))
    }
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(site)
    try container.encode(value.value)
  }

}
