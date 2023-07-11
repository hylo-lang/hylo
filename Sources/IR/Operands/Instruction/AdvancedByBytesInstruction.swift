import Core

/// Computes a `source` address value advanced by `offset` bytes.
public struct AdvancedByBytesInstruction: Instruction {

  /// The value of a pointer to be advanced.
  public private(set) var base: Operand

  /// The value of an integer number of bytes by which to advance the source value.
  public private(set) var byteOffset: Operand

  /// The site of the val source from which `self` was generated.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    source: Operand,
    offset: Operand,
    site: SourceRange
  ) {
    self.base = source
    self.byteOffset = offset
    self.site = site
  }

  public var types: [LoweredType] { [.object(BuiltinType.ptr)] }

  public var operands: [Operand] { [base, byteOffset] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i >= 0 && i < 2)
    if i == 0 { base = new } else { byteOffset = new }
  }

}

extension AdvancedByBytesInstruction: CustomStringConvertible {

  public var description: String {
    "\(base) advanced by \(byteOffset) bytes"
  }

}

extension Module {

  /// Creates an `advanced by bytes` instruction anchored at `site` computing the `source` address
  /// value advanced by `offset` bytes.
  func makeAdvancedByBytes(source: Operand, offset: Operand, at site: SourceRange)
    -> AdvancedByBytesInstruction
  {
    precondition(type(of: source).isAddress)
    precondition(type(of: offset).isAddress)
    return .init(
      source: source,
      offset: offset,
      site: site)
  }

}
