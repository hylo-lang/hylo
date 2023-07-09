import Core

/// Computes a `source` address value advanced by `offset` strides of the `source`'s content type
/// (`source + offset` in C terms).
public struct AdvancedByStridesInstruction: Instruction {

  /// The pointer value that is advanced to produce the result.
  public private(set) var source: Operand

  /// An integer number of strides to advance the source value.
  public private(set) var offset: Operand

  /// The site of the val source from which `self` was generated.
  public let site: SourceRange

  /// The content type of the `source` (and resulting) address value.
  private let contentType: LoweredType

  /// Creates an instance with the given properties.
  fileprivate init(
    contentType: LoweredType,
    source: Operand,
    offset: Operand,
    site: SourceRange
  ) {
    self.contentType = contentType
    self.source = source
    self.offset = offset
    self.site = site
  }

  public var types: [LoweredType] { [contentType] }

  public var operands: [Operand] { [source, offset] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i >= 0 && i < 2)
    if i == 0 { source = new } else { offset = new }
  }

}

extension AdvancedByStridesInstruction: CustomStringConvertible {

  public var description: String {
    "\(source) advanced by \(offset) strides"
  }

}

extension Module {

  /// Creates an `advanced by strides` instruction anchored at `site` computing the `source` address
  /// value advanced by `offset` strides of the `source`'s content type.
  func makeAdvancedByStrides(source: Operand, offset: Operand, at site: SourceRange)
    -> AdvancedByStridesInstruction
  {
    precondition(type(of: source).isAddress)
    precondition(type(of: offset).isAddress)
    return .init(
      contentType: type(of: source),
      source: source,
      offset: offset,
      site: site)
  }

}
