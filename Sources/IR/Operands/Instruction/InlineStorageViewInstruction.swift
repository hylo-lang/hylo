import Core

/// Given the address of a record value in memory, derives the address of a stored property within.
public struct InlineStorageViewInstruction: Instruction {

  /// The address of a record.
  public private(set) var base: Operand

  /// A sequence of indices identifying a part of the value at `base`.
  public let elementPath: PartPath

  /// The type of the derived address.
  public let elementType: LoweredType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    base: Operand,
    elementPath path: PartPath,
    elementType type: LoweredType,
    site: SourceRange
  ) {
    self.base = base
    self.elementPath = path
    self.elementType = type
    self.site = site
  }

  public var types: [LoweredType] { [elementType] }

  public var operands: [Operand] { [base] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    base = new
  }

}

extension InlineStorageViewInstruction: CustomStringConvertible {

  public var description: String {
    let s = "inline_storage_view \(base)"
    return elementPath.isEmpty ? s : "\(s), \(list: elementPath)"
  }

}

extension Module {

  /// Creates an `inline_storage_view` anchored at `site` that computes the address of the property
  /// at `path` rooted at `base`.
  ///
  /// - Note: `base` is returned unchanged if `elementPath` is empty.
  /// - Parameters:
  ///   - base: The base address used for the computation.
  ///   - elementPath: An array of of indices identifying a sub-location in `base`.
  func makeInlineStorageView(
    _ base: Operand, at elementPath: PartPath, at anchor: SourceRange
  ) -> InlineStorageViewInstruction {
    precondition(type(of: base).isAddress)
    let l = AbstractTypeLayout(of: type(of: base).ast, definedIn: program)
    return .init(
      base: base,
      elementPath: elementPath,
      elementType: .address(l[elementPath].type),
      site: anchor)
  }

}
