import Core

/// Given the address of a record value in memory, derives the address of a stored property within.
public struct ElementAddrInstruction: Instruction {

  /// The address of a record.
  public let base: Operand

  /// A sequence of indices identifying a part of the value at `base`.
  public let elementPath: [Int]

  /// The type of the derived address.
  public let elementType: LoweredType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    base: Operand,
    elementPath path: [Int],
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

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Instruction result has an address type.
    if !elementType.isAddress { return false }

    // Operand has an address type.
    if !module.type(of: base).isAddress { return false }

    return true
  }

}

extension Module {

  /// Creates an `element_addr` anchored at `anchor` that computes the address of the property at
  /// `path` rooted at `base`.
  ///
  /// - Note: `base` is returned unchanced if `elementPath` is empty.
  /// - Parameters:
  ///   - base: The base address used for the computation.
  ///   - elementPath: An array of of indices identifying a sub-location in `base`.
  func makeElementAddr(
    _ base: Operand,
    at elementPath: [Int],
    anchoredAt anchor: SourceRange
  ) -> ElementAddrInstruction {
    precondition(type(of: base).isAddress)

    let l = AbstractTypeLayout(of: type(of: base).astType, definedIn: program)
    return ElementAddrInstruction(
      base: base,
      elementPath: elementPath,
      elementType: .address(l[elementPath].type),
      site: anchor)
  }

}
