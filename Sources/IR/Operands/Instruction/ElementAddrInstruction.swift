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
  func makeElementAddr(
    _ base: Operand,
    at elementPath: [Int],
    anchoredAt anchor: SourceRange
  ) -> ElementAddrInstruction {
    let l = AbstractTypeLayout(of: type(of: base).astType, definedIn: program)
    let i = ElementAddrInstruction(
      base: base,
      elementPath: elementPath,
      elementType: .address(l[elementPath].type),
      site: anchor)
    precondition(i.isWellFormed(in: self))
    return i
  }

}
