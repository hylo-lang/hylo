import Core

// Borrows an access on an object.
public struct BorrowInstruction: Instruction {

  /// The capability being borrowed.
  public let capability: AccessEffect

  /// The type of the borrowed access.
  public let borrowedType: LoweredType

  /// The location of the root object on which an access is borrowed.
  public let location: Operand

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: VarDecl.Typed?

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    borrowedType: LoweredType,
    capability: AccessEffect,
    location: Operand,
    binding: VarDecl.Typed? = nil,
    site: SourceRange
  ) {
    self.borrowedType = borrowedType
    self.capability = capability
    self.location = location
    self.binding = binding
    self.site = site
  }

  public var types: [LoweredType] { [borrowedType] }

  public var operands: [Operand] { [location] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Capability may not be `sink`.
    if capability == .sink { return false }

    // Instruction result has an address type.
    if !borrowedType.isAddress { return false }

    // Operand has an address type.
    if !module.type(of: location).isAddress { return false }

    return true
  }

}

extension Module {

  /// Creates a `borrow` anchored at `anchor` that takes `capability` from `source`.
  func makeBorrow(
    _ capability: AccessEffect,
    from source: Operand,
    anchoredAt anchor: SourceRange
  ) -> BorrowInstruction {
    let i = BorrowInstruction(
      borrowedType: type(of: source),
      capability: capability,
      location: source,
      site: anchor)
    precondition(i.isWellFormed(in: self))
    return i
  }

}
