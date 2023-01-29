import Core

// Borrows an access on an object or sub-object.
public struct BorrowInstruction: Instruction {

  /// The capability being borrowed.
  public let capability: AccessEffect

  /// The type of the borrowed access.
  public let borrowedType: LoweredType

  /// The location of the root object on which an access is borrowed.
  public let location: Operand

  /// A sequence of indices identifying a sub-location of `location`.
  public let path: [Int]

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: VarDecl.Typed?

  public let site: SourceRange

  init(
    _ capability: AccessEffect,
    _ borrowedType: LoweredType,
    from location: Operand,
    at path: [Int] = [],
    binding: VarDecl.Typed? = nil,
    site: SourceRange
  ) {
    self.borrowedType = borrowedType
    self.capability = capability
    self.location = location
    self.path = path
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
