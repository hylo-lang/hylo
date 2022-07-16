// Borrows an access on an object or sub-object.
public struct BorrowInst: Inst, BorrowInstProtocol {

  /// The capability being borrowed.
  public var capability: ProjectionType.Capability

  /// The type of the borrowed access.
  public var borrowedType: LoweredType

  /// The value of the root object on which an access is borrowed.
  public var value: Operand

  /// A sequence of indices identifying a sub-object of `value`.
  public var path: [Int]

  /// The binding in source program to which the instruction corresponds, if any.
  public var binding: NodeID<VarDecl>?

  public var range: SourceRange?

  init(
    _ capability: ProjectionType.Capability,
    _ borrowedType: LoweredType,
    from value: Operand,
    at path: [Int] = [],
    binding: NodeID<VarDecl>? = nil,
    range: SourceRange? = nil
  ) {
    self.borrowedType = borrowedType
    self.capability = capability
    self.value = value
    self.path = path
    self.binding = binding
    self.range = range
  }

  public var types: [LoweredType] { [borrowedType] }

  public var operands: [Operand] { [value] }

  public func check(in module: Module) -> Bool {
    // Instruction result has an address type.
    if !borrowedType.isAddress { return false }

    // Operand has an address type.
    if !module.type(of: value).isAddress { return false }

    return true
  }

}
