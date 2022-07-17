// Borrows an access on an object or sub-object.
public struct BorrowInst: Inst, BorrowInstProtocol {

  /// The capability being borrowed.
  public var capability: ProjectionType.Capability

  /// The type of the borrowed access.
  public var borrowedType: LoweredType

  /// The location of the root object on which an access is borrowed.
  public var location: Operand

  /// A sequence of indices identifying a sub-location of `location`.
  public var path: [Int]

  /// The binding in source program to which the instruction corresponds, if any.
  public var binding: NodeID<VarDecl>?

  public var range: SourceRange?

  init(
    _ capability: ProjectionType.Capability,
    _ borrowedType: LoweredType,
    from location: Operand,
    at path: [Int] = [],
    binding: NodeID<VarDecl>? = nil,
    range: SourceRange? = nil
  ) {
    self.borrowedType = borrowedType
    self.capability = capability
    self.location = location
    self.path = path
    self.binding = binding
    self.range = range
  }

  public var types: [LoweredType] { [borrowedType] }

  public var operands: [Operand] { [location] }

  public var isTerminator: Bool { false }

  public func check(in module: Module) -> Bool {
    // Instruction result has an address type.
    if !borrowedType.isAddress { return false }

    // Operand has an address type.
    if !module.type(of: location).isAddress { return false }

    return true
  }

}
