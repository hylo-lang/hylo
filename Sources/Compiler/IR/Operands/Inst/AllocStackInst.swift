/// Allocates memory on the stack.
public struct AllocStackInst: Inst {

  /// The type for which storage is allocated.
  public var allocatedType: Type

  /// The binding in source program to which the instruction corresponds, if any.
  public var binding: NodeID<VarDecl>?

  public var range: SourceRange?

  init(_ allocatedType: Type, binding: NodeID<VarDecl>? = nil, range: SourceRange? = nil) {
    self.allocatedType = allocatedType
    self.binding = binding
    self.range = range
  }

  public var types: [LoweredType] { [.address(allocatedType)] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { false }

  public func check(in module: Module) -> Bool {
    true
  }

}
