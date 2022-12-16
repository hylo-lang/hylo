import Core

/// Allocates memory on the stack.
public struct AllocStackInstruction: Instruction {

  /// The type for which storage is allocated.
  public let allocatedType: AnyType

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: NodeID<VarDecl>?

  public let range: SourceRange?

  init(_ allocatedType: AnyType, binding: NodeID<VarDecl>? = nil, range: SourceRange? = nil) {
    self.allocatedType = allocatedType
    self.binding = binding
    self.range = range
  }

  public var types: [LoweredType] { [.address(allocatedType)] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
