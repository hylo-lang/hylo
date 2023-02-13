import Core

/// Allocates memory on the stack.
public struct AllocStackInstruction: Instruction {

  /// The type for which storage is allocated.
  public let allocatedType: AnyType

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: NodeID<VarDecl>?

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(allocatedType: AnyType, binding: NodeID<VarDecl>?, site: SourceRange) {
    self.allocatedType = allocatedType
    self.binding = binding
    self.site = site
  }

  public var types: [LoweredType] { [.address(allocatedType)] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}

extension Module {

  /// Creates an `alloc_stack` anchored at `anchor` that allocates storage of type `storageType`,
  /// associated with `storageDecl`.
  ///
  /// - Parameters:
  ///   - storageType: The type of the allocated storage.
  ///   - storageDecl: The declaration to associated with the returned instruction, if any.
  func makeAllocStack(
    _ storageType: AnyType,
    for storageDecl: NodeID<VarDecl>? = nil,
    anchoredAt anchor: SourceRange
  ) -> AllocStackInstruction {
    return AllocStackInstruction(
      allocatedType: program.relations.canonical(storageType),
      binding: storageDecl,
      site: anchor)
  }

}
