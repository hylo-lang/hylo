import Core

/// Allocates memory on the stack.
public struct AllocStackInstruction: Instruction {

  /// The type for which storage is allocated.
  public let allocatedType: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(allocatedType: AnyType, site: SourceRange) {
    self.allocatedType = allocatedType
    self.site = site
  }

  public var types: [IR.LoweredType] { [.address(allocatedType)] }

  public var operands: [Operand] { [] }

  public func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension AllocStackInstruction: CustomStringConvertible {

  public var description: String {
    "alloc_stack \(allocatedType)"
  }

}

extension Module {

  /// Creates an `alloc_stack` anchored at `site` that allocates storage of type `allocatedType`.
  func makeAllocStack(_ allocatedType: AnyType, at site: SourceRange) -> AllocStackInstruction {
    .init(allocatedType: program.relations.canonical(allocatedType), site: site)
  }

}
