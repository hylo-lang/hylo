import FrontEnd

/// Allocates memory on the stack.
public struct AllocStack: Instruction {

  /// The type for which storage is allocated.
  public let allocatedType: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(allocatedType: AnyType, site: SourceRange) {
    precondition(allocatedType.isCanonical)
    self.allocatedType = allocatedType
    self.site = site
  }

  public var result: IR.`Type`? {
    .address(allocatedType)
  }

  public func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension AllocStack: CustomStringConvertible {

  public var description: String {
    "alloc_stack \(allocatedType)"
  }

}
