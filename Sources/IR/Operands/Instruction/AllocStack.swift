import FrontEnd

/// Allocates memory on the stack.
public struct AllocStack: Instruction {

  /// The type for which storage is allocated.
  public let allocatedType: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(allocatedType: AnyType, site: SourceRange) {
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

extension Module {

  /// Creates an `alloc_stack` anchored at `site` that allocates storage of type `t`.
  ///
  /// - Requires: `t` is canonical.
  func makeAllocStack(_ t: AnyType, at site: SourceRange) -> AllocStack {
    precondition(t.isCanonical)
    return .init(allocatedType: t, site: site)
  }

}
