import Core

/// Deallocates memory previously allocated by `alloc_stack`.
public struct DeallocStackInstruction: Instruction {

  /// The location of the memory being deallocated.
  public private(set) var location: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(location: Operand, site: SourceRange) {
    self.location = location
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [location] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    location = new
  }

}

extension Module {

  /// Creates a `dealloc_stack` anchored at `anchor` that deallocates memory previously allocated
  /// by `alloc`.
  ///
  /// - Parameters:
  ///   - alloc: The address of the memory to deallocate. Must be the result of `alloc`.
  func makeDeallocStack(
    for alloc: Operand,
    anchoredAt anchor: SourceRange
  ) -> DeallocStackInstruction {
    precondition(alloc.instruction.map({ self[$0] is AllocStackInstruction }) ?? false)
    return DeallocStackInstruction(location: alloc, site: anchor)
  }

}
