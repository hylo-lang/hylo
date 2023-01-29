import Core

/// Deallocates memory previously allocated by `alloc_stack`.
public struct DeallocStackInstruction: Instruction {

  /// The location of the memory being deallocated.
  public let location: Operand

  public let site: SourceRange

  init(_ location: Operand, site: SourceRange) {
    self.location = location
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [location] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    /// The location operand denotes the result of an `alloc_stack` instruction.
    guard let l = location.instruction else { return false }
    return module[l] is AllocStackInstruction
  }

}
