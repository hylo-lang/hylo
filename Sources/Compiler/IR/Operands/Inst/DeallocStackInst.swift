/// Deallocates memory previously allocated by `alloc_stack`.
public struct DeallocStackInst: Inst {

  /// The location of the memory being deallocated.
  public let location: Operand

  public let range: SourceRange?

  init(_ location: Operand, range: SourceRange? = nil) {
    self.location = location
    self.range = range
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [location] }

  public var isTerminator: Bool { false }

  public func check(in module: Module) -> Bool {
    /// The location operand denotes the result of an `alloc_stack` instruction.
    guard let l = location.inst else { return false }
    return module[l.function][l.block][l.address] is AllocStackInst
  }

}
