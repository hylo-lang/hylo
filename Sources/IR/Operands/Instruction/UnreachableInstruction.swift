import Core

/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct UnrechableInstruction: Terminator {

  public var site: SourceRange

  /// Creates an instance anchored at `anchor`.
  fileprivate init(anchoredAt anchor: SourceRange) {
    self.site = anchor
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var successors: [Block.ID] { [] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

  func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
    false
  }

}

extension Module {

  /// Creates an `unreachable` anchored at `anchor` that marks the execution path unreachable.
  func makeUnreachable(anchoredAt anchor: SourceRange) -> UnrechableInstruction {
    UnrechableInstruction(anchoredAt: anchor)
  }

}
