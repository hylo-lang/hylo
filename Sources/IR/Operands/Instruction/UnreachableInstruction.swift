import Core

/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct UnrechableInstruction: Instruction {

  public var site: SourceRange

  /// Creates an instance anchored at `anchor`.
  fileprivate init(anchoredAt anchor: SourceRange) {
    self.site = anchor
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}

extension Module {

  /// Creates an `unreachable` anchored at `anchor` that marks the execution path unreachable.
  func makeUnreachable(anchoredAt anchor: SourceRange) -> UnrechableInstruction {
    UnrechableInstruction(anchoredAt: anchor)
  }

}
