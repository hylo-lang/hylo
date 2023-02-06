import Core

/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct UnrechableInstruction: Instruction {

  public var site: SourceRange

  init(site: SourceRange) {
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
