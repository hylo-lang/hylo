/// Marks this execution path as unreachable, causing a fatal error otherwise.
public struct UnrechableInst: Inst {

  public var range: SourceRange?

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func check(in module: Module) -> Bool {
    true
  }

}
