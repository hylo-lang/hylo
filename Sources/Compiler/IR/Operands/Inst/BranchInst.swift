/// Branches unconditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct BranchInst: Inst {

  /// The target of the branch.
  public var target: Block.ID

  public var range: SourceRange?

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func check(in module: Module) -> Bool {
    true
  }

}
