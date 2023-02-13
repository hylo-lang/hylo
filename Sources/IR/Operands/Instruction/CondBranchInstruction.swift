import Core

/// Branches conditionally to the start of a basic block.
public struct CondBranchInstruction: Instruction {

  /// A Boolean condition.
  public let condition: Operand

  /// The target of the branch if `condition` is true.
  public let targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public let targetIfFalse: Block.ID

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    condition: Operand,
    targetIfTrue: Block.ID,
    targetIfFalse: Block.ID,
    site: SourceRange
  ) {
    self.condition = condition
    self.targetIfTrue = targetIfTrue
    self.targetIfFalse = targetIfFalse
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [condition] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    /// The condition operand has an object type.
    return !module.type(of: condition).isAddress
  }

}

extension Module {

  /// Creates a `cond_branch` anchored at `anchor` that jumps to `targetIfTrue` if `condition` is
  /// true or `targetIfFalse` otherwise.
  ///
  /// - Parameters:
  ///   - condition: The condition tested to select the jump destination. Must a built-in `i1`
  ///     object.
  ///   - targetIfTrue: The block in which control flow jumps if `condition` is true.
  ///   - targetIfFalse: The block in which control flow jumps if `condition` is false.
  func makeCondBranch(
    if condition: Operand,
    then targetIfTrue: Block.ID,
    else targetIfFalse: Block.ID,
    anchoredAt anchor: SourceRange
  ) -> CondBranchInstruction {
    precondition(type(of: condition) == .object(BuiltinType.i(1)))

    return CondBranchInstruction(
      condition: condition,
      targetIfTrue: targetIfTrue,
      targetIfFalse: targetIfFalse,
      site: anchor)
  }

}
