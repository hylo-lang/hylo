import FrontEnd

/// Branches conditionally to the start of a basic block.
public struct CondBranch: Terminator {

  /// A Boolean condition.
  public private(set) var condition: Operand

  /// The target of the branch if `condition` is true.
  public private(set) var targetIfTrue: Block.AbsoluteID

  /// The target of the branch if `condition` is false.
  public private(set) var targetIfFalse: Block.AbsoluteID

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    condition: Operand,
    targetIfTrue: Block.AbsoluteID,
    targetIfFalse: Block.AbsoluteID,
    site: SourceRange
  ) {
    self.condition = condition
    self.targetIfTrue = targetIfTrue
    self.targetIfFalse = targetIfFalse
    self.site = site
  }

  public var operands: [Operand] {
    [condition]
  }

  public var successors: [Block.AbsoluteID] { [targetIfTrue, targetIfFalse] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    condition = new
  }

  mutating func replaceSuccessor(_ old: Block.AbsoluteID, with new: Block.AbsoluteID) -> Bool {
    precondition(new.function == targetIfTrue.function)
    if targetIfTrue == old {
      targetIfTrue = new
      return true
    } else if targetIfFalse == old {
      targetIfFalse = new
      return true
    } else {
      return false
    }
  }

}

extension CondBranch: CustomStringConvertible {

  public var description: String {
    "cond_branch \(condition), \(targetIfTrue), \(targetIfFalse)"
  }

}

extension Module {

  /// Creates a `cond_branch` anchored at `site` that jumps to `targetIfTrue` if `condition` is
  /// true or `targetIfFalse` otherwise.
  ///
  /// - Parameters:
  ///   - condition: The condition tested to select the jump destination. Must a built-in `i1`
  ///     object.
  ///   - targetIfTrue: The block in which control flow jumps if `condition` is true.
  ///   - targetIfFalse: The block in which control flow jumps if `condition` is false.
  func makeCondBranch(
    if condition: Operand,
    then targetIfTrue: Block.AbsoluteID,
    else targetIfFalse: Block.AbsoluteID,
    in f: Function.ID,
    at site: SourceRange
  ) -> CondBranch {
    precondition(type(of: condition, in: f) == .object(BuiltinType.i(1)))
    return .init(
      condition: condition,
      targetIfTrue: targetIfTrue,
      targetIfFalse: targetIfFalse,
      site: site)
  }

}
