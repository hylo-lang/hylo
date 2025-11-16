import FrontEnd

/// Branches conditionally to the start of a basic block.
public struct CondBranch: Terminator {

  /// A Boolean condition.
  public private(set) var condition: Operand

  /// The target of the branch if `condition` is true.
  public private(set) var targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public private(set) var targetIfFalse: Block.ID

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(
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

  public var operands: [Operand] {
    [condition]
  }

  public var successors: [Block.ID] { [targetIfTrue, targetIfFalse] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    condition = new
  }

  mutating func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool {
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
