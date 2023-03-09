import Core

/// Branches depending on a condition evaluated statically during flow-sensitive analysis.
///
/// - Invariant: The successor blocks of this instruction must be immediately dominated by it.
public struct StaticBranchInstruction: Terminator {

  /// The property expected on the operand of a static branch.
  public enum Predicate {

    /// The operand is initialized.
    case initialized

  }

  /// The operand on which the branch depends.
  public let subject: Operand

  /// The property tested on `subject`.
  public let predicate: Predicate

  /// The target of the branch if `predicate` holds on `subject`.
  public private(set) var targetIfTrue: Block.ID

  /// The target of the branch if `predicate` does not hold on `subject`.
  public private(set) var targetIfFalse: Block.ID

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  fileprivate init(
    if subject: Operand,
    is predicate: Predicate,
    then targetIfTrue: Block.ID,
    else targetIfFalse: Block.ID,
    anchoredAt anchor: SourceRange
  ) {
    self.subject = subject
    self.predicate = predicate
    self.targetIfTrue = targetIfTrue
    self.targetIfFalse = targetIfFalse
    self.site = anchor
  }

  public var types: [LoweredType] = []

  public var operands: [Operand] { [subject] }

  public var successors: [Block.ID] { [targetIfTrue, targetIfFalse] }

  mutating func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
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

extension StaticBranchInstruction: CustomStringConvertible {

  public var description: String {
    "static_branch \(predicate)(\(subject)), \(targetIfTrue), \(targetIfFalse)"
  }

}

extension Module {

  /// Creates a `static_branch` anchored at `anchor` that jumps to `targetIfTrue` if `predicate`
  /// holds on `subject` or to `targetIfFalse` otherwise.
  ///
  /// - Parameters:
  ///   - subject: The value on which `predicate` is tested.
  ///   - predicate: A property that can be checked statically.
  ///   - targetIfTrue: The block in which control flow jumps if `predicate` holds.
  ///   - targetIfFalse: The block in which control flow jumps if `predicate` doesn't hold.
  func makeStaticBranch(
    if subject: Operand,
    is predicate: StaticBranchInstruction.Predicate,
    then targetIfTrue: Block.ID,
    else targetIfFalse: Block.ID,
    anchoredAt anchor: SourceRange
  ) -> StaticBranchInstruction {
    switch predicate {
    case .initialized:
      precondition(type(of: subject).isAddress)
    }

    return StaticBranchInstruction(
      if: subject, is: predicate, then: targetIfTrue, else: targetIfFalse,
      anchoredAt: anchor)
  }

}
