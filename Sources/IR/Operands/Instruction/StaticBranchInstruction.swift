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

  /// A table describing the cases of a static branch as a map from predicate to its target.
  ///
  /// A `nil` predicate denotes the default case of the branch.
  public typealias Cases = [Predicate?: Block.ID]

  /// The operand on which the branch depends.
  public let subject: Operand

  /// The predicated cases of the branch.
  public private(set) var cases: Cases

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with given properties.
  fileprivate init(subject: Operand, cases: Cases, site: SourceRange) {
    self.subject = subject
    self.cases = cases
    self.site = site
  }

  public var types: [LoweredType] = []

  public var operands: [Operand] { [subject] }

  public var successors: [Block.ID] { cases.map(\.value) }

  mutating func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
    for (p, t) in cases where t == old {
      cases[p] = new
      return true
    }
    return false
  }

}

extension StaticBranchInstruction: CustomStringConvertible {

  public var description: String {
    let branches = cases.map { (p, t) in
      let predicate = p.map(String.init(describing:)) ?? "default"
      return "\(predicate) => \(t)"
    }
    return "static_branch \(list: branches)"
  }

}

extension Module {

  /// Creates a `static_branch` anchored at `anchor` that jumps to the target in `cases` depending
  /// for which the corresponding predicate holds on `subject`.
  ///
  /// - Parameters:
  ///   - subject: The value on which predicates are tested.
  ///   - cases: The cases of the branch.
  /// - Requires: The non-default predicates of the cases must be mutually exclusive.
  func makeStaticBranch(
    switch subject: Operand,
    cases: StaticBranchInstruction.Cases,
    anchoredAt anchor: SourceRange
  ) -> StaticBranchInstruction {
    return .init(subject: subject, cases: cases, site: anchor)
  }

}
