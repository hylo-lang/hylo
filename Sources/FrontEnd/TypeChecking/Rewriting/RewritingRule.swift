/// A rule in a rewriting system.
struct RewritingRule<Term: RewritingTerm>: Equatable {

  /// The left-hand side of the rule.
  let lhs: Term

  /// The right-hand side of the rule.
  let rhs: Term

  /// `true` if `self` has been simplified.
  private(set) var flags: RequirementRuleFlags

  /// Creates an instance rewriting `lhs` to `rhs`.
  init(_ lhs: Term, _ rhs: Term) {
    self.lhs = lhs
    self.rhs = rhs
    self.flags = []
  }

  /// `self` as a pair `(source, target)`.
  var deconstructed: (Term, Term) { (lhs, rhs) }

  /// `true` if `self` has been simplified.
  var isSimplified: Bool {
    flags.contains(.isLeftSimplified) || flags.contains(.isRightSimplified)
  }

  /// Returns a copy of `self` in which occurrences of `s` have been replaced by `t`.
  func substituting(_ s: Term, for t: Term) -> Self {
    .init(lhs.substituting(s, for: t), rhs.substituting(s, for: t))
  }

  /// Raises the flags `fs` in the rule.
  mutating func raiseFlags(_ fs: RequirementRuleFlags) {
    flags.insert(fs)
  }

}

extension RewritingRule where Term == RequirementTerm {

  /// Returns `true` if the generic parameters mentioned by this rule are contained in `ps`.
  func parametersAreContained(in ps: [GenericParameterDecl.ID]) -> Bool {
    if let a = lhs.rootParameter, !ps.contains(a) {
      return false
    } else {
      return rhs.rootParameter.map(ps.contains(_:)) ?? true
    }
  }

}

/// A set of flags associated with a rewriting rule.
struct RequirementRuleFlags: OptionSet {

  typealias RawValue = UInt8

  var rawValue: UInt8

  /// Indicates that a rule has been removed by left-simplification.
  static let isLeftSimplified = RequirementRuleFlags(rawValue: 1)

  /// Indicates that a rule has been removed by right-simplification.
  static let isRightSimplified = RequirementRuleFlags(rawValue: 2)

}
