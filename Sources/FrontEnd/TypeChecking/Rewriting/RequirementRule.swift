/// A rule in a requirement rewritting system.
struct RequirementRule {

  /// The left-hand side of the rule.
  let lhs: RequirementTerm

  /// The right-hand side of the rule.
  let rhs: RequirementTerm

  /// `true` if `self` has been simplified.
  private(set) var flags: Flags

  /// Creates an instance rewriting `lhs` to `rhs`.
  init(_ lhs: RequirementTerm, _ rhs: RequirementTerm) {
    self.lhs = lhs
    self.rhs = rhs
    self.flags = []
  }

  /// `self` as a pair `(source, target)`.
  var deconstructed: (RequirementTerm, RequirementTerm) { (lhs, rhs) }

  /// `true` if `self` has been simplified.
  var isSimplified: Bool {
    flags.contains(.isLeftSimplified) || flags.contains(.isRightSimplified)
  }

  /// Returns a copy of `self` in which occurrences of `s` have been replaced by `t`.
  func substituting(_ s: RequirementTerm, for t: RequirementTerm) -> RequirementRule {
    .init(lhs.substituting(s, for: t), rhs.substituting(s, for: t))
  }

  /// Returns `true` if the generic parameters mentioned by this rule are contained in `ps`.
  func parametersAreContained(in ps: [GenericParameterDecl.ID]) -> Bool {
    if let a = lhs.rootParameter, !ps.contains(a) {
      return false
    } else {
      return rhs.rootParameter.map(ps.contains(_:)) ?? true
    }
  }

  /// Raises the flags `fs` in the rule.
  mutating func raiseFlags(_ fs: Flags) {
    flags.insert(fs)
  }

  /// A set of flags associated with a rewriting rule.
  struct Flags: OptionSet {

    typealias RawValue = UInt8

    var rawValue: UInt8

    /// Indicates that a rule has been removed by left-simplification.
    static let isLeftSimplified = Flags(rawValue: 1)

    /// Indicates that a rule has been removed by right-simplification.
    static let isRightSimplified = Flags(rawValue: 2)

  }

}
