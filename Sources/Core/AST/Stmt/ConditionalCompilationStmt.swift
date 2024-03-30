/// A conditional-compilation statement.
public struct ConditionalCompilationStmt: Stmt {

  /// A comparison test for semantic version.
  public enum VersionComparison: Codable, Equatable {

    /// Represents "_ >= payload".
    case greaterOrEqual(SemanticVersion)

    /// Represents "_ < payload".
    case less(SemanticVersion)

    /// Evaluate the comparison predicate for `lhs`.
    func evaluate(for lhs: SemanticVersion) -> Bool {
      switch self {
      case .greaterOrEqual(let target):
        return !(lhs < target)
      case .less(let target):
        return lhs < target
      }
    }

  }

  /// A condition in a conditional compilation statement.
  public indirect enum Condition: Codable, Equatable {

    /// Always holds.
    case `true`

    /// Never holds.
    case `false`

    /// Holds iff the operating system for which the code is compiled matches the payload.
    case operatingSystem(Identifier)

    /// Holds iff the processor architecture for which the code is compiled matches the payload.
    case architecture(Identifier)

    /// Holds iff the payload matches any of the feature enabled in the compiler.
    case feature(Identifier)

    /// Holds iff the name of the compiler processing the file matches the payload.
    case compiler(Identifier)

    /// Holds iff the version of the compiler processing the file, satisfies the `comparison`.
    case compilerVersion(comparison: VersionComparison)

    /// Holds iff the version of Hylo for which this file is compiled, satisfies `comparison`.
    case hyloVersion(comparison: VersionComparison)

    /// Holds iff the payload doesn't.
    case not(Condition)

    /// `true` iff the body of the conditional-compilation shouldn't be parsed.
    public var mayNotNeedParsing: Bool {
      switch self {
      case .compiler:
        return true
      case .compilerVersion:
        return true
      case .hyloVersion:
        return true
      case .not(let c):
        return c.mayNotNeedParsing
      default:
        return false
      }
    }

    /// Returns `true` iff `self` holds for the current process.
    public func holds(for factors: ConditionalCompilationFactors) -> Bool {
      switch self {
      case .`true`:
        return true
      case .`false`:
        return false
      case .operatingSystem(let id):
        return id == factors.operatingSystem.description
      case .architecture(let id):
        return id == factors.architecture.description
      case .feature(let id):
        return id == "freestanding" && factors.freestanding
      case .compiler(let id):
        return id == "hc"
      case .compilerVersion(let comparison):
        return comparison.evaluate(for: factors.compilerVersion)
      case .hyloVersion(let comparison):
        return comparison.evaluate(for: factors.hyloVersion)
      case .not(let c):
        return !c.holds(for: factors)
      }
    }

  }

  /// A condition in a conditional compilation statement expressed as a composition of predicates.
  public indirect enum ConditionTree: Codable {

    /// A proposition or the negation of a proposition.
    case operand(Condition)

    /// A conjunction of conditions.
    case and(ConditionTree, ConditionTree)

    /// A disjunction of conditions.
    case or(ConditionTree, ConditionTree)

    public enum ConditionKind: String {

      case holdOnly

      case skipMain

      case skipElse

    }

    /// Visit the SequenceCondition tail to calculate the right case for executing the right branch.
    private func unroll(
      for factors: ConditionalCompilationFactors, conditionCase: ConditionKind
    ) -> Bool {
      switch self {
      case .and(let leftSubtree, let rightSubtree):
        return leftSubtree.unroll(for: factors, conditionCase: conditionCase)
          && rightSubtree.unroll(for: factors, conditionCase: conditionCase)

      case .or(let leftSubtree, let rightSubtree):
        return leftSubtree.unroll(for: factors, conditionCase: conditionCase)
          || rightSubtree.unroll(for: factors, conditionCase: conditionCase)

      case .operand(let cond):
        switch conditionCase {
        case .holdOnly: return cond.holds(for: factors)
        case .skipMain: return cond.mayNotNeedParsing && !cond.holds(for: factors)
        case .skipElse: return cond.mayNotNeedParsing && cond.holds(for: factors)
        }
      }
    }

    /// Return `true` iff the the full condition is satisfied.
    public func mustSkipMainBranch(for factors: ConditionalCompilationFactors) -> Bool {
      self.unroll(for: factors, conditionCase: ConditionKind.skipMain)
    }

    /// Return `true` iff the the full condition is unsatisfied.
    public func mustSkipElseBranch(for factors: ConditionalCompilationFactors) -> Bool {
      self.unroll(for: factors, conditionCase: ConditionKind.skipElse)
    }

    /// Return `true` iff the the full condition is satisfied.
    fileprivate func holds(for factors: ConditionalCompilationFactors) -> Bool {
      self.unroll(for: factors, conditionCase: ConditionKind.holdOnly)
    }
  }

  public let site: SourceRange

  /// The condition.
  public let condition: ConditionTree

  /// The statements in the block.
  public let stmts: [AnyStmtID]

  /// The statements to be used if the condition is false.
  public let fallback: [AnyStmtID]

  /// Creates an instance with the given properties.
  public init(
    condition: ConditionTree, stmts: [AnyStmtID], fallback: [AnyStmtID], site: SourceRange
  ) {
    self.site = site
    self.condition = condition
    self.stmts = stmts
    self.fallback = fallback
  }

  /// Returns the statements that this expands to.
  public func expansion(for factors: ConditionalCompilationFactors) -> [AnyStmtID] {
    condition.holds(for: factors) ? stmts : fallback
  }

}
