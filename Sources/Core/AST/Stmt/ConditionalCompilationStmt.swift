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
    public func holds(for info: ConditionalCompilationConfiguration) -> Bool {
      switch self {
      case .`true`: return true
      case .`false`: return false
      case .operatingSystem(let id): return id == info.operatingSystem.description
      case .architecture(let id): return id == info.architecture.description
      case .feature(let id): return id == "freestanding" && info.freestanding
      case .compiler(let id): return id == "hc"
      case .compilerVersion(let comparison):
        return comparison.evaluate(for: info.compilerVersion)
      case .hyloVersion(let comparison):
        return comparison.evaluate(for: info.hyloVersion)
      case .not(let c):
        return !c.holds(for: info)
      }
    }

  }

  public let site: SourceRange

  /// The condition.
  public let condition: Condition

  /// The statements in the block.
  public let stmts: [AnyStmtID]

  /// The statements to be used if the condition is false.
  public let fallback: [AnyStmtID]

  /// Creates an instance with the given properties.
  public init(condition: Condition, stmts: [AnyStmtID], fallback: [AnyStmtID], site: SourceRange) {
    self.site = site
    self.condition = condition
    self.stmts = stmts
    self.fallback = fallback
  }

  /// Returns the statements that this expands to.
  public func expansion(for compiler: ConditionalCompilationConfiguration) -> [AnyStmtID] {
    if condition.holds(for: compiler) {
      return stmts
    } else {
      return fallback
    }
  }

}
