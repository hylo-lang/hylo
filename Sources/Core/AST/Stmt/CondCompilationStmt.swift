/// A conditional-compilation statement.
public struct CondCompilationStmt: Stmt {

  public enum VersionComparison: Codable {

    case greaterOrEqual
    case less

  }

  public enum Condition: Codable, Equatable {

    case `true`
    case `false`
    case os(Identifier)
    case arch(Identifier)
    case compiler(Identifier)
    case compilerVersion(comparison: VersionComparison, versionNumber: [Int])
    case hyloVersion(comparison: VersionComparison, versionNumber: [Int])

    /// Indicates if we may need to skip parsing the body of the conditional-compilation.
    public var mayNotNeedParsing: Bool {
      switch self {
      case .compiler: return true
      case .compilerVersion: return true
      case .hyloVersion: return true
      default: return false
      }
    }

    /// Indicates if the condition is true for the current instance of the compiler.
    public func isTrue() -> Bool {
      switch self {
      case .`true`: return true
      // TODO
      default: return false
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

  public init(condition: Condition, stmts: [AnyStmtID], fallback: [AnyStmtID], site: SourceRange) {
    self.site = site
    self.condition = condition
    self.stmts = stmts
    self.fallback = fallback
  }

}
