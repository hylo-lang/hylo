/// A conditional-compilation statement.
public struct ConditionalCompilationStmt: Stmt {

  public enum VersionComparison: Codable {

    case greaterOrEqual

    case less

    func compare<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
      switch self {
      case .greaterOrEqual: return !(lhs < rhs)
      case .less: return lhs < rhs
      }
    }

  }

  public enum Condition: Codable, Equatable {

    case `true`
    case `false`
    case os(Identifier)
    case arch(Identifier)
    case compiler(Identifier)
    case compilerVersion(comparison: VersionComparison, versionNumber: CompilerInfo.VersionNumber)
    case hyloVersion(comparison: VersionComparison, versionNumber: CompilerInfo.VersionNumber)

    /// `true` iff the body of the conditional-compilation shouldn't be parsed.
    public var mayNotNeedParsing: Bool {
      switch self {
      case .compiler: return true
      case .compilerVersion: return true
      case .hyloVersion: return true
      default: return false
      }
    }

    /// Returns `true` iff `self` holds for the current process.
    public func holds(for info: CompilerInfo) -> Bool {
      switch self {
      case .`true`: return true
      case .`false`: return false
      case .os(let id): return id == info.os
      case .arch(let id): return id == info.arch
      case .compiler(let id): return id == info.compiler
      case .compilerVersion(let comparison, let version):
        return comparison.compare(info.compilerVersion, version)
      case .hyloVersion(let comparison, let version):
        return comparison.compare(info.hyloVersion, version)
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

  /// Returns the statements that this expands to.
  public var expansion: [AnyStmtID] {
    if condition.isTrue(for: CompilerInfo.instance) {
      return stmts
    } else {
      return fallback
    }
  }

}
