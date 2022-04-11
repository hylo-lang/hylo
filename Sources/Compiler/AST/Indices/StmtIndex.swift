import Utils

/// The index of a statement.
public protocol StmtIndex: NodeIndexProtocol {}

extension NodeIndex: StmtIndex where T: Stmt {}

/// The type-erased index of a statement.
public struct AnyStmtIndex: StmtIndex {

  /// The underlying type-erased index.
  var base: AnyNodeIndex

  /// Creates a type-erased index from a statement index.
  public init<T: StmtIndex>(_ other: T) {
    base = AnyNodeIndex(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Stmt>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case BraceStmt.kind:
      return visitor.visit(brace: NodeIndex(rawValue: base.rawValue))
    case BreakStmt.kind:
      return visitor.visit(break: NodeIndex(rawValue: base.rawValue))
    case ContinueStmt.kind:
      return visitor.visit(continue: NodeIndex(rawValue: base.rawValue))
    case DeclStmt.kind:
      return visitor.visit(decl: NodeIndex(rawValue: base.rawValue))
    case DoWhileStmt.kind:
      return visitor.visit(doWhile: NodeIndex(rawValue: base.rawValue))
    case ExprStmt.kind:
      return visitor.visit(expr: NodeIndex(rawValue: base.rawValue))
    case ForStmt.kind:
      return visitor.visit(for: NodeIndex(rawValue: base.rawValue))
    case ReturnStmt.kind:
      return visitor.visit(return: NodeIndex(rawValue: base.rawValue))
    case WhileStmt.kind:
      return visitor.visit(while: NodeIndex(rawValue: base.rawValue))
    case YieldStmt.kind:
      return visitor.visit(yield: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
