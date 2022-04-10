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

  public var typeID: ObjectIdentifier { base.typeID }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Stmt>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch base.typeID {
    case BraceStmt.typeID:
      return visitor.visit(brace: NodeIndex(rawValue: base.rawValue))
    case BreakStmt.typeID:
      return visitor.visit(break: NodeIndex(rawValue: base.rawValue))
    case ContinueStmt.typeID:
      return visitor.visit(continue: NodeIndex(rawValue: base.rawValue))
    case DeclStmt.typeID:
      return visitor.visit(decl: NodeIndex(rawValue: base.rawValue))
    case DoWhileStmt.typeID:
      return visitor.visit(doWhile: NodeIndex(rawValue: base.rawValue))
    case ExprStmt.typeID:
      return visitor.visit(expr: NodeIndex(rawValue: base.rawValue))
    case ForStmt.typeID:
      return visitor.visit(for: NodeIndex(rawValue: base.rawValue))
    case ReturnStmt.typeID:
      return visitor.visit(return: NodeIndex(rawValue: base.rawValue))
    case WhileStmt.typeID:
      return visitor.visit(while: NodeIndex(rawValue: base.rawValue))
    case YieldStmt.typeID:
      return visitor.visit(yield: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
