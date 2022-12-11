import Utils

/// The ID of a statement.
public protocol StmtID: NodeIDProtocol {}

extension NodeID: StmtID where Subject: Stmt {}

/// The type-erased ID of a statement.
public struct AnyStmtID: StmtID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from a statement ID.
  public init<T: StmtID>(_ other: T) { base = AnyNodeID(other) }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
