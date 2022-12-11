import Utils

/// The ID of a declaration.
public protocol DeclID: NodeIDProtocol {}

extension NodeID: DeclID where Subject: Decl {}

/// The type-erased ID of a declaration.
public struct AnyDeclID: DeclID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from a declaration ID.
  public init<T: DeclID>(_ other: T) { base = AnyNodeID(other) }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
