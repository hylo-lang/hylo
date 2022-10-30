import Utils

/// The ID of a pattern.
public protocol PatternID: NodeIDProtocol {}

extension PatternID {

  /// Calls the `visitor.visit` method corresponding to the type of this node.
  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    switch kind {
    case .bindingPattern:
      return visitor.visit(binding: NodeID(rawValue: rawValue))
    case .exprPattern:
      return visitor.visit(expr: NodeID(rawValue: rawValue))
    case .namePattern:
      return visitor.visit(name: NodeID(rawValue: rawValue))
    case .tuplePattern:
      return visitor.visit(tuple: NodeID(rawValue: rawValue))
    case .wildcardPattern:
      return visitor.visit(wildcard: NodeID(rawValue: rawValue))
    default:
      unreachable()
    }
  }

}

extension NodeID: PatternID where Subject: Pattern {}

/// The type-erased ID of a pattern.
public struct AnyPatternID: PatternID {

  /// The underlying type-erased ID.
  private var base: AnyNodeID

  /// Creates a type-erased ID from a pattern ID.
  public init<T: PatternID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
