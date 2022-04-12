import Utils

/// The ID of a pattern.
public protocol PatternID: NodeIDProtocol {}

extension NodeID: PatternID where T: Pattern {}

/// The type-erased ID of a pattern.
public struct AnyPatternID: PatternID {

  /// The underlying type-erased ID.
  var base: AnyNodeID

  /// Creates a type-erased ID from a pattern ID.
  public init<T: PatternID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this ID, or `nil` if the type conversion failed.
  public func convert<T: Expr>(to: T.Type) -> NodeID<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case BindingPattern.kind:
      return visitor.visit(binding: NodeID(rawValue: base.rawValue))
    case ExprPattern.kind:
      return visitor.visit(expr: NodeID(rawValue: base.rawValue))
    case NamePattern.kind:
      return visitor.visit(name: NodeID(rawValue: base.rawValue))
    case TuplePattern.kind:
      return visitor.visit(tuple: NodeID(rawValue: base.rawValue))
    case WildcardPattern.kind:
      return visitor.visit(wildcard: NodeID(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
