import Utils

/// The index of a pattern.
public protocol PatternIndex: NodeIndexProtocol {}

extension NodeIndex: PatternIndex where T: Pattern {}

/// The type-erased index of a pattern.
public struct AnyPatternIndex: PatternIndex {

  /// The underlying type-erased index.
  var base: AnyNodeIndex

  /// Creates a type-erased index from a pattern index.
  public init<T: PatternIndex>(_ other: T) {
    base = AnyNodeIndex(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Expr>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case BindingPattern.kind:
      return visitor.visit(binding: NodeIndex(rawValue: base.rawValue))
    case ExprPattern.kind:
      return visitor.visit(expr: NodeIndex(rawValue: base.rawValue))
    case NamePattern.kind:
      return visitor.visit(name: NodeIndex(rawValue: base.rawValue))
    case TuplePattern.kind:
      return visitor.visit(tuple: NodeIndex(rawValue: base.rawValue))
    case WildcardPattern.kind:
      return visitor.visit(wildcard: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
