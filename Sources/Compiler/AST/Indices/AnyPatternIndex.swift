import Utils

/// A type-erased index of a pattern.
public struct AnyPatternIndex: NodeIndexProtocol {

  /// The underlying type-erased index.
  var base: AnyNodeIndex

  /// Creates a type-erased index from a typed index.
  public init<T: Pattern>(_ typedIndex: NodeIndex<T>) {
    base = AnyNodeIndex(typedIndex)
  }

  public var rawValue: Int { base.rawValue }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Expr>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    switch base.typeID {
    case BindingPattern.typeID:
      return visitor.visit(binding: NodeIndex(rawValue: base.rawValue))
    case ExprPattern.typeID:
      return visitor.visit(expr: NodeIndex(rawValue: base.rawValue))
    case NamePattern.typeID:
      return visitor.visit(name: NodeIndex(rawValue: base.rawValue))
    case TuplePattern.typeID:
      return visitor.visit(tuple: NodeIndex(rawValue: base.rawValue))
    case WildcardPattern.typeID:
      return visitor.visit(wildcard: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
