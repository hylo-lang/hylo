/// A type that implements a visitation method for each kind of pattern node.
public protocol PatternVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(binding: NodeID<BindingPattern>) -> Result

  mutating func visit(expr: NodeID<ExprPattern>) -> Result

  mutating func visit(name: NodeID<NamePattern>) -> Result

  mutating func visit(tuple: NodeID<TuplePattern>) -> Result

  mutating func visit(wildcard: NodeID<WildcardPattern>) -> Result

}
