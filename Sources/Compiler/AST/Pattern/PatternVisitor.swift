/// A type that implements a visitation method for each kind of pattern node.
public protocol PatternVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(binding: NodeIndex<BindingPattern>) -> Result

  mutating func visit(expr: NodeIndex<ExprPattern>) -> Result

  mutating func visit(name: NodeIndex<NamePattern>) -> Result

  mutating func visit(tuple: NodeIndex<TuplePattern>) -> Result

  mutating func visit(wildcard: NodeIndex<WildcardPattern>) -> Result

}
