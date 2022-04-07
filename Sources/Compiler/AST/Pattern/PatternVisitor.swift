/// A type that implements a visitation method for each kind of pattern node.
public protocol PatternVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(binding: BindingPattern) -> Result

  mutating func visit(expr: ExprPattern) -> Result

  mutating func visit(name: NamePattern) -> Result

  mutating func visit(tuple: TuplePattern) -> Result

  mutating func visit(wildcard: WildcardPattern) -> Result

}
