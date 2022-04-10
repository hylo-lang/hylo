/// A type that implements a visitation method for each kind of type expression node.
public protocol TypeExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(async: NodeIndex<AsyncTypeExpr>) -> Result

  mutating func visit(conformanceLens: NodeIndex<ConformanceLensTypeExpr>) -> Result

  mutating func visit(existential: NodeIndex<ExistentialTypeExpr>) -> Result

  mutating func visit(indirect: NodeIndex<IndirectTypeExpr>) -> Result

  mutating func visit(lambda: NodeIndex<LambdaTypeExpr>) -> Result

  mutating func visit(name: NodeIndex<NameTypeExpr>) -> Result

  mutating func visit(param: NodeIndex<ParamTypeExpr>) -> Result

  mutating func visit(storedProjection: NodeIndex<StoredProjectionTypeExpr>) -> Result

  mutating func visit(tuple: NodeIndex<TupleTypeExpr>) -> Result

  mutating func visit(union: NodeIndex<UnionTypeExpr>) -> Result


}
