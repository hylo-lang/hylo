/// A type that implements a visitation method for each kind of type expression node.
public protocol TypeExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(async: NodeID<AsyncTypeExpr>) -> Result

  mutating func visit(conformanceLens: NodeID<ConformanceLensTypeExpr>) -> Result

  mutating func visit(existential: NodeID<ExistentialTypeExpr>) -> Result

  mutating func visit(indirect: NodeID<IndirectTypeExpr>) -> Result

  mutating func visit(lambda: NodeID<LambdaTypeExpr>) -> Result

  mutating func visit(name: NodeID<NameTypeExpr>) -> Result

  mutating func visit(param: NodeID<ParamTypeExpr>) -> Result

  mutating func visit(storedProjection: NodeID<StoredProjectionTypeExpr>) -> Result

  mutating func visit(tuple: NodeID<TupleTypeExpr>) -> Result

  mutating func visit(union: NodeID<UnionTypeExpr>) -> Result


}
