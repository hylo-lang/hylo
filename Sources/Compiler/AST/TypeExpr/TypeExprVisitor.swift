/// A type that implements a visitation method for each kind of type expression node.
public protocol TypeExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(async: AsyncTypeExpr) -> Result

  mutating func visit(conformanceLens: ConformanceLensTypeExpr) -> Result

  mutating func visit(existential: ExistentialTypeExpr) -> Result

  mutating func visit(indirect: IndirectTypeExpr) -> Result

  mutating func visit(lambda: LambdaTypeExpr) -> Result

  mutating func visit(name: NameTypeExpr) -> Result

  mutating func visit(param: ParamTypeExpr) -> Result

  mutating func visit(storedProjection: StoredProjectionTypeExpr) -> Result

  mutating func visit(tuple: TupleTypeExpr) -> Result

  mutating func visit(union: UnionTypeExpr) -> Result

}
