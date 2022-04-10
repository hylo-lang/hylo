/// A type that implements a visitation method for each kind of expression node.
public protocol ExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(async: NodeIndex<AsyncExpr>) -> Result

  mutating func visit(await: NodeIndex<AwaitExpr>) -> Result

  mutating func visit(boolLiteral: NodeIndex<BoolLiteralExpr>) -> Result

  mutating func visit(bufferLiteral: NodeIndex<BufferLiteralExpr>) -> Result

  mutating func visit(charLiteral: NodeIndex<CharLiteralExpr>) -> Result

  mutating func visit(cond: NodeIndex<CondExpr>) -> Result

  mutating func visit(floatLiteral: NodeIndex<FloatLiteralExpr>) -> Result

  mutating func visit(funCall: NodeIndex<FunCallExpr>) -> Result

  mutating func visit(intLiteral: NodeIndex<IntLiteralExpr>) -> Result

  mutating func visit(lambda: NodeIndex<LambdaExpr>) -> Result

  mutating func visit(mapLiteral: NodeIndex<MapLiteralExpr>) -> Result

  mutating func visit(match: NodeIndex<MatchExpr>) -> Result

  mutating func visit(matchCase: NodeIndex<MatchCaseExpr>) -> Result

  mutating func visit(name: NodeIndex<NameExpr>) -> Result

  mutating func visit(nil: NodeIndex<NilExpr>) -> Result

  mutating func visit(storedProjection: NodeIndex<StoredProjectionExpr>) -> Result

  mutating func visit(stringLiteral: NodeIndex<StringLiteralExpr>) -> Result

  mutating func visit(subscriptCall: NodeIndex<SubscriptCallExpr>) -> Result

  mutating func visit(tuple: NodeIndex<TupleExpr>) -> Result

  mutating func visit(unfolded: NodeIndex<UnfoldedExpr>) -> Result

}
