/// A type that implements a visitation method for each kind of expression node.
public protocol ExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(assign: NodeID<AssignExpr>) -> Result

  mutating func visit(async: NodeID<AsyncExpr>) -> Result

  mutating func visit(await: NodeID<AwaitExpr>) -> Result

  mutating func visit(booleanLiteral: NodeID<BooleanLiteralExpr>) -> Result

  mutating func visit(bufferLiteral: NodeID<BufferLiteralExpr>) -> Result

  mutating func visit(charLiteral: NodeID<CharLiteralExpr>) -> Result

  mutating func visit(cast: NodeID<CastExpr>) -> Result

  mutating func visit(cond: NodeID<CondExpr>) -> Result

  mutating func visit(error: NodeID<ErrorExpr>) -> Result

  mutating func visit(floatLiteral: NodeID<FloatLiteralExpr>) -> Result

  mutating func visit(funCall: NodeID<FunCallExpr>) -> Result

  mutating func visit(`inout`: NodeID<InoutExpr>) -> Result

  mutating func visit(integerLiteral: NodeID<IntegerLiteralExpr>) -> Result

  mutating func visit(lambda: NodeID<LambdaExpr>) -> Result

  mutating func visit(mapLiteral: NodeID<MapLiteralExpr>) -> Result

  mutating func visit(match: NodeID<MatchExpr>) -> Result

  mutating func visit(matchCase: NodeID<MatchCaseExpr>) -> Result

  mutating func visit(name: NodeID<NameExpr>) -> Result

  mutating func visit(nil: NodeID<NilExpr>) -> Result

  mutating func visit(sequence: NodeID<SequenceExpr>) -> Result

  mutating func visit(storedProjection: NodeID<StoredProjectionExpr>) -> Result

  mutating func visit(stringLiteral: NodeID<StringLiteralExpr>) -> Result

  mutating func visit(subscriptCall: NodeID<SubscriptCallExpr>) -> Result

  mutating func visit(tuple: NodeID<TupleExpr>) -> Result

  mutating func visit(tupleMember: NodeID<TupleMemberExpr>) -> Result

}
