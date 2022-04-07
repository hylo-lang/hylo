/// A type that implements a visitation method for each kind of expression node.
public protocol ExprVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(async: AsyncExpr) -> Result

  mutating func visit(await: AwaitExpr) -> Result

  mutating func visit(boolLiteral: BoolLiteralExpr) -> Result

  mutating func visit(bufferLiteral: BufferLiteralExpr) -> Result

  mutating func visit(charLiteral: CharLiteralExpr) -> Result

  mutating func visit(cond: CondExpr) -> Result

  mutating func visit(floatLiteral: FloatLiteralExpr) -> Result

  mutating func visit(funCall: FunCallExpr) -> Result

  mutating func visit(intLiteral: IntLiteralExpr) -> Result

  mutating func visit(lambda: LambdaExpr) -> Result

  mutating func visit(mapLiteral: MapLiteralExpr) -> Result

  mutating func visit(match: MatchExpr) -> Result

  mutating func visit(name: NameExpr) -> Result

  mutating func visit(nil: NilExpr) -> Result

  mutating func visit(storedProjection: StoredProjectionExpr) -> Result

  mutating func visit(stringLiteral: StringLiteralExpr) -> Result

  mutating func visit(subscriptCall: SubscriptCallExpr) -> Result

  mutating func visit(tupleExpr: TupleExpr) -> Result

  mutating func visit(unfoldedExpr: UnfoldedExpr) -> Result

}
