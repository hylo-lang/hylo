/// A type that implements a visitation method for each kind of statement node.
public protocol StmtVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(brace: NodeID<BraceStmt>) -> Result

  mutating func visit(break: NodeID<BreakStmt>) -> Result

  mutating func visit(continue: NodeID<ContinueStmt>) -> Result

  mutating func visit(decl: NodeID<DeclStmt>) -> Result

  mutating func visit(discard: NodeID<DiscardStmt>) -> Result

  mutating func visit(doWhile: NodeID<DoWhileStmt>) -> Result

  mutating func visit(expr: NodeID<ExprStmt>) -> Result

  mutating func visit(for: NodeID<ForStmt>) -> Result

  mutating func visit(return: NodeID<ReturnStmt>) -> Result

  mutating func visit(while: NodeID<WhileStmt>) -> Result

  mutating func visit(yield: NodeID<YieldStmt>) -> Result

}
