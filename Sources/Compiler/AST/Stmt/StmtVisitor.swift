/// A type that implements a visitation method for each kind of statement node.
public protocol StmtVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(brace: NodeIndex<BraceStmt>) -> Result

  mutating func visit(break: NodeIndex<BreakStmt>) -> Result

  mutating func visit(continue: NodeIndex<ContinueStmt>) -> Result

  mutating func visit(decl: NodeIndex<DeclStmt>) -> Result

  mutating func visit(doWhile: NodeIndex<DoWhileStmt>) -> Result

  mutating func visit(expr: NodeIndex<ExprStmt>) -> Result

  mutating func visit(for: NodeIndex<ForStmt>) -> Result

  mutating func visit(return: NodeIndex<ReturnStmt>) -> Result

  mutating func visit(while: NodeIndex<WhileStmt>) -> Result

  mutating func visit(yield: NodeIndex<YieldStmt>) -> Result

}
