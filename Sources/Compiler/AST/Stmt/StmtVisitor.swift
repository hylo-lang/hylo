/// A type that implements a visitation method for each kind of statement node.
public protocol StmtVisitor {

  /// The return type of the visitation methods.
  associatedtype Result

  mutating func visit(brace: BraceStmt) -> Result

  mutating func visit(break: BreakStmt) -> Result

  mutating func visit(continue: ContinueStmt) -> Result

  mutating func visit(decl: DeclStmt) -> Result

  mutating func visit(doWhile: DoWhileStmt) -> Result

  mutating func visit(expr: ExprStmt) -> Result

  mutating func visit(for: ForStmt) -> Result

  mutating func visit(return: ReturnStmt) -> Result

  mutating func visit(while: WhileStmt) -> Result

  mutating func visit(yield: YieldStmt) -> Result

}
