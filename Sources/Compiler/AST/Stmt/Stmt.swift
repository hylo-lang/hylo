/// A statement wrapped in a type-erased container.
public indirect enum Stmt: Hashable {

  case brace(BraceStmt)

  case `break`(BreakStmt)

  case `continue`(ContinueStmt)

  case decl(DeclStmt)

  case doWhile(DoWhileStmt)

  case expr(ExprStmt)

  case `for`(ForStmt)

  case `while`(WhileStmt)

  case `yield`(YieldStmt)

  case `return`(ReturnStmt)

  /// The value wrapped in this container.
  public var base: Any {
    switch self {
    case let .brace(s):     return s
    case let .break(s):     return s
    case let .continue(s):  return s
    case let .decl(s):      return s
    case let .doWhile(s):   return s
    case let .expr(s):      return s
    case let .for(s):       return s
    case let .while(s):     return s
    case let .yield(s):     return s
    case let .return(s):    return s
    }
  }

  /// Accepts the specified visitor.
  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    switch self {
    case let .brace(s):     return visitor.visit(brace: s)
    case let .break(s):     return visitor.visit(break: s)
    case let .continue(s):  return visitor.visit(continue: s)
    case let .decl(s):      return visitor.visit(decl: s)
    case let .doWhile(s):   return visitor.visit(doWhile: s)
    case let .expr(s):      return visitor.visit(expr: s)
    case let .for(s):       return visitor.visit(for: s)
    case let .while(s):     return visitor.visit(while: s)
    case let .yield(s):     return visitor.visit(yield: s)
    case let .return(s):    return visitor.visit(return: s)
    }
  }

}
