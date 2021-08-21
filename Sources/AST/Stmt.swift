import Basic

/// A statement.
public protocol Stmt: Node {

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A statement visitor.
  func accept<V>(_ visitor: inout V) -> V.StmtResult where V: StmtVisitor

}

/// A collection of declarations, control statements, and expressions processed sequentially.
public final class BraceStmt: Stmt, IterableDeclSpace {

  public typealias DeclSequence = LazyMapSequence<
    LazyFilterSequence<LazyMapSequence<LazySequence<[Node]>.Elements, Decl?>>, Decl>

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  /// The declarations, statements and expressions in the code block.
  public var stmts: [Node]

  public init(statements: [Node], range: SourceRange? = nil) {
    self.stmts = statements
    self.range = range
  }

  public var decls: DeclSequence {
    return stmts.lazy.compactMap({ $0 as? Decl })
  }

  public func accept<V>(_ visitor: inout V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}

/// A return statement.
public final class RetStmt: Stmt {

  public var range: SourceRange?

  /// The returned value.
  public var value: Expr?

  /// The innermost function in which the return statement resides.
  public weak var funDecl: BaseFunDecl?

  public init(value: Expr?, range: SourceRange? = nil) {
    self.value = value
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}

/// A case statement of a match construct.
public final class MatchCaseStmt: Stmt, IterableDeclSpace {

  public typealias DeclSequence = LazyMapSequence<LazySequence<[NamedPattern]>.Elements, Decl>

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  /// The pattern of the case.
  public var pattern: Pattern

  /// The optional condition of the case.
  public var condition: Expr?

  /// The body of the case.
  public var body: BraceStmt

  public init(pattern: Pattern, condition: Expr?, body: BraceStmt, range: SourceRange? = nil) {
    self.pattern = pattern
    self.condition = condition
    self.body = body
    self.range = range
  }

  /// The expression of the case, if that case only contains a single expression.
  public var singleExprBody: Expr? {
    if body.stmts.count == 1 {
      return body.stmts[0] as? Expr
    } else {
      return nil
    }
  }

  public var decls: DeclSequence {
    return pattern.namedPatterns.lazy.map({ $0.decl })
  }

  public func accept<V>(_ visitor: inout V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}
