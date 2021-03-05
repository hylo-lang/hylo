import Basic

/// A statement.
public protocol Stmt: Node {

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A statement visitor.
  func accept<V>(_ visitor: V) -> V.StmtResult where V: StmtVisitor

}

/// A block of code.
public final class BraceStmt: Stmt, IterableDeclSpace {

  public typealias DeclSequence = LazyMapSequence<
    LazyFilterSequence<LazyMapSequence<LazySequence<[Node]>.Elements, Decl?>>, Decl>

  public init(statements: [Node], range: SourceRange) {
    self.stmts = statements
    self.range = range
  }

  /// The declarations, statements and expressions in the code block.
  public var stmts: [Node]

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public var decls: DeclSequence {
    return stmts.lazy.compactMap({ $0 as? Decl })
  }

  public func accept<V>(_ visitor: V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}

/// A return statement.
public final class RetStmt: Stmt {

  public init(value: Expr?, range: SourceRange) {
    self.value = value
    self.range = range
  }

  public var value: Expr?

  /// The innermost function in which the return statement resides.
  public weak var funDecl: BaseFunDecl?

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}

/// A case statement of a match construct.
public final class MatchCaseStmt: Stmt, IterableDeclSpace {

  public typealias DeclSequence = LazyMapSequence<LazySequence<[NamedPattern]>.Elements, Decl>

  public init(pattern: Pattern, condition: Expr?, body: BraceStmt, range: SourceRange) {
    self.pattern = pattern
    self.condition = condition
    self.body = body
    self.range = range
  }

  /// The pattern of the case.
  public var pattern: Pattern

  /// The optional condition of the case.
  public var condition: Expr?

  /// The body of the case.
  public var body: BraceStmt

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public var decls: DeclSequence {
    return pattern.namedPatterns.lazy.map({ $0.decl })
  }

  public func accept<V>(_ visitor: V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}
