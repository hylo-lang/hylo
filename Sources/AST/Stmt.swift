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
    self.statements = statements
    self.range = range
  }

  /// The statements in the code block.
  public var statements: [Node]

  public var decls: DeclSequence {
    return statements.lazy.compactMap({ $0 as? Decl })
  }

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

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
  public weak var funDecl: AbstractFunDecl?

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.StmtResult where V: StmtVisitor {
    return visitor.visit(self)
  }

}
