/// A pattern visitor that collects the named patterns contained in a pattern.
struct NamePatternCollector: PatternVisitor {

  typealias Result = Void

  /// The AST to which the visited pattern belongs.
  let ast: AST

  /// The name patterns that have been collected.
  private(set) var namePatterns: [NodeID<NamePattern>] = []

  mutating func visit(binding id: NodeID<BindingPattern>) {
    ast[id].subpattern.accept(&self)
  }

  func visit(expr: NodeID<ExprPattern>) -> Void {}

  mutating func visit(name id: NodeID<NamePattern>) -> Void {
    namePatterns.append(id)
  }

  mutating func visit(tuple id: NodeID<TuplePattern>) -> Void {
    for element in ast[id].elements {
      element.value.pattern.accept(&self)
    }
  }

  func visit(wildcard: NodeID<WildcardPattern>) -> Void {}

}

extension PatternID {

  /// Returns the IDs of the named patterns contained within in the pattern denoted by this ID.
  public func names(ast: AST) -> [NodeID<NamePattern>] {
    var collector = NamePatternCollector(ast: ast)
    accept(&collector)
    return collector.namePatterns
  }

}
