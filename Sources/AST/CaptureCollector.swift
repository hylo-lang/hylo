import Basic

/// A node visitor that gathers the values that are captured by an expression.
public final class CaptureCollector: NodeWalker {

  /// The declaration space in which the visited expression is being analyzed.
  private var space: DeclSpace?

  /// The set of declaration captured by the visited expression.
  private var captureSet: Set<HashableBox<DeclRefExpr, CaptureHashWitness>>

  /// The declaration references of the caputed values, sorted.
  public var captures: [HashableBox<DeclRefExpr, CaptureHashWitness>] {
    return captureSet
      .sorted(by: { a, b in a.value.decl.name < b.value.decl.name })
  }

  public init(relativeTo space: DeclSpace?) {
    self.space = space
    self.captureSet = []
  }

  public override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    guard let decl = decl as? BaseFunDecl else { return (true, decl) }

    let currentSpace = space
    space = decl
    _ = decl.accept(self)
    space = currentSpace
    return (false, decl)
  }

  public override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    // Check whether the expression is a declaration reference.
    if let expr = expr as? DeclRefExpr {
      // If the referre declaration is a function, make sure it is a identifying a local closure.
      if let decl = expr.decl as? BaseFunDecl {
        switch decl.parentDeclSpace {
        case is TypeDecl, is TypeExtnDecl, is SourceUnit:
          return (true, expr)
        default:
          break
        }
      }

      // Check whether the symbol is defined outside of the expression's declaration space.
      let declSpace = expr.decl.parentDeclSpace!
      if let space = self.space, declSpace.isDescendant(of: space) {
        return (true, expr)
      }

      // Register a new capture.
      captureSet.insert(HashableBox(expr))
    }

    return (true, expr)
  }

  public struct CaptureHashWitness: HashWitness {

    public typealias Value = DeclRefExpr

    public static func hash(_ value: DeclRefExpr, into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(value.decl))
    }

    public static func equals(_ lhs: DeclRefExpr, _ rhs: DeclRefExpr) -> Bool {
      return lhs.decl === rhs.decl
    }

  }

}
