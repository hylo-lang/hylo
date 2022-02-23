import OrderedCollections
import Utils

/// A node visitor that gathers the values that are captured by an expression.
public struct CaptureCollector: NodeWalker {

  public enum _DeclRefHashWitness: HashWitness {

    public typealias Value = DeclRefExpr

    public static func hash(_ value: Value, into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(value.decl))
    }

    public static func equals(_ lhs: Value, _ rhs: Value) -> Bool {
      return lhs.decl === rhs.decl
    }

  }

  public typealias Result = Bool

  public var parent: Node?

  public var innermostSpace: DeclSpace?

  /// The outermost declaration space in which declarations are considered local, not captured.
  private var boundary: DeclSpace?

  /// The first representative of each captured declaration that was collected.
  public var capturedDeclRefs: OrderedSet<HashableBox<_DeclRefHashWitness>>

  public init(relativeTo boundary: DeclSpace?) {
    self.boundary = boundary
    self.capturedDeclRefs = []
  }

  public mutating func visit(_ decl: BaseFunDecl) -> Bool {
    let currentBoundary = boundary
    boundary = decl
    _ = traverse(decl)
    boundary = currentBoundary
    return true
  }

  public mutating func visit(_ expr: DeclRefExpr) -> Bool {
    // If the referred declaration is a function, make sure it is a identifying a local closure.
    if let decl = expr.decl as? BaseFunDecl {
      switch decl.parentDeclSpace {
      case is TypeDecl, is TypeExtnDecl, is SourceUnit:
        return true
      default:
        break
      }
    }

    // Check whether the symbol is defined beyond the boundary.
    let declSpace = expr.decl.parentDeclSpace!
    if let boundary = self.boundary, declSpace.isDescendant(of: boundary) {
      return true
    }

    // Register a new capture.
    capturedDeclRefs.append(HashableBox(expr))
    return true
  }

}
