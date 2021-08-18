import Basic
import OrderedCollections

/// A node visitor that gathers the values that are captured by an expression.
public struct CaptureCollector: NodeWalker {

  public typealias Result = Bool

  /// A capture.
  public struct Capture: Hashable {

    /// A reference to the declaration being captured.
    public let expr: DeclRefExpr

    public func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(expr.decl))
    }

    public static func == (_ lhs: Capture, _ rhs: Capture) -> Bool {
      return lhs.expr.decl === rhs.expr.decl
    }

  }

  public var parent: Node?

  public var innermostSpace: DeclSpace?

  /// The outermost declaration space in which declarations are considered local, not captured.
  private var boundary: DeclSpace?

  /// The set of declaration references captured by the visited expression.
  ///
  /// This set only contains the first occurence of each reference to a captured declaration.
  public private(set) var captures: OrderedSet<Capture>

  public init(relativeTo boundary: DeclSpace?) {
    self.boundary = boundary
    self.captures = []
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
    captures.append(Capture(expr: expr))
    return true
  }

}
