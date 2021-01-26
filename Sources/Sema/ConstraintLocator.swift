import AST

/// An object that denotes the location of a constraint.
///
/// Each
public struct ConstraintLocator {

  public init<P>(_ anchor: Node, path: P) where P: Sequence, P.Element == PathComponent {
    self.anchor = anchor
    self.path = Array(path)
  }

  public init(_ anchor: Node, _ path: PathComponent...) {
    self.anchor = anchor
    self.path = path
  }

  /// The node at which the constraint is anchored.
  public let anchor: Node

  /// A path from `anchor` to the constraint origin.
  public let path: [PathComponent]

  /// Resolves the node to which the locator resolve.
  public func resolve() -> Node {
    var base = anchor
    for component in path {
      guard let node = component.resolve(from: base) else { break }
      base = node
    }
    return base
  }

  /// Returns a new locator prefixed by this one and suffixed by the specified path component.
  public func appending(_ component: PathComponent) -> ConstraintLocator {
    return ConstraintLocator(anchor, path: path + [component])
  }

  /// A derivation step in a constraint locator.
  public enum PathComponent {

    /// The type annotation of a value declaration.
    case annotation

    /// The expression being assigned in a value binding.
    case assignment

    /// A function application.
    case application

    /// A function parameter.
    case parameter

    /// The return type of a function declaration.
    case returnType

    /// The value of a return statement.
    case returnValue

    /// The member of an value expression.
    case valueMember(String)

    /// The i-th element of a tuple type.
    case typeTupleElem(Int)

  }

}

extension ConstraintLocator.PathComponent {

  fileprivate func resolve(from base: Node) -> Node? {
    switch self {
    case .annotation:
      switch base {
      case let node as PatternBindingDecl : return node.typeSign
      default: return nil
      }

    case .assignment:
      switch base {
      case let node as PatternBindingDecl : return node.initializer
      case let node as AssignExpr         : return node.rvalue
      default: return nil
      }

    case .returnValue:
      switch base {
      case let node as RetStmt            : return node.value
      default: return nil
      }

    default: return nil
    }
  }

}
