import AST

/// An object that denotes the location of a constraint.
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

  /// Returns a new locator prefixed by this one and suffixed by the specified path component.
  public func appending(_ component: PathComponent) -> ConstraintLocator {
    return ConstraintLocator(anchor, path: path + [component])
  }

  /// A derivation step in a constraint locator.
  public enum PathComponent {

    /// A type annotation.
    case annotation

    /// An assignment.
    case assignment

    /// A pattern or parameter initializer.
    case initializer

    /// A function application.
    case application

    /// A function parameter.
    case parameter

    /// The return type of a function.
    case returnType

    /// The value of a return statement.
    case returnValue

    /// The member of an value expression.
    case valueMember(String)

    /// The i-th element of a tuple type.
    case typeTupleElem(Int)

  }

}
