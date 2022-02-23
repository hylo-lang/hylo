/// An object that denotes the location of a constraint.
public struct ConstraintLocator {

  public init<P>(anchor: Node, path: P) where P: Sequence, P.Element == ConstraintPathComponent {
    self.anchor = anchor
    self.path = Array(path)
  }

  public init(_ anchor: Node, _ path: ConstraintPathComponent...) {
    self.anchor = anchor
    self.path = path
  }

  /// The node at which the constraint is anchored.
  public let anchor: Node

  /// A path from `anchor` to the constraint origin.
  public let path: [ConstraintPathComponent]

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
  public func appending(_ component: ConstraintPathComponent) -> ConstraintLocator {
    return ConstraintLocator(anchor: anchor, path: path + [component])
  }

}

extension ConstraintLocator: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(anchor))
    hasher.combine(path)
  }

  public static func == (lhs: ConstraintLocator, rhs: ConstraintLocator) -> Bool {
    return (lhs.anchor === rhs.anchor) && (lhs.path == rhs.path)
  }

}

/// A derivation step in a constraint locator.
public enum ConstraintPathComponent: Hashable {

  /// The type annotation of a value declaration.
  case annotation

  /// A function application.
  case application

  /// The i-th argument of a function call.
  case argument(Int)

  /// The expression being assigned in a value binding.
  case assignment

  /// The i-th parameter of a function type.
  case parameter(Int)

  /// The return type of a function declaration.
  case returnType

  /// The value of a return statement.
  case returnValue

  /// The i-th element of tuple expression.
  case tupleElem(Int)

  /// The i-th element of a tuple type.
  case typeTupleElem(Int)

  /// The member of a value expression.
  case valueMember(String)

  fileprivate func resolve(from base: Node) -> Node? {
    switch self {
    case .annotation:
      switch base {
      case let node as PatternBindingDecl:
        return node.sign
      default:
        return nil
      }

    case .assignment:
      switch base {
      case let node as PatternBindingDecl:
        return node.initializer
      case let node as AssignExpr:
        return node.rvalue
      default:
        return nil
      }

    case .returnValue:
      switch base {
      case let node as RetStmt:
        return node.value
      default:
        return nil
      }

    case .argument(let i):
      switch base {
      case let node as CallExpr where i < node.args.count:
        return node.args[i].value
      default:
        return nil
      }

    default:
      return nil
    }
  }

}
