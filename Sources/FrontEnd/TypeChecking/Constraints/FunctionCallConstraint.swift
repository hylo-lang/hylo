import Core
import Utils

/// A constraint `F(A1, ..., An) -> R` specifying that `F` is the type of a callable object that
/// returns instances of `R` when called with arguments of types `A1, ..., An`.
struct FunctionCallConstraint: Constraint, Hashable {

  /// The label, type, and site of an argument passed to a callable object.
  struct Argument: Hashable {

    /// The label of the argument.
    let label: SourceRepresentable<String>?

    /// The type of the argument.
    fileprivate(set) var type: AnyType

    /// The site from which the argument's value was parsed.
    let site: SourceRange

  }

  /// A type assumed to be callable.
  private(set) var callee: AnyType

  /// The arguments passed to `callee`.
  private(set) var arguments: [Argument]

  /// The expected return type of `callee`.
  private(set) var returnType: AnyType

  let origin: ConstraintOrigin

  /// Creates a constraint requiring `callee` to be the type of a callable object that accepts
  /// given `arguments` and returns `returnType`.
  init(
    _ callee: AnyType,
    accepts arguments: [Argument],
    returns returnType: AnyType,
    origin: ConstraintOrigin
  ) {
    self.callee = callee
    self.arguments = arguments
    self.returnType = returnType
    self.origin = origin
  }

  /// The expected labels of `callee`.
  var labels: LazyMapSequence<[Argument], String?> {
    arguments.lazy.map(\.label?.value)
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&callee, with: transform)
    update(&returnType, with: transform)
    for i in 0 ..< arguments.count {
      update(&arguments[i].type, with: transform)
    }
  }

}

extension FunctionCallConstraint: CustomStringConvertible {

  var description: String {
    "(\(callee))(\(list: arguments)) -> \(returnType)"
  }

}

extension FunctionCallConstraint.Argument: CustomStringConvertible {

  var description: String {
    if let l = label {
      return "\(l.value): \(type)"
    } else {
      return "\(type)"
    }
  }

}
