import Utils

/// A constraint `F(A1, ..., An) -> R` specifying that `F` is the type of a callable object that
/// returns instances of `R` when called with arguments of types `A1, ..., An`.
public struct FunctionCallConstraint: Constraint, Hashable {

  /// The label, type, and site of an argument passed to a callable object.
  public struct Argument: Hashable {

    /// The label of the argument.
    public let label: SourceRepresentable<String>?

    /// The type of the argument.
    public fileprivate(set) var type: AnyType

    /// The site from which the argument's value was parsed.
    public let site: SourceRange

    /// Creates an instance with the given properties.
    public init(label: SourceRepresentable<String>?, type: AnyType, site: SourceRange) {
      self.label = label
      self.type = type
      self.site = site
    }

  }

  /// A type assumed to be callable.
  public private(set) var callee: AnyType

  /// The arguments passed to `callee`.
  public private(set) var arguments: [Argument]

  /// The expected return type of `callee`.
  public private(set) var returnType: AnyType

  public let origin: ConstraintOrigin

  /// Creates a constraint requiring `callee` to be the type of a callable object that accepts
  /// given `arguments` and returns `returnType`.
  public init(
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
  public var labels: LazyMapSequence<[Argument], String?> {
    arguments.lazy.map(\.label?.value)
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&callee, with: transform)
    update(&returnType, with: transform)
    for i in 0 ..< arguments.count {
      update(&arguments[i].type, with: transform)
    }
  }

}

extension FunctionCallConstraint: CustomStringConvertible {

  public var description: String { "(\(callee))(\(list: arguments)) -> \(returnType)" }

}

extension FunctionCallConstraint.Argument: CustomStringConvertible {

  public var description: String {
    if let l = label {
      return "\(l.value): \(type)"
    } else {
      return "\(type)"
    }
  }

}
