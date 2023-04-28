import Utils

/// A constraint `F(P1, ..., Pn) -> R` specifying that `F` is the type of a callable object that
/// has parameters `Pi` and returns `R`.
public struct FunctionCallConstraint: Constraint, Hashable {

  /// A type assumed to be callable.
  public private(set) var callee: AnyType

  /// The expected parameters of `callee`.
  public private(set) var parameters: [CallableTypeParameter]

  /// The expected return type of `callee`.
  public private(set) var returnType: AnyType

  public let origin: ConstraintOrigin

  /// Creates a constraint requiring `calleeType` to be the type of a callable object with the
  /// given parameters and return type.
  public init(
    _ calleeType: AnyType,
    takes parameters: [CallableTypeParameter],
    andReturns returnType: AnyType,
    origin: ConstraintOrigin
  ) {
    self.callee = calleeType
    self.parameters = parameters
    self.returnType = returnType
    self.origin = origin
  }

  /// The expected labels of `callee`.
  public var labels: LazyMapSequence<[CallableTypeParameter], String?> {
    parameters.lazy.map(\.label)
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&callee, with: transform)
    update(&returnType, with: transform)
    for i in 0 ..< parameters.count {
      update(&parameters[i].type, with: transform)
    }
  }

}

extension FunctionCallConstraint: CustomStringConvertible {

  public var description: String { "(\(callee))(\(list: parameters)) -> \(returnType)" }

}
