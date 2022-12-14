import Utils

/// A constraint `F(P1, ..., Pn) -> R` specifying that `F` is the type of a callable object that
/// has parameters `Pi` and returns `R`.
struct FunctionCallConstraint: Constraint, Hashable {

  /// A type assumed to be callable.
  private(set) var callee: AnyType

  /// The expected parameters of `callee`.
  private(set) var parameters: [CallableTypeParameter]

  /// The expected return type of `callee`.
  private(set) var returnType: AnyType

  var cause: ConstraintCause

  /// Creates a constraint specifying `callee` is the type of a callable object with the given
  /// parameters and return type.
  public init(
    _ callee: AnyType,
    hasParameters parameters: [CallableTypeParameter],
    andReturns returnType: AnyType,
    because cause: ConstraintCause
  ) {
    self.callee = callee
    self.parameters = parameters
    self.returnType = returnType
    self.cause = cause
  }

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&callee)
    modify(&returnType)
    for i in 0 ..< parameters.count {
      modify(&parameters[i].type)
    }
  }

  func depends(on variable: TypeVariable) -> Bool {
    callee == variable
      || returnType == variable
      || parameters.contains(where: { (p) in p.type == variable })
  }

}

extension FunctionCallConstraint: CustomStringConvertible {

  public var description: String { "\(callee)\(list: parameters) -> \(returnType)" }

}
