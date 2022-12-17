import Utils

/// A constraint `F(P1, ..., Pn) -> R` specifying that `F` is the type of a callable object that
/// has parameters `Pi` and returns `R`.
public struct FunctionCallConstraint: Constraint, Hashable {

  /// A type assumed to be callable.
  public private(set) var calleeType: AnyType

  /// The expected parameters of `callee`.
  public private(set) var parameters: [CallableTypeParameter]

  /// The expected return type of `callee`.
  public private(set) var returnType: AnyType

  public var cause: ConstraintCause

  /// Creates a constraint requiring `calleeType` to be the type of a callable object with the
  /// given parameters and return type.
  public init(
    _ calleeType: AnyType,
    takes parameters: [CallableTypeParameter],
    andReturns returnType: AnyType,
    because cause: ConstraintCause
  ) {
    self.calleeType = calleeType
    self.parameters = parameters
    self.returnType = returnType
    self.cause = cause
  }

  public mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&calleeType)
    modify(&returnType)
    for i in 0 ..< parameters.count {
      modify(&parameters[i].type)
    }
  }

  public func depends(on variable: TypeVariable) -> Bool {
    calleeType == variable
      || returnType == variable
      || parameters.contains(where: { (p) in p.type == variable })
  }

}

extension FunctionCallConstraint: CustomStringConvertible {

  public var description: String { "\(calleeType)\(list: parameters) -> \(returnType)" }

}
