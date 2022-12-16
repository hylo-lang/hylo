import Core

extension ParameterType {
  /// Returns `self` as an input to an IR function.
  func asIRFunctionInput() -> Function.Input {
    switch convention {
    case .let, .inout, .set:
      return (convention: convention, type: .address(bareType))
    case .sink:
      return (convention: convention, type: .object(bareType))
    case .yielded:
      preconditionFailure("cannot lower yielded parameter")
    }
  }
}
