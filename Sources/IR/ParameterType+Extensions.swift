import Core

extension ParameterType {
  /// Returns `self` as an input to an IR function.
  func asIRFunctionInput() -> Function.Input {
    switch access {
    case .let, .inout, .set:
      return (convention: access, type: .address(bareType))
    case .sink:
      return (convention: access, type: .object(bareType))
    case .yielded:
      preconditionFailure("cannot lower yielded parameter")
    }
  }
}
