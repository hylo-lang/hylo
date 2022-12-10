import Utils

/// The type of a paramter in a lambda, method, or subscript type.
public struct ParameterType: TypeProtocol {

  /// The passing convention of the parameter.
  public let convention: PassingConvention

  /// The bare type.
  public let bareType: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(convention: PassingConvention, bareType: AnyType) {
    self.convention = convention
    self.bareType = bareType
    self.flags = bareType.flags
  }

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

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    ParameterType(convention: convention, bareType: bareType.transform(transformer))
  }

}

extension ParameterType: CustomStringConvertible {

  public var description: String { "\(convention) \(bareType)" }

}
