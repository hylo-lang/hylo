import Utils

/// The type of a paramter in a lambda, method, or subscript type.
public struct ParameterType: TypeProtocol, Hashable {

  /// The passing convention of the parameter.
  public var convention: ParamConvention

  /// The bare type.
  public var bareType: Type

  public let flags: TypeFlags

  public init(convention: ParamConvention, bareType: Type) {
    self.convention = convention
    self.bareType = bareType
    self.flags = bareType.flags
  }

}

extension ParameterType: CustomStringConvertible {

  public var description: String {
    switch convention {
    case .let:
      return "\(bareType)"
    case .inout, .set, .sink, .yielded:
      return "\(convention) \(bareType)"
    }
  }

}
