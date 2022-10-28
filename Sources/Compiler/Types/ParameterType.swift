import Utils

/// The type of a paramter in a lambda, method, or subscript type.
public struct ParameterType: TypeProtocol, Hashable {

  /// The passing convention of the parameter.
  public var convention: PassingConvention

  /// The bare type.
  public var bareType: Type

  public let flags: TypeFlags

  public init(convention: PassingConvention, bareType: Type) {
    self.convention = convention
    self.bareType = bareType
    self.flags = bareType.flags
  }

  /// Creates an instance identifying `x`, failing if `x` is not a parameter type.
  public init?(_ x: Type) {
    if case .parameter(let t) = x {
      self = t
    } else {
      return nil
    }
  }

}

extension ParameterType: CustomStringConvertible {

  public var description: String { "\(convention) \(bareType)" }

}
