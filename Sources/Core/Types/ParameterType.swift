import Utils

/// The type of a paramter in a lambda, method, or subscript type.
public struct ParameterType: TypeProtocol {

  /// The passing convention of the parameter.
  public let convention: AccessEffect

  /// The bare type.
  public let bareType: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(convention: AccessEffect, bareType: AnyType) {
    self.convention = convention
    self.bareType = bareType
    self.flags = bareType.flags
  }

  /// Creates an instance converting `t`.
  public init(_ t: RemoteType) {
    self.init(convention: t.capability, bareType: t.base)
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    ParameterType(convention: convention, bareType: bareType.transform(transformer))
  }

}

extension ParameterType: CustomStringConvertible {

  public var description: String { "\(convention) \(bareType)" }

}
