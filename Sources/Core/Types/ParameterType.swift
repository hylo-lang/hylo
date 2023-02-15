import Utils

/// The type of a paramter in a lambda, method, or subscript type.
public struct ParameterType: TypeProtocol {

  /// The passing convention of the parameter.
  public let access: AccessEffect

  /// The type of the parameter's value.
  public let bareType: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(_ access: AccessEffect, _ bareType: AnyType) {
    self.access = access
    self.bareType = bareType
    self.flags = bareType.flags
  }

  /// Creates an instance converting `t`.
  public init(_ t: RemoteType) {
    self.init(t.access, t.bareType)
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    ParameterType(access, bareType.transform(transformer))
  }

}

extension ParameterType: CustomStringConvertible {

  public var description: String { "\(access) \(bareType)" }

}
