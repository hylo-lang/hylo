/// The type of a remote part.
public struct RemoteType: TypeProtocol {

  /// The capabilities of the projection.
  public let access: AccessEffect

  /// The type of the projected object.
  public let bareType: AnyType

  public let flags: ValueFlags

  /// Creates an instance with the given properties.
  public init(_ access: AccessEffect, _ bareType: AnyType) {
    self.access = access
    self.bareType = bareType
    self.flags = bareType.flags
  }

  /// Creates an instance converting `t`.
  public init(_ t: ParameterType) {
    self.init(t.access, t.bareType)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    RemoteType(access, bareType.transform(mutating: &m, transformer))
  }
}

extension RemoteType: CustomStringConvertible {

  public var description: String {
    "remote \(access) \(bareType)"
  }

}
