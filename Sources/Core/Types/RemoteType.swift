/// The type of a remote part.
public struct RemoteType: TypeProtocol {

  /// The capabilities of the projection.
  public let access: AccessEffect

  /// The type of the projected object.
  public let bareType: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(_ access: AccessEffect, _ bareType: AnyType) {
    self.access = access
    self.bareType = bareType
    self.flags = bareType.flags.inserting(.hasRemoteType)
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    RemoteType(access, bareType.transform(transformer))
  }

}

extension RemoteType: CustomStringConvertible {

  public var description: String {
    return "remote \(access) \(bareType)"
  }

}
