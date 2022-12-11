/// The type of a remote part.
public struct RemoteType: TypeProtocol {

  /// The capability of a remote part.
  public enum Capability: Hashable {

    case `let`

    case `inout`

    case `set`

    case yielded

  }

  /// The capabilities of the projection.
  public let capability: Capability

  /// The type of the projected object.
  public let base: AnyType

  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(_ capability: Capability, _ base: AnyType) {
    self.capability = capability
    self.base = base
    self.flags = base.flags.inserting(.hasProjections)
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    RemoteType(capability, base.transform(transformer))
  }

}

extension RemoteType: CustomStringConvertible {

  public var description: String {
    return "remote \(capability) \(base)"
  }

}
