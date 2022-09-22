/// The type of a remote part.
public struct RemoteType: TypeProtocol, Hashable {

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
  public let base: Type

  public let flags: TypeFlags

  public init(_ capability: Capability, _ base: Type) {
    self.capability = capability
    self.base = base
    self.flags = base.flags.inserting(.hasProjections)
  }

}

extension RemoteType: CustomStringConvertible {

  public var description: String {
    return "remote \(capability) \(base)"
  }


}
