/// The type of a stored projection.
public struct ProjectionType: TypeProtocol, Hashable {

  /// The capability of a projection.
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

extension ProjectionType: CustomStringConvertible {

  public var description: String {
    return "\(capability) \(base)"
  }


}
