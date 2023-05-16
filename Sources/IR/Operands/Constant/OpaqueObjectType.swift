import Core

/// The type of an opaque object.
public struct OpaqueObjectType: TypeProtocol, CustomStringConvertible {

  /// Creates an instance.
  public init() {}

  public var flags: TypeFlags { .isCanonical }

  public var description: String { "Object" }

}
