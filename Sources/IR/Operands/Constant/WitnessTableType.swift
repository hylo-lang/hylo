import FrontEnd

/// The type of a witness table.
public struct WitnessTableType: TypeProtocol, CustomStringConvertible {

  /// Creates an instance.
  public init() {}

  public var flags: TypeFlags { .init() }

  public var description: String { "WitnessTable" }

}
