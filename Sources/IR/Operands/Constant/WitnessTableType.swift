import FrontEnd
import Utils

/// The type of a witness table.
public struct WitnessTableType: TypeProtocol, CustomStringConvertible, ColoredDescribable {

  /// Creates an instance.
  public init() {}

  public var flags: ValueFlags { .init() }

  public var description: String { "WitnessTable" }

  public var coloredDescription: String { styledType("WitnessTable") }

}
