import Utils

/// The stable identity of an instruction in its function.
public struct InstructionID: Hashable, Sendable {

  /// The identity of the instruction in its block.
  public let address: Function.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ address: Function.Instructions.Address
  ) {
    self.address = address
  }

}

extension InstructionID: CustomStringConvertible {

  public var description: String { "i\(address)" }

}
