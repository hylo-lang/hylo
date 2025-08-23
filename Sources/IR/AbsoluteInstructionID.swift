import Utils

/// The stable identity of an instruction in its module.
///
/// - SeeAlso: `InstructionID`
public struct AbsoluteInstructionID: Hashable {

  /// The function containing the instruction.
  public let function: Function.ID

  /// The identity of the instruction in its block.
  public let address: Function.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ function: Function.ID,
    _ address: Function.Instructions.Address
  ) {
    self.function = function
    self.address = address
  }

  /// Creates an instance with the given properties.
  public init(_ block: Block.AbsoluteID, _ address: Function.Instructions.Address) {
    self.function = block.function
    self.address = address
  }

  /// Creates an instance with the given properties.
  public init(_ function: Function.ID, _ i: InstructionID) {
    self.function = function
    self.address = i.address
  }

}

extension AbsoluteInstructionID: CustomStringConvertible {

  public var description: String { "i\(address)" }

}
