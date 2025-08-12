import Utils

/// The stable identity of an instruction in its module.
///
/// - SeeAlso: `InstructionID`
public struct AbsoluteInstructionID: Hashable {

  /// The function containing the instruction.
  public let function: Function.ID

  /// The block containing the instruction.
  public let block: Function.Blocks.Address

  /// The identity of the instruction in its block.
  public let address: Block.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ function: Function.ID,
    _ block: Function.Blocks.Address,
    _ address: Block.Instructions.Address
  ) {
    self.function = function
    self.block = block
    self.address = address
  }

  /// Creates an instance with the given properties.
  public init(_ block: Block.AbsoluteID, _ address: Block.Instructions.Address) {
    self.function = block.function
    self.block = block.address
    self.address = address
  }

  /// Creates an instance with the given properties.
  public init(_ function: Function.ID, _ i: InstructionID) {
    self.function = function
    self.block = i.block
    self.address = i.address
  }

}

extension AbsoluteInstructionID: CustomStringConvertible {

  public var description: String { "i\(block).\(address)" }

}
