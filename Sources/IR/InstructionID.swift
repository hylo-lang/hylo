import Utils

/// The stable identity of an instruction in its function.
public struct InstructionID: Hashable {

  /// The block containing the instruction.
  public let block: Function.Blocks.Address

  /// The identity of the instruction in its block.
  public let address: Block.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ block: Function.Blocks.Address,
    _ address: Block.Instructions.Address
  ) {
    self.block = block
    self.address = address
  }

  /// Creates an instance with the given properties.
  public init(_ block: Block.ID, _ address: Block.Instructions.Address) {
    self.block = block.address
    self.address = address
  }

}

extension InstructionID: CustomStringConvertible {

  public var description: String { "i\(block).\(address)" }

}
