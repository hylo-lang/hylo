/// The stable identity of an instruction in its module.
///
/// - SeeAlso: `InstructionIndex`
public struct InstructionID: Hashable {

  /// The function containing this identity.
  public let function: Module.Functions.Index

  /// The block containing this identity.
  public let block: Function.Blocks.Address

  /// The identity of the instruction in its block.
  public let address: Block.Instructions.Address

  /// Creates an identity with the given properties.
  public init(
    function: Module.Functions.Index,
    block: Function.Blocks.Address,
    address: Block.Instructions.Address
  ) {
    self.function = function
    self.block = block
    self.address = address
  }

  /// Creates an identity with the given properties.
  public init(block: Block.ID, address: Block.Instructions.Address) {
    self.function = block.function
    self.block = block.address
    self.address = address
  }

}
