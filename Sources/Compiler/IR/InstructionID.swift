/// A stable identity denoting an instruction in a module.
///
/// - SeeAlso: `InstructionIndex`
public struct InstructionID: Hashable {

  /// The index of the function in which the instruction resides.
  public let function: Module.Functions.Index

  /// The address of the block in which the instruction resides.
  public let block: Function.Blocks.Address

  /// The address of the instruction.
  public let address: Block.Instructions.Address

  /// Creates a path with the given properties.
  public init(
    function: Module.Functions.Index,
    block: Function.Blocks.Address,
    address: Block.Instructions.Address
  ) {
    self.function = function
    self.block = block
    self.address = address
  }

  /// Creates an index with the given properties.
  public init(block: Block.ID, address: Block.Instructions.Address) {
    self.function = block.function
    self.block = block.address
    self.address = address
  }

}
