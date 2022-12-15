/// The stable identity of an instruction in its module.
///
/// - SeeAlso: `InstructionIndex`
public struct InstructionID: Hashable {

  /// The function containing the instruction.
  public let function: Module.Functions.Index

  /// The block containing this identity.
  public let block: Function.Blocks.Address

  /// The identity of the instruction in its block.
  public let address: Block.Instructions.Address

  /// Creates an instance with the given properties.
  public init(
    _ function: Module.Functions.Index,
    _ block: Function.Blocks.Address,
    _ address: Block.Instructions.Address
  ) {
    self.function = function
    self.block = block
    self.address = address
  }

  /// Creates an identity with the given properties.
  public init(_ block: Block.ID, _ address: Block.Instructions.Address) {
    self.function = block.function
    self.block = block.address
    self.address = address
  }

}
