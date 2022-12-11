/// An index denoting an instruction in a module.
///
/// An index identifies a function, a basic block in this function, and a position in this block,
/// including the "past-the-end" position that isn't valid for use as a subscript argument. Unlike
/// an identity, an index is not *stable*: inserting or removing instructions from the containing
/// block may invalidate existing indices.
///
/// - SeeAlso: `InstructionIdentity`
public struct InstructionIndex: Hashable {

  /// The index of the function in which the instruction resides.
  public let function: Module.Functions.Index

  /// The address of the block in which the instruction resides.
  public let block: Function.Blocks.Address

  /// The index of the instruction.
  public let index: Block.Instructions.Index

  /// Creates a path with the given properties.
  public init(
    function: Module.Functions.Index,
    block: Function.Blocks.Address,
    index: Block.Instructions.Index
  ) {
    self.function = function
    self.block = block
    self.index = index
  }

}
