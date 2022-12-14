/// A position in a module's collection of instructions.
///
/// An index identifies a function, a basic block in this function, and a position in this block,
/// including the "past-the-end" position that isn't valid for use as a subscript argument. Unlike
/// an identity, an index is not *stable*: inserting or removing instructions from the containing
/// block may invalidate existing indices.
///
/// - SeeAlso: `InstructionIdentity`
public struct InstructionIndex: Hashable {

  /// The function containing this index.
  public let function: Module.Functions.Index

  /// The block containing this index.
  public let block: Function.Blocks.Address

  /// The position of the instruction in its block.
  public let index: Block.Instructions.Index

  /// Creates an index with the given properties.
  public init(
    _ function: Module.Functions.Index,
    _ block: Function.Blocks.Address,
    _ index: Block.Instructions.Index
  ) {
    self.function = function
    self.block = block
    self.index = index
  }

  /// Creates an index with the given properties.
  public init(_ block: Block.ID, _ index: Block.Instructions.Index) {
    self.function = block.function
    self.block = block.address
    self.index = index
  }

}
