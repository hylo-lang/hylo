/// The position of an instruction in, or a potential instruction insertion into, a module.
///
/// An index identifies a function, a basic block in the function, and a position in the block,
/// including the "past-the-end" position that isn't valid for use as a subscript argument. Unlike
import Utils

/// Unlike an identity, an index is not *stable*: inserting or removing instructions from the containing
/// block may invalidate existing indices.
///
/// - SeeAlso: `AbsoluteInstructionID`
public struct InstructionIndex: Hashable {

  /// The function containing the instruction.
  public let function: Function.ID

  /// The block containing the instruction.
  public let block: Function.Blocks.Address

  /// The position of the instruction in its block.
  public let index: Block.Instructions.Index

  /// Creates an index with the given properties.
  public init(
    _ function: Function.ID,
    _ block: Function.Blocks.Address,
    _ index: Block.Instructions.Index
  ) {
    self.function = function
    self.block = block
    self.index = index
  }

  /// Creates an index with the given properties.
  public init(_ block: Block.AbsoluteID, _ index: Block.Instructions.Index) {
    self.function = block.function
    self.block = block.address
    self.index = index
  }

}
