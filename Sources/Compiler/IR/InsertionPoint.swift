/// A value denoting the position at which an instruction should be inserted.
public struct InsertionPoint {

  /// A position in a basic block.
  public enum Position {

    /// After the instruction at the specified index.
    case after(Block.InstIndex)

    /// At the end of the block.
    case end

  }

  /// The ID of a basic block.
  public var block: BlockID

  /// A position in the basic block denoted by `block`.
  public var position: Position

  /// Creates an insertion point positioned at the end of `block`.
  public init(endOf block: BlockID) {
    self.block = block
    self.position = .end
  }

  /// Creates an insertion point position right after `inst` in `block`.
  public init(after inst: Block.InstIndex, in block: BlockID) {
    self.block = block
    self.position = .after(inst)
  }

}
