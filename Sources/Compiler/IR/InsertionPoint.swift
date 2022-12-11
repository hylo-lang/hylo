/// The position at which an instruction should be inserted.
public struct InsertionPoint {

  /// A position in a basic block.
  public enum Position {

    /// After the instruction at the specified address.
    case after(Block.InstAddress)

    /// Before the insruction at the specified address.
    case before(Block.InstAddress)

    /// At the end of the block.
    case end

  }

  /// The ID of a basic block.
  public let block: Block.ID

  /// A position in the basic block denoted by `block`.
  public let position: Position

  /// Creates an insertion point positioned at the end of `block`.
  public init(endOf block: Block.ID) {
    self.block = block
    self.position = .end
  }

  /// Creates an insertion point positioned right after `inst` in `block`.
  public init(after inst: Block.InstAddress, in block: Block.ID) {
    self.block = block
    self.position = .after(inst)
  }

  /// Creates an insertion point positioned right after `inst`.
  public init(after instID: InstID) {
    self.init(after: instID.address, in: Block.ID(function: instID.function, address: instID.block))
  }

  /// Creates an insertion point positioned right before `inst` in `block`.
  public init(before inst: Block.InstAddress, in block: Block.ID) {
    self.block = block
    self.position = .before(inst)
  }

  /// Creates an insertion point positioned right before `inst`.
  public init(before instID: InstID) {
    self.init(
      before: instID.address, in: Block.ID(function: instID.function, address: instID.block))
  }

}
