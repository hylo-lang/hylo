/// The ID of a VIR instruction in a basic block.
public struct InstID: Hashable {

  /// The ID of the block in which the instruction lives.
  public var block: BlockID

  /// The index of the instruction in its block.
  public var index: Block.Index

}
