import Utils

/// A basic block in a Val IR function.
public struct Block {

  public typealias InstIndex = StableArray<Inst>.Index

  /// The type input parameters of the block.
  public var inputs: [IRType]

  /// The instructions in the block.
  public var instructions: StableArray<Inst>

}
