import Utils

/// A basic block in a VIR function.
public struct Block {

  /// The instructions in the block.
  public var instructions: StableArray<Inst>

}

extension Block {

  public typealias Index = StableArray<Inst>.Index

}
