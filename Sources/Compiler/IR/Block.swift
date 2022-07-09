import Utils

/// A basic block in a Val IR function.
public struct Block {

  /// The ID of a basic block in a Val IR function.
  public struct ID: Hashable {

    /// The ID of the function containing the block.
    public var function: Module.FunctionIndex

    /// The index of the block in the containing function.
    public var index: Function.BlockIndex

  }

  /// The type input parameters of the block.
  public var inputs: [LoweredType] = []

  /// The instructions in the block.
  public var instructions: StableArray<Inst> = []

}

extension Block {

  public typealias InstIndex = StableArray<Inst>.Index

  public subscript(_ position: InstIndex) -> Inst {
    _read   { yield instructions[position] }
    _modify { yield &instructions[position] }
  }

}
