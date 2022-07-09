import Utils

/// A function lowered to Val IR.
public struct Function {

  /// The ID of a basic block in a Val IR function.
  public typealias ID = Module.FunctionIndex

  /// The mangled name of the function.
  public var name: String

  /// The debug name of the function, if any.
  public var debugName: String?

  /// The types of the function's parameters.
  public var inputs: [LoweredType]

  /// The type of the function's output.
  public var output: LoweredType

  /// The blocks in the function.
  ///
  /// The first block of the array is the function's entry.
  public var blocks: StableArray<Block>

  /// The entry of the function.
  public var entry: Block { blocks[blocks.startIndex] }

}

extension Function {

  public typealias BlockIndex = StableArray<Block>.Index

  public subscript(_ position: BlockIndex) -> Block {
    _read   { yield blocks[position] }
    _modify { yield &blocks[position] }
  }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}
