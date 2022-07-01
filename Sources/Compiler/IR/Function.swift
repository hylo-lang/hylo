import Utils

/// A function lowered to Val IR.
public struct Function {

  public typealias BlockIndex = StableArray<Block>.Index

  /// The mangled name of the function.
  public var name: String

  /// The debug name of the function, if any.
  public var debugName: String?

  /// The blocks in the function.
  ///
  /// The first block of the array is the function's entry.
  public var blocks: StableArray<Block>

  /// The entry of the function.
  public var entry: Block { blocks[blocks.startIndex] }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}
