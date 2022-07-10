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

  /// The control flow graph of the function.
  var cfg: ControlFlowGraph {
    var result = ControlFlowGraph()

    for source in blocks.indices {
      switch blocks[source].instructions.last {
      case let inst as BranchInst:
        result.define(source, predecessorOf: inst.target.index)
      case let inst as CondBranchInst:
        result.define(source, predecessorOf: inst.targetIfTrue.index)
        result.define(source, predecessorOf: inst.targetIfFalse.index)
      default:
        break
      }
    }

    return result
  }

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
