import Utils

/// A collection of basic blocks representing a lowered function.
public struct Function {

  /// The ID of a Val IR function.
  public typealias ID = Module.Functions.Index

  /// A collection of blocks with stable identities.
  public typealias Blocks = DoublyLinkedList<Block>

  /// The profile of a IR function input.
  public typealias Input = (convention: PassingConvention, type: LoweredType)

  /// The mangled name of the function.
  public let name: String

  /// The debug name of the function, if any.
  public let debugName: String?

  /// The linkage of the function.
  public let linkage: Linkage

  /// The types of the function's parameters.
  public let inputs: [Input]

  /// The type of the function's output.
  public let output: LoweredType

  /// The blocks in the function.
  ///
  /// The first block of the array is the function's entry.
  public internal(set) var blocks: Blocks

  /// The entry of the function.
  public var entry: Block? { blocks.first }

  /// Accesses the basic block at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: Blocks.Address) -> Block {
    get { blocks[address] }
    _modify { yield &blocks[address] }
  }

  /// The control flow graph of `self`.
  var cfg: ControlFlowGraph {
    var result = ControlFlowGraph()

    for source in blocks.indices {
      switch blocks[source.address].instructions.last {
      case let inst as BranchInst:
        result.define(source.address, predecessorOf: inst.target.address)
      case let inst as CondBranchInst:
        result.define(source.address, predecessorOf: inst.targetIfTrue.address)
        result.define(source.address, predecessorOf: inst.targetIfFalse.address)
      default:
        break
      }
    }

    return result
  }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}
