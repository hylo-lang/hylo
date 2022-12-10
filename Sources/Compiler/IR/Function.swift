import Utils

/// A collection of basic blocks representing a lowered function.
public struct Function {

  /// The ID of a Val IR function.
  public typealias ID = Module.Functions.Index

  /// The address of a basic block in `self`.
  public typealias BlockAddress = DoublyLinkedList<Block>.Address

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
  public internal(set) var blocks: DoublyLinkedList<Block>

  /// The entry of the function.
  public var entry: Block? { blocks.first }

  /// Accesses the basic block at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: BlockAddress) -> Block {
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

extension Function {

  /// A view of a function's instruction addresses.
  public struct InstructionAddresses: Sequence {

    public typealias Element = (block: BlockAddress, inst: Block.InstAddress)

    /// A type supplying the addresses of the instruction in a function, in no particular order.
    public struct Iterator: IteratorProtocol {

      /// The function containing the instructions over which `self` iterates.
      private let base: Function

      /// The current block address, representing the outer position.
      private var outer: BlockAddress?

      /// The current instruction address in `outer`, representing the inner position.
      private var inner: Block.InstAddress?

      /// Creates an iterator supplying the addresses of the instructions in `base`.
      fileprivate init(_ base: Function) {
        self.base = base
        self.outer = base.blocks.firstAddress
        self.inner = base.blocks.first?.instructions.firstAddress
      }

      public mutating func next() -> Element? {
        // If either of the outer or inner iterator is at the end, we're done.
        guard let b = outer, let i = inner else { return nil }

        // Advance `self` to the next instruction address by pointing to the next instruction in
        // the current block, or if such instruction doesn't exist, by pointing to the first
        // instruction of the next non-empty block.
        if let ni = base[b].instructions.address(after: i) {
          inner = ni
        } else {
          var cb = b
          while let nb = base.blocks.address(after: cb) {
            if let ni = base[nb].instructions.firstAddress {
              inner = ni
              outer = nb
              return (block: b, inst: i)
            }
            cb = nb
          }

          // There are no more addresses to supply.
          outer = nil
          inner = nil
        }

        return (block: b, inst: i)
      }

    }

    /// The function containing the instructions viewed through `self`.
    fileprivate let base: Function

    public func makeIterator() -> Iterator {
      Iterator(base)
    }

  }

  /// A sequence containing the addresses of the instructions in `self`, in no particular order.
  public var instructions: InstructionAddresses { InstructionAddresses(base: self) }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}
