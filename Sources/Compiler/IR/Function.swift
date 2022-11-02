import Utils

/// A function lowered to Val IR.
public struct Function {

  /// The ID of a Val IR function.
  public typealias ID = Module.FunctionIndex

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

  /// The control flow graph of the function.
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

extension Function: Sequence {

  public typealias BlockAddress = DoublyLinkedList<Block>.Address

  public typealias Element = (block: BlockAddress, inst: Block.InstAddress)

  /// An type supplying the addresses of the instruction in a function, in no particular order.
  public struct Iterator: IteratorProtocol {

    private let base: Function

    private var outer: BlockAddress?

    private var inner: Block.InstAddress?

    fileprivate init(_ base: Function) {
      self.base = base
      self.outer = base.blocks.firstAddress
      self.inner = base.blocks.first?.instructions.firstAddress
    }

    public mutating func next() -> Element? {
      guard let b = outer, let i = inner else { return nil }
      advance(b, i)
      return (block: b, inst: i)
    }

    private mutating func advance(_ b: BlockAddress, _ i: Block.InstAddress) {
      if let ni = base[b].instructions.address(after: i) {
        inner = ni
      } else {
        var cb = b
        while let nb = base.blocks.address(after: cb) {
          if let ni = base[nb].instructions.firstAddress {
            inner = ni
            outer = nb
            return
          }
          cb = nb
        }
        outer = nil
        inner = nil
      }
    }

  }

  public func makeIterator() -> Iterator {
    Iterator(self)
  }

  public subscript(_ addresses: Element) -> Inst {
    get { blocks[addresses.block][addresses.inst] }
    set { blocks[addresses.block][addresses.inst] = newValue }
  }

  public subscript(_ address: BlockAddress) -> Block {
    get { blocks[address] }
    _modify { yield &blocks[address] }
  }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}
