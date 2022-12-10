import Utils

/// A collection of instructions.
public struct Block {

  /// The ID of a basic block.
  public struct ID: Hashable {

    /// The ID of the function containing the block.
    public var function: Module.Functions.Index

    /// The address of the block in the containing function.
    public var address: Function.BlockAddress

    /// The ID of the instruction at `instAddress` in the block identified by `self`.
    public func id(at instAddress: Block.InstAddress) -> InstID {
      InstID(function: function, block: address, address: instAddress)
    }

    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(block: self, index: index)
    }

    /// The operand denoting the `index`-th result of the instruction at `instAddress` in the block
    /// identified by `self`.
    public func result(at instAddress: Block.InstAddress, index: Int) -> Operand {
      .result(inst: id(at: instAddress), index: index)
    }

  }

  /// The address of an instruction in `self`.
  public typealias InstAddress = DoublyLinkedList<Inst>.Address

  /// The type input parameters of the block.
  public let inputs: [LoweredType]

  /// The instructions in the block.
  public internal(set) var instructions: DoublyLinkedList<Inst> = []

  /// Accesses the instruction at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: InstAddress) -> Inst {
    get { instructions[address] }
    set { instructions[address] = newValue }
  }

}
