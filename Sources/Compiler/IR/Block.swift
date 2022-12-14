import Utils

/// A basic block in a Val IR function.
///
/// A basic blocks is sequence of instructions free of conditional control flow. It may also accept
/// arguments representing values that are notionally defined before its first instruction.
public struct Block {

  /// The ID of a basic block.
  public struct ID: Hashable {

    /// The ID of the function containing the block.
    public var function: Module.Functions.Index

    /// The address of the block in the containing function.
    public var address: Function.BlockAddress

    /// The ID of the instruction at `instAddress` in the block identified by `self`.
    public func id(at instAddress: Block.InstructionAddress) -> InstructionID {
      InstructionID(function: function, block: address, address: instAddress)
    }

    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(block: self, index: index)
    }

    /// The operand denoting the `index`-th result of the instruction at `instAddress` in the block
    /// identified by `self`.
    public func result(at instAddress: Block.InstructionAddress, index: Int) -> Operand {
      .result(inst: id(at: instAddress), index: index)
    }

  }

  /// The address of an instruction in `self`.
  public typealias InstructionAddress = DoublyLinkedList<Instruction>.Address

  /// The type input parameters of the block.
  public let inputs: [LoweredType]

  /// The instructions in the block.
  public internal(set) var instructions: DoublyLinkedList<Instruction> = []

  /// Accesses the instruction at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: InstructionAddress) -> Instruction {
    get { instructions[address] }
    set { instructions[address] = newValue }
  }

}
