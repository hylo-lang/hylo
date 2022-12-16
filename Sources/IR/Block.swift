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
    public var address: Function.Blocks.Address

    /// The ID of the instruction at `instAddress` in the block identified by `self`.
    public func id(at instAddress: Block.Instructions.Address) -> InstructionID {
      InstructionID(function, address, instAddress)
    }

    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(block: self, index: index)
    }

    /// The operand denoting the `index`-th result of the instruction at `instAddress` in the block
    /// identified by `self`.
    public func result(at instAddress: Block.Instructions.Address, index: Int) -> Operand {
      .result(instruction: id(at: instAddress), index: index)
    }

  }

  /// A collection of instructions with stable identities.
  public typealias Instructions = DoublyLinkedList<Instruction>

  /// The type input parameters of the block.
  public let inputs: [LoweredType]

  /// The instructions in the block.
  public internal(set) var instructions: Instructions = []

  /// Accesses the instruction at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: Instructions.Address) -> Instruction {
    get { instructions[address] }
    set { instructions[address] = newValue }
  }

}
