import Utils

/// A basic block in a Val IR function.
///
/// A basic blocks is sequence of instructions free of conditional control flow. It may also accept
/// arguments representing values that are notionally defined before its first instruction.
public struct Block {

  /// The ID of a basic block.
  public struct ID: Hashable {

    /// The ID of the function containing the block.
    public var function: Function.ID

    /// The address of the block in the containing function.
    public var address: Function.Blocks.Address

    /// Creates an instance with the given properties.
    public init(_ function: Function.ID, _ address: Function.Blocks.Address) {
      self.function = function
      self.address = address
    }

    /// The ID of the instruction at `instructionAddress` in the block identified by `self`.
    public func appending(_ instructionAddress: Block.Instructions.Address) -> InstructionID {
      InstructionID(function, address, instructionAddress)
    }

    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(self, index)
    }

    /// The operand denoting the `index`-th result of the instruction at `instructionAddress` in
    /// the block identified by `self`.
    public func result(at instructionAddress: Block.Instructions.Address, index: Int) -> Operand {
      .register(appending(instructionAddress), index)
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

extension Block.ID: CustomStringConvertible {

  public var description: String { "b\(address)" }

}
