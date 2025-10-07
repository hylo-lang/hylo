import Utils

extension Block {

  /// The stable ID of a basic block in its function.
  public struct ID: Hashable {

    /// The address of the block.
    public var address: Function.Blocks.Address

    /// Creates an instance with the given address.
    public init(_ address: Function.Blocks.Address) {
      self.address = address
    }

    /// Creates an instance denoting the block containing `i`.
    public init(containing i: InstructionID) {
      self.address = i.block
    }

    /// The ID of the instruction at `instructionAddress` in the block identified by `self`.
    public func appending(_ instructionAddress: Block.Instructions.Address) -> InstructionID {
      InstructionID(address, instructionAddress)
    }

    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(self, index)
    }

    /// The operand denoting the result of the instruction at `instructionAddress` in the block
    /// identified by `self`.
    public func result(at instructionAddress: Block.Instructions.Address) -> Operand {
      .register(appending(instructionAddress))
    }

  }

}

extension Block.ID: CustomStringConvertible {

  public var description: String { "b\(address)" }

}
