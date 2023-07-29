import Utils

extension Block {

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
