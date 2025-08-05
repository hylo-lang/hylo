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

    /// Creates an instance denoting the block containing `i`.
    public init(containing i: AbsoluteInstructionID) {
      self.address = i.block
    }

    /// The ID of the instruction at `instructionAddress` in the block identified by `self`.
    public func appending(_ instructionAddress: Block.Instructions.Address) -> InstructionID {
      InstructionID(address, instructionAddress)
    }

    // TODO (LucTeo): Revisit this.

    /// The ID of the `index`-th parameter of the block.
    // public func parameter(_ index: Int) -> Operand {
    //   .parameter(self, index)
    // }

    /// The operand denoting the result of the instruction at `instructionAddress` in the block
    /// identified by `self`.
    // public func result(at instructionAddress: Block.Instructions.Address) -> Operand {
    //   .register(appending(instructionAddress))
    // }

  }

  /// The absolute ID of a basic block.
  public struct AbsoluteID: Hashable {

    /// The ID of the function containing the block.
    public var function: Function.ID

    /// The address of the block in the containing function.
    public var address: Function.Blocks.Address

    /// Creates an instance with the given properties.
    public init(_ function: Function.ID, _ address: Function.Blocks.Address) {
      self.function = function
      self.address = address
    }

    /// Creates an instance with the given properties.
    public init(_ function: Function.ID, _ block: Block.ID) {
      self.function = function
      self.address = block.address
    }

    /// Creates an instance denoting the block containing `i`.
    public init(containing i: AbsoluteInstructionID) {
      self.function = i.function
      self.address = i.block
    }

    /// The ID of the instruction at `instructionAddress` in the block identified by `self`.
    public func appending(_ instructionAddress: Block.Instructions.Address) -> AbsoluteInstructionID {
      AbsoluteInstructionID(function, address, instructionAddress)
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

extension Block.AbsoluteID: CustomStringConvertible {

  public var description: String { "b\(address)" }

}
