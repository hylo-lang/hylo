import Utils

extension Block {

  /// The stable ID of a basic block in its function.
  public struct ID: Hashable, Sendable {

    /// The address of the block.
    public var address: Function.Blocks.Address

    /// Creates an instance with the given address.
    public init(_ address: Function.Blocks.Address) {
      self.address = address
    }


    /// The ID of the `index`-th parameter of the block.
    public func parameter(_ index: Int) -> Operand {
      .parameter(self, index)
    }

  }

}

extension Block.ID: CustomStringConvertible {

  public var description: String { "b\(address)" }

}
