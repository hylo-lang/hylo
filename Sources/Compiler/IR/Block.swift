import Utils

/// A basic block in a Val IR function.
public struct Block {

  /// The ID of a basic block in a Val IR function.
  public struct ID: Hashable {

    /// The ID of the function containing the block.
    public var function: Module.FunctionIndex

    /// The address of the block in the containing function.
    public var address: Function.BlockAddress

  }

  /// The type input parameters of the block.
  public var inputs: [LoweredType] = []

  /// The instructions in the block.
  public var instructions: DoublyLinkedList<Inst> = []

}

extension Block {

  public typealias InstAddress = DoublyLinkedList<Inst>.Address

  public subscript(_ address: InstAddress) -> Inst {
    get { instructions[address] }
    set { instructions[address] = newValue }
  }

}
