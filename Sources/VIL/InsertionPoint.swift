  /// An insertion point.
  public struct InsertionPoint {

    /// The function in which new instructions are being inserted.
    public let function: Function

    /// The ID of the basic block in which new instructions are being inserted.
    public var blockID: BasicBlock.ID {
      didSet {
        precondition(function.blocks[blockID] != nil, "specified block is not in the function")
      }
    }

    public init(function: Function, blockID: BasicBlock.ID) {
      precondition(function.blocks[blockID] != nil, "specified block is not in the function")
      self.function = function
      self.blockID = blockID
    }

  }
