import Basic

/// A linear sequence of instructions.
///
/// A basic block is well-formed if it contains a list of non-terminating instructions, followed by
/// a single terminator, which typically transfers control to another block. A function must always
/// contain at least one basic block (unless it refers to another module).
///
/// Note that function applications and `await` instructions are not terminators, because execution
/// resumes at the same place after they are evaluated.
public struct BasicBlock {

  public typealias ID = UInt32

  public typealias Index = StableDoublyLinkedList<Inst>.Index

  /// The formal parameters of the block.
  public let params: [ArgumentValue]

  /// The instructions in the block.
  public var instructions: StableDoublyLinkedList<Inst> = []

  init(id: ID, paramTypes: [VILType]) {
    params = paramTypes.map(ArgumentValue.init(type:))
  }

}
