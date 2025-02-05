import FrontEnd
import Utils

/// A basic block in a Hylo IR function.
///
/// A basic blocks is sequence of instructions free of conditional control flow. It may also accept
/// arguments representing values that are notionally defined before its first instruction.
public struct Block: Sendable {

  /// A collection of instructions with stable identities.
  public typealias Instructions = DoublyLinkedList<Instruction>

  /// The innermost lexical scope corresponding to the block's instructions.
  public let scope: AnyScopeID

  /// The type input parameters of the block.
  public let inputs: [IR.`Type`]

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
