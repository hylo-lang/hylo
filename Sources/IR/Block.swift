import FrontEnd
import Utils

/// A basic block in a Hylo IR function.
///
/// A basic blocks is sequence of instructions free of conditional control flow. It may also accept
/// arguments representing values that are notionally defined before its first instruction.
public struct Block: Sendable {

  /// The first instruction in `self`, if any.
  public fileprivate(set) var first: InstructionID?
  /// The last instruction in `self`, if any.
  public fileprivate(set) var last: InstructionID?

  /// The innermost lexical scope corresponding to the block's instructions.
  public let scope: AnyScopeID

  /// The type input parameters of the block.
  public let inputs: [IR.`Type`]

  /// Assigns the first instruction of `self`.
  internal mutating func setFirst(_ i: InstructionID?) {
    first = i
    if last == nil { last = i }
  }

  /// Assigns the last instruction of `self`.
  internal mutating func setLast(_ i: InstructionID?) {
    last = i
    if first == nil { first = i }
  }

  /// Creates a block for scope `scope` with the given `inputs` and `instructions`.
  public init(
    scope: AnyScopeID,
    inputs: [IR.`Type`]
  ) {
    self.scope = scope
    self.inputs = inputs
    self.first = nil
    self.last = nil
  }

}
