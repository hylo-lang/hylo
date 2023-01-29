import Core

/// A Val IR instruction.
public protocol Instruction {

  /// The types of the instruction's results (empty for instructions with no result).
  var types: [LoweredType] { get }

  /// The operands of the instruction.
  var operands: [Operand] { get }

  /// The site of the code corresponding to that instruction, if any.
  var range: SourceRange? { get }

  /// Indicates whether the instruction is a terminator.
  ///
  /// A "terminator" is an instruction that indicates which block should be executed after the
  /// current block is finished, returns a value, or yields control.
  var isTerminator: Bool { get }

  /// Returns whether `self` is a well-formed instruction of `module`.
  ///
  /// Use this method as a sanity check to verify `self`'s invariants.
  func isWellFormed(in module: Module) -> Bool

}
