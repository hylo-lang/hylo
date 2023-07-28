import Core
import Utils

/// A Val IR instruction.
public protocol Instruction: CustomStringConvertible {

  /// The type of the instruction's result, if any.
  var result: IR.`Type`? { get }

  /// The operands of the instruction.
  var operands: [Operand] { get }

  /// The site of the code corresponding to that instruction.
  var site: SourceRange { get }

  /// Replaces the operand at position `i` with `o`.
  ///
  /// Do not call this method direcly. Use `Module.replaceUses(of:with:)` instead to ensure def-use
  /// ensure def-use chains are kept updated.
  mutating func replaceOperand(at i: Int, with new: Operand)

}

extension Instruction {

  public var result: IR.`Type`? {
    nil
  }

  public var operands: [Operand] {
    []
  }

  public var description: String {
    let n = String(describing: type(of: self)).snakeCased()
    return operands.isEmpty ? String(n) : "\(n) \(list: operands)"
  }

}
