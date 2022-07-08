/// A Val IR instruction.
public protocol Inst {

  /// The type of the instruction.
  var type: LoweredType { get }

  /// Writes the textual representation of the instruction into `output` using `printer` to
  /// pretty-print its operands.
  func dump<Target: TextOutputStream>(into output: inout Target, with printer: inout IRPrinter)

}
