import FrontEnd
import Utils

/// Invokes `callee`, which is a foreign function interface, with `operands`.
public struct CallFFI: Instruction {

  /// The type of the return value.
  public let returnType: IR.`Type`

  /// The name of the foreign function.
  public let callee: String

  /// The arguments of the call.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  init(
    returnType: IR.`Type`,
    callee: String,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.callee = callee
    self.operands = arguments
    self.site = site
  }

  public var result: IR.`Type`? {
    returnType
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallFFI: CustomStringConvertible {

  public var description: String {
    let s = "call_ffi \(callee)"
    return operands.isEmpty ? s : "\(s), \(list: operands)"
  }

}
