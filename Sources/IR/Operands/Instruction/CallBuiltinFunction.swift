import FrontEnd

/// Invokes the built-in function `callee`, passing `operands` as the argument list.
public struct CallBuiltinFunction: Instruction {

  /// The function to be invoked.
  public let callee: BuiltinFunction

  /// The arguments to the invocation, in order.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// An instance with the given properties.
  init(applying s: BuiltinFunction, to operands: [Operand], site: SourceRange) {
    self.callee = s
    self.operands = operands
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(callee.output)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallBuiltinFunction: CustomStringConvertible {

  public var description: String {
    operands.isEmpty ? "\(callee)" : "\(callee) \(list: operands)"
  }

}
