import FrontEnd

/// An instruction whose semantics is defined by LLVM.
public struct LLVMInstruction: Instruction {

  /// The LLVM instruction corresponding to this instance.
  public let instruction: BuiltinFunction

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(applying s: BuiltinFunction, to operands: [Operand], site: SourceRange) {
    self.instruction = s
    self.operands = operands
    self.site = site
  }

  public var result: IR.`Type`? {
    .object(instruction.output)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension LLVMInstruction: CustomStringConvertible {

  public var description: String {
    operands.isEmpty ? "\(instruction)" : "\(instruction) \(list: operands)"
  }

}

extension Module {

  /// Creates a llvm instruction anchored at `site` that applies `f` to `operands`.
  ///
  /// - Parameters:
  ///   - f: A built-in function.
  ///   - operands: A collection of built-in objects.
  func makeLLVM(
    applying s: BuiltinFunction, to operands: [Operand], at site: SourceRange
  ) -> LLVMInstruction {
    precondition(
      operands.allSatisfy { (o) in
        let t = type(of: o)
        return t.isObject && (t.ast.base is BuiltinType)
      })

    return .init(applying: s, to: operands, site: site)
  }

}
