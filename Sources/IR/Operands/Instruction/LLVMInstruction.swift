import Core

/// An instruction whose semantics is defined by LLVM.
public struct LLVMInstruction: Instruction {

  /// The LLVM instruction corresponding to this instance.
  public let instruction: NativeInstruction

  /// The operands of the instruction.
  public let operands: [Operand]

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(applying s: NativeInstruction, to operands: [Operand], site: SourceRange) {
    self.instruction = s
    self.operands = operands
    self.site = site
  }

  public var types: [LoweredType] { [.object(instruction.type.output)] }

}

extension LLVMInstruction: CustomStringConvertible {

  public var description: String {
    operands.isEmpty ? "\(instruction)" : "\(instruction) \(list: operands)"
  }

}

extension Module {

  /// Creates a llvm instruction anchored at `anchor` that applies `f` to `operands`.
  ///
  /// - Parameters:
  ///   - f: A built-in function.
  ///   - operands: A collection of built-in objects.
  func makeLLVM(
    applying s: NativeInstruction,
    to operands: [Operand],
    anchoredAt anchor: SourceRange
  ) -> LLVMInstruction {
    precondition(
      operands.allSatisfy { (o) in
        let t = type(of: o)
        return t.isObject && (t.ast.base is BuiltinType)
      })

    return LLVMInstruction(applying: s, to: operands, site: anchor)
  }

}
