import Core

/// An instruction whose semantics is defined by LLVM.
public struct LLVMInstruction: Instruction {

  /// The built-in function representing this instruction.
  public let function: BuiltinFunction

  /// The operands of the instruction.
  public let operands: [Operand]

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(applying f: BuiltinFunction, to operands: [Operand], site: SourceRange) {
    self.function = f
    self.operands = operands
    self.site = site
  }

  public var types: [LoweredType] { [.object(function.type.output)] }

}

extension Module {

  /// Creates a llvm instruction anchored at `anchor` that applies `f` to `operands`.
  ///
  /// - Parameters:
  ///   - f: A built-in function.
  ///   - operands: A collection of built-in objects.
  func makeLLVM(
    applying f: BuiltinFunction,
    to operands: [Operand],
    anchoredAt anchor: SourceRange
  ) -> LLVMInstruction {
    precondition(
      operands.allSatisfy { (o) in
        let t = type(of: o)
        return t.isObject && (t.astType.base is BuiltinType)
      })

    return LLVMInstruction(applying: f, to: operands, site: anchor)
  }

}
