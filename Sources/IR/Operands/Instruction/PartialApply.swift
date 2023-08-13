import Core

/// Creates a lambda wrapping a function pointer and an environment.
public struct PartialApply: Instruction {

  /// The operands of the instruction.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(callee: FunctionReference, environment: Operand, site: SourceRange) {
    self.operands = [.constant(callee), environment]
    self.site = site
  }

  /// The partially applied function.
  public var callee: FunctionReference {
    operands[0].constant as! FunctionReference
  }

  /// The environment of the lambda.
  public var environment: Operand {
    operands[1]
  }

  public var result: IR.`Type`? {
    .object(callee.type.ast)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition((i != 0) || (new.constant is FunctionReference), "invalid substitution")
    operands[i] = new
  }

}

extension Module {

  /// Creates a lambda instruction anchored at `site` that wraps `f` together with `environment`.
  ///
  /// - Parameters:
  ///   - f: The address of a function implementing the lambda.
  ///   - e: The environment of the lambda.
  func makePartialApply(
    wrapping f: FunctionReference, with e: Operand, at site: SourceRange
  ) -> PartialApply {
    .init(callee: f, environment: e, site: site)
  }

}
