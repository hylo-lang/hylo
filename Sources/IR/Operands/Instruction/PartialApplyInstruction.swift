import Core

/// Creates a lambda wrapping a function pointer and an environment.
public struct PartialApplyInstruction: Instruction {

  /// The address of the underlying function.
  public private(set) var function: FunctionReference

  /// The environment of the lambda.
  public private(set) var environment: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(function: FunctionReference, environment: Operand, site: SourceRange) {
    self.function = function
    self.environment = environment
    self.site = site
  }

  public var types: [LoweredType] { [.object(function.type.ast)] }

  public var operands: [Operand] { [.constant(function), environment] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0:
      guard let f = new.constant as? FunctionReference else {
        preconditionFailure("invalid substitution")
      }
      function = f
    case 1:
      environment = new
    default:
      preconditionFailure()
    }
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
  ) -> PartialApplyInstruction {
    .init(function: f, environment: e, site: site)
  }

}
