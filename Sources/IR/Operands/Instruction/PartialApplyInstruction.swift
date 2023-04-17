import Core

/// Creates a lambda wrapping a function pointer and an environment.
public struct PartialApplyInstruction: Instruction {

  /// The address of the underlying function.
  public let function: Constant

  /// The environment of the lambda.
  public let environment: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(function: Constant, environment: Operand, site: SourceRange) {
    self.function = function
    self.environment = environment
    self.site = site
  }

  public var types: [LoweredType] { [.object(function.type.ast)] }

  public var operands: [Operand] { [.constant(function), environment] }

}

extension Module {

  /// Creates a lambda instruction anchored at `anchor` that wraps `f` together with `environment`.
  ///
  /// - Parameters:
  ///   - f: The address of a function implementing the lambda.
  ///   - e: The environment of the lambda.
  func makePartialApply(
    wrapping f: Constant,
    with e: Operand,
    anchoredAt anchor: SourceRange
  ) -> PartialApplyInstruction {
    guard case .function = f else { preconditionFailure() }
    return .init(function: f, environment: e, site: anchor)
  }

}
