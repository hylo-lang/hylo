import Core

/// Accesses the value passed to a generic parameter.
public struct GenericParameter: Instruction {

  /// The parameter whose value is accessed.
  public let parameter: GenericParameterDecl.ID

  /// The type of the instruction's result.
  public let result: IR.`Type`?

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    parameter: GenericParameterDecl.ID,
    result: IR.`Type`,
    site: SourceRange
  ) {
    self.parameter = parameter
    self.result = result
    self.site = site
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension GenericParameter: CustomStringConvertible {

  public var description: String {
    "generic_parameter @\(parameter)"
  }

}

extension Module {

  /// Creates an `generic_parameter` anchored at `site` that returns the address of the generic
  /// argument passed to `p`.
  func makeGenericParameter(
    passedTo p: GenericParameterDecl.ID, at site: SourceRange
  ) -> GenericParameter {
    .init(parameter: p, result: .address(program[p].type), site: site)
  }

}
