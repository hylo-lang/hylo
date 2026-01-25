import FrontEnd

/// Returns the place of a global binding.
public struct GlobalPlace: Instruction {

  /// The ID of the global global binding to access.
  public let binding: BindingDecl.ID

  /// The type of the binding value.
  public let valueType: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    binding: BindingDecl.ID,
    valueType: AnyType,
    site: SourceRange
  ) {
    self.binding = binding
    self.valueType = valueType
    self.site = site
  }

  public var result: IR.`Type`? {
    .place(valueType)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension GlobalPlace: CustomStringConvertible {

  public var description: String {
    "global_place @\(binding)"
  }

}

extension Module {

  /// Creates an `global_place` anchored at `site` that returns the place of `binding`.
  func makeGlobalPlace(of binding: BindingDecl.ID, at anchor: SourceRange) -> GlobalPlace {
    let t = program.canonical(program[binding].type, in: program[binding].scope)
    return .init(binding: binding, valueType: t, site: anchor)
  }

}
