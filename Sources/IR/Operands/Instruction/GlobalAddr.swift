import FrontEnd

/// Returns the address of a global binding.
public struct GlobalAddr: Instruction {

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
    .address(valueType)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    preconditionFailure()
  }

}

extension GlobalAddr: CustomStringConvertible {

  public var description: String {
    "global_addr @\(binding)"
  }

}

extension Module {

  /// Creates an `global_addr` anchored at `site` that returns the address of `binding`.
  func makeGlobalAddr(of binding: BindingDecl.ID, at anchor: SourceRange) -> GlobalAddr {
    let t = program.canonical(program[binding].type, in: program[binding].scope)
    return .init(binding: binding, valueType: t, site: anchor)
  }

}
