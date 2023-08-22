import Core

/// Returns the address of a global value.
public struct GlobalAddr: Instruction {

  /// The ID of the global in `container`.
  public let id: Module.GlobalID

  /// The module in which the global is defined.
  public let container: ModuleDecl.ID

  /// The type of the global value.
  public let valueType: AnyType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    id: Module.GlobalID,
    container: ModuleDecl.ID,
    valueType: AnyType,
    site: SourceRange
  ) {
    self.container = container
    self.id = id
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
    "global_addr @\(container).\(id)"
  }

}

extension Module {

  /// Creates an `global_addr` anchored at `site` that returns the address of `g` in `m`, which
  /// has type `t`.
  func makeGlobalAddr(
    of g: Module.GlobalID, in m: ModuleDecl.ID, typed t: AnyType, at anchor: SourceRange
  ) -> GlobalAddr {
    .init(id: g, container: m, valueType: t, site: anchor)
  }

}
