import Core

/// Returns the address of a global value.
public struct GlobalAddrInstruction: Instruction {

  /// The ID of the global in `container`.
  public let id: Module.GlobalID

  /// The module in which the global is defined.
  public let container: ModuleDecl.ID

  /// The type of the global value.
  public let valueType: AnyType

  /// `true` iff the pointed value is assumed initialized.
  public let isValueInitialized: Bool

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    id: Module.GlobalID,
    container: ModuleDecl.ID,
    valueType: AnyType,
    isValueInitialized: Bool,
    site: SourceRange
  ) {
    self.container = container
    self.id = id
    self.valueType = valueType
    self.isValueInitialized = isValueInitialized
    self.site = site
  }

  public var types: [LoweredType] { [.address(valueType)] }

  public var operands: [Operand] { [] }

}

extension GlobalAddrInstruction: CustomStringConvertible {

  public var description: String {
    if isValueInitialized {
      return "global_addr @\(container).\(id)"
    } else {
      return "global_addr [uninitialized] @\(container).\(id)"
    }
  }

}

extension Module {

  /// Creates an `global_addr` anchored at `anchor` that returns the address of `g` in `m`, which
  /// has type `t`.
  func makeGlobalAddr(
    of g: Module.GlobalID,
    in m: ModuleDecl.ID,
    typed t: AnyType,
    assumedInitialized isValueInitialized: Bool = true,
    anchoredAt anchor: SourceRange
  ) -> GlobalAddrInstruction {
    .init(
      id: g, container: m, valueType: t,
      isValueInitialized: isValueInitialized,
      site: anchor)
  }

}
