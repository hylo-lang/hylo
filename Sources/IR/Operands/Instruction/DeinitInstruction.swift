import Core

/// Deinitializes an object.
public struct DeinitInstruction: Instruction {

  /// The object being deinitialized.
  public let object: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(object: Operand, site: SourceRange) {
    self.object = object
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Operand has an object type.
    if module.type(of: object).isAddress { return false }

    // Operand is register.
    if object.instruction == nil { return false }

    return true
  }

}

extension Module {

  /// Creates a `deinit` anchored at `anchor` that deinitializes `object`.
  ///
  /// - Parameters:
  ///   - object: The object to deinitialize. Must be a local register with an object type.
  func makeDeinit(
    _ object: Operand,
    anchoredAt anchor: SourceRange
  ) -> DeinitInstruction {
    precondition(type(of: object).isObject)
    precondition(object.instruction != nil)

    return DeinitInstruction(object: object, site: anchor)
  }

}
