import Core

/// Deinitializes an object.
public struct DeinitInstruction: Instruction {

  /// The object being deinitialized.
  public let object: Operand

  public let site: SourceRange

  init(_ object: Operand, site: SourceRange) {
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
