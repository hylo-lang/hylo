import Core

/// Deinitializes an object.
public struct DeinitInstruction: Instruction {

  /// The object being deinitialized.
  public let object: Operand

  public let range: SourceRange?

  init(_ object: Operand, range: SourceRange? = nil) {
    self.object = object
    self.range = range
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
