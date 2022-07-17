/// Deinitializes an object.
public struct DeinitInst: Inst {

  /// The object being deinitialized.
  public var object: Operand

  public var range: SourceRange?

  init(_ object: Operand, range: SourceRange? = nil) {
    self.object = object
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object] }

  public var isTerminator: Bool { false }

  public func check(in module: Module) -> Bool {
    // Operand has an object type.
    if module.type(of: object).isAddress { return false }

    // Operand is register.
    if object.inst == nil { return false }

    return true
  }

}
