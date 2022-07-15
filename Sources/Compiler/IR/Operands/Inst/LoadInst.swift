/// A load instruction.
public struct LoadInst: Inst {

  public var type: LoweredType

  /// The address of the object to load.
  public var source: Operand

  public var range: SourceRange?

  public var operands: [Operand] { [source] }

  public func check(in module: Module) -> Bool {
    // Instruction has an object type.
    if type.isAddress { return false }

    // Source jas an address type.
    if !module.type(of: source).isAddress { return false }

    // Type of the instruction matches the type of the operand.
    if module.type(of: source).astType != type.astType { return false }

    return true
  }

}
