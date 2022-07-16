/// A load instruction.
public struct LoadInst: Inst {

  /// The type of the object being loaded.
  public var objectType: LoweredType

  /// The address of the object to load.
  public var source: Operand

  public var range: SourceRange?

  init(_ objectType: LoweredType, from source: Operand, range: SourceRange? = nil) {
    self.objectType = objectType
    self.source = source
    self.range = range
  }

  public var types: [LoweredType] { [objectType] }

  public var operands: [Operand] { [source] }

  public func check(in module: Module) -> Bool {
    // Instruction has an object type.
    if objectType.isAddress { return false }

    // Source jas an address type.
    if !module.type(of: source).isAddress { return false }

    // Type of the instruction matches the type of the operand.
    if module.type(of: source).astType != objectType.astType { return false }

    return true
  }

}
