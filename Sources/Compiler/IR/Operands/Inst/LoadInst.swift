/// A load instruction.
public struct LoadInst: Inst {

  /// The type of the object being loaded.
  public let objectType: LoweredType

  /// The location of the object is being loaded, or the root location of the object from which a
  /// sub-object is being loaded.
  public let source: Operand

  /// A sequence of indices identifying a sub-location of `location`.
  public let path: [Int]

  public let range: SourceRange?

  init(
    _ objectType: LoweredType,
    from source: Operand,
    at path: [Int] = [],
    range: SourceRange? = nil
  ) {
    self.objectType = objectType
    self.source = source
    self.path = path
    self.range = range
  }

  public var types: [LoweredType] { [objectType] }

  public var operands: [Operand] { [source] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Instruction result has an object type.
    if objectType.isAddress { return false }

    // Source jas an address type.
    if !module.type(of: source).isAddress { return false }

    // Type of the instruction matches the type of the operand.
    if module.type(of: source).astType != objectType.astType { return false }

    return true
  }

}
