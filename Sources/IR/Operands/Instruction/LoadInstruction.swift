import Core

/// A load instruction.
public struct LoadInstruction: Instruction {

  /// The type of the object being loaded.
  public let objectType: LoweredType

  /// The location of the object is being loaded.
  public let source: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(objectType: LoweredType, from source: Operand, site: SourceRange) {
    self.objectType = objectType
    self.source = source
    self.site = site
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

extension Module {

  /// Creates a `load` anchored at `anchor` that loads the object at `source`.
  ///
  /// - Parameters:
  ///   - source: The location from which the object is loaded. Must have an address type.
  func makeLoad(
    _ source: Operand,
    anchoredAt anchor: SourceRange
  ) -> LoadInstruction {
    let t = type(of: source)
    precondition(t.isAddress)
    return LoadInstruction(objectType: .object(t.astType), from: source, site: anchor)
  }

}
