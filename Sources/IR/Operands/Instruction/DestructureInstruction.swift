import Core

/// Destructures a record.
public struct DestructureInstruction: Instruction {

  /// The types of the destructured members.
  public let types: [LoweredType]

  /// The object being destructured.
  public let whole: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(types: [LoweredType], whole: Operand, site: SourceRange) {
    self.types = types
    self.whole = whole
    self.site = site
  }

  public var operands: [Operand] { [whole] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Instruction results have object types.
    for output in types {
      if output.isAddress { return false }
    }

    // Operand has an object type.
    if module.type(of: whole).isAddress { return false }

    // Operand has a record layout.
    if !module.type(of: whole).astType.hasRecordLayout { return false }

    return true
  }

}

extension Module {

  /// Creates a `destructure` anchored at `anchor` that splits `whole` as individual parts.
  ///
  /// - Parameters:
  ///   - whole: The object to destructure. Must have an object type.
  func makeDestructure(
    _ whole: Operand,
    anchoredAt anchor: SourceRange
  ) -> DestructureInstruction {
    precondition(type(of: whole).isObject)
    let parts = AbstractTypeLayout(of: type(of: whole).astType, definedIn: program)
      .properties.map({ LoweredType.object($0.type) })

    return DestructureInstruction(
      types: parts,
      whole: whole,
      site: anchor)
  }

}
