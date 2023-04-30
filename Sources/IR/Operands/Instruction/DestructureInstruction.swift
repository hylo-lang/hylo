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
    let parts = AbstractTypeLayout(of: type(of: whole).ast, definedIn: program)
      .properties.map({ LoweredType.object($0.type) })

    return DestructureInstruction(
      types: parts,
      whole: whole,
      site: anchor)
  }

}
