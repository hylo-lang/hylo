/// Destructures a record.
public struct DestructureInst: Inst {

  /// The types of the destructured members.
  public var types: [LoweredType]

  /// The object being destructured.
  public var object: Operand

  public var range: SourceRange?

  init(_ object: Operand, as types: [LoweredType], range: SourceRange? = nil) {
    self.types = types
    self.object = object
    self.range = range
  }

  public var operands: [Operand] { [object] }

  public func check(in module: Module) -> Bool {
    // Instruction results have object types.
    for output in types {
      if output.isAddress { return false }
    }

    // Operand has an object type.
    if module.type(of: object).isAddress { return false }

    // Operand has a record layout.
    if !module.type(of: object).astType.hasRecordLayout { return false }

    return true
  }

}
