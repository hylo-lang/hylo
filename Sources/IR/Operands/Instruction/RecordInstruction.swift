import Core

/// Creates a record of the specified type, consuming `operands` to initialize its members.
///
/// `type` must have a record layout. A type has a record layout if it is a product type, tuple
/// type, or bound generic type whose base has a record layout.
public struct RecordInstruction: Instruction {

  /// The type of the created record.
  public let objectType: LoweredType

  /// The operands consumed to initialize the record members.
  public let operands: [Operand]

  public let range: SourceRange?

  public var types: [LoweredType] { [objectType] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    // Instruction result has an object type.
    return !objectType.isAddress
  }

  init(objectType: LoweredType, operands: [Operand], range: SourceRange? = nil) {
    self.objectType = objectType
    self.operands = operands
    self.range = range
  }
}
