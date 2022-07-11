/// Creates a record of the specified type, consuming `operands` to initialize its members.
///
/// `type` must have a record layout. A type has a record layout if it is a product type, tuple
/// type, or bound generic type whose base has a record layout.
public struct RecordInst: Inst {

  /// The type of the created record.
  public let objectType: Type

  /// The operands consumed to initialize the record members.
  public let operands: [Operand]

  public var type: LoweredType { .object(objectType) }

}

