/// Creates a record of the specified type, consuming `operands` to initialize its members.
///
/// `type` must have a record layout. A type has a record layout if it is a product type, tuple
/// type, or bound generic type whose base has a record layout.
public struct RecordInst: Inst {

  /// The type of the created record.
  public let type: IRType

  /// The operands consumed to initialize the record members.
  public let operands: [Operand]

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("record \(type.valType)")
    for operand in operands {
      output.write(", ")
      operand.dump(into: &output, with: &printer)
    }
  }

}

