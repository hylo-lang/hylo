/// Stores `object` at the specified location.
public struct StoreInst: Inst {

  /// The object to store.
  public let object: Operand

  /// The location at which the object is stored.
  public let target: Operand

  public var type: IRType { .owned(.unit) }

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("store ")
    object.dump(into: &output, with: &printer)
    output.write(", ")
    target.dump(into: &output, with: &printer)
  }

}
